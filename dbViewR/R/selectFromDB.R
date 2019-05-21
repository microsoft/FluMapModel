#' selectFromDB: function for fetching data from research DB
#' (Currently pulls from simulated data at https://github.com/seattleflu/simulated-data)
#'
#' STANDARD DB QUERIES WILL ALL LIKELY MIGRATE TO THE HUTCH RESARCH DB BEFORE PRODUCTION.
#'
#' @param queryList  list or json specifying query  (See example)
#' @param source source database, one of: 'simulated_data' (default) or 'production'
#' @param credentials_path path to your pg_service and pgpass file for production database
#' @param na.rm = FALSE (default) Drop rows with NA from dataset as incidenceMapR will ignore them anyway
#' @param shp = dbViewR::masterSpatialDB(shape_level = 'census_tract', source = 'king_county_geojson') (Needed hack until higher-level shape labels are in database)
#' @return dbViewR list with query and observedData table that has been prepared for defineModels.R
#'
#' @import jsonlite
#' @import dplyr
#' @import lubridate
#' @import DBI
#' @import RPostgres
#' @importFrom RCurl getURL
#' @importFrom magrittr %>%
#' @importFrom lazyeval interp
#' @import tidyr 
#'
#' @export
#' @examples
#' return h1n1pdm summary by time and location
#' queryJSON <- jsonlite::toJSON(
#'   list(
#'       SELECT   =list(COLUMN=c('pathogen','encountered_week','residence_puma','residence_census_tract')),
#'       GROUP_BY =list(COLUMN=c('encountered_week','residence_puma','residence_census_tract')),
#'       SUMMARIZE=list(COLUMN='pathogen', IN= c('h1n1pdm'))
#'       )
#'    )
#'    db <- selectFromDB( queryJSON )
#'
selectFromDB <- function( queryIn = jsonlite::toJSON(
                            list(
                              SELECT   =list(COLUMN=c('pathogen','encountered_date','residence_puma','residence_census_tract')),
                              GROUP_BY =list(COLUMN=c('encountered_week','residence_puma','residence_census_tract')),
                              SUMMARIZE=list(COLUMN='pathogen', IN= c('h1n1pdm'))
                            )
                          ), source = 'simulated_data', 
                          credentials_path = '/home/rstudio/seattle_flu',
                          na.rm = FALSE
                          ){

  if(class(queryIn) == "json"){
    queryList <- jsonlite::fromJSON(queryIn)
  } else if(class(queryIn) == "list"){
    queryList<-queryIn
  }
  

  # connect to database
  if(source == 'simulated_data'){

    rawData <- RCurl::getURL("https://raw.githubusercontent.com/seattleflu/simulated-data/master/simulated_subject_database.csv")
    db <- read.table(text = rawData, header=TRUE, sep=",", stringsAsFactors = FALSE)

  } else if(source == 'production'){

    # Define standard Pg environment variables for our connection files.
    #
    # These are set here instead of passed in via the Dockerfile or
    # docker-compose.rstudio.yaml file, because this typically runs as an
    # rstudio-managed user with its own shell environment separate from the
    # base Docker user.
    Sys.setenv(PGSERVICEFILE = file.path(credentials_path, ".pg_service.conf"),
               PGPASSFILE    = file.path(credentials_path, ".pgpass"))

    # Connect to database using service definition and credentials in files
    # defined by the environment variables above.
    rawData <- DBI::dbConnect(RPostgres::Postgres(), service = "seattleflu-production")

    db <- DBI::dbGetQuery(rawData, "select * from shipping.incidence_model_observation_v1;")
    
    # fake pathogen field until db is ready
    # if (!('pathogen' %in% names(db))){
    #   db$pathogen <- 'unknown'
    # }
    
    # db <- DBI::dbGetQuery(rawData, paste('select distinct * from shipping.incidence_model_observation_v1 encounter',
    #                                      'left join shipping.presence_absence_result_v1 taq',
    #                                      'on encounter.sample = taq.sample',
    #                                      ';'),sep=' ') 
    
    # this logic should be substantially rethought, as I'm mixing sql and dplyr in confusing ways, but it will have to do for now!
    
    # get all samples and nest
    db3 <- DBI::dbGetQuery(rawData, paste('select distinct * from shipping.presence_absence_result_v1',
                                          ';'),sep=' ') 

    names(db3)[names(db3)=='target'] <- 'pathogen'
    
    db3 <- db3 %>% group_by(sample) %>%
      mutate(number_pathogens_found = sum(present), number_pathogens_tested = n()) %>% 
      filter(present == TRUE | number_pathogens_found==0) %>%
      group_by(sample,number_pathogens_found,number_pathogens_tested) %>% 
      tidyr::nest() 

      for (k in which(db3$number_pathogens_found==0)){
        db3$data[[k]] <- tibble(pathogen='undetected',present=TRUE)
      }
    
    names(db3)[names(db3) == 'data'] <- 'pathogens_found'    
    
    
    # join with encounter list, using nice formatting
    db <- db %>% left_join(db3)
    idx<-is.na(db$number_pathogens_tested)
    db$number_pathogens_tested[idx] <- 0
    for (k in which(idx)){
      db$pathogens_found[[k]] <- tibble(pathogen='not_yet_tested',present=TRUE)
    }

    db <- db %>% tidyr::unnest()  # nice flat file like simulated data, but with repeated encounters for multiple positives
    
    DBI::dbDisconnect(rawData)



  } else {
     print('unknown source database!')
  }

  
  # run query
  # this logic will probably move to sql queries in the database instead of dplyr after....
    if(queryList$SELECT !="*"){
      
      

      #(Needed hack until higher-level shape labels are in database)
        if ( any( grepl('residence',queryList$SELECT$COLUMN) | grepl('work',queryList$SELECT$COLUMN) ) ){
          if (! any( grepl('cra_name',queryList$SELECT$COLUMN) | grepl('neighbo',queryList$SELECT$COLUMN) ) ){
            shp = dbViewR::masterSpatialDB(shape_level = 'census_tract', source = 'wa_geojson')
          } else {
            shp = dbViewR::masterSpatialDB(shape_level = 'census_tract', source = 'king_county_geojson')
          }
        
          # append higher-level spatial labels
          # this feature will eventually be in the database, but it's needed for now to index to pumas, cra_name, etc
          nestedVariables <- c('cra_name','neighborhood_district_name','puma','city')
          
          for( COLUMN in nestedVariables){
            COLNAME <- paste0('residence_',COLUMN)
            if( ('residence_census_tract' %in% names(db))  & !(COLNAME %in% names(db)) & (COLNAME %in% names(shp))){
              db[[COLNAME]] <- as.character(shp[[COLNAME]][match(db$residence_census_tract,shp$residence_census_tract)])
            }
            COLNAME <- paste0('work_',COLUMN)
            if( ('work_census_tract' %in% names(db)) & !(COLNAME %in% names(db)) & (COLNAME %in% names(shp))){
              db[[COLNAME]] <- as.character(shp[[COLNAME]][match(db$work_census_tract,shp$work_census_tract)])
            }
          }
        }
      
      ## real flow starts here
        
      db <- db %>% dplyr::select(dplyr::one_of(queryList$SELECT$COLUMN))

      for(FILTER in which(grepl('WHERE',names(queryList)))){

        if( any(grepl('IN',names(queryList[[FILTER]])))){

          if(any(queryList[[FILTER]]$IN != 'all')){
            filter_criteria <- lazyeval::interp(~y %in% x, .values=list(y = as.name(queryList[[FILTER]]$COLUMN), x = queryList[[FILTER]]$IN))
            db <- db %>% dplyr::filter_(filter_criteria)
          }
        
        } else if( any(grepl('BETWEEN',names(queryList[[FILTER]])))){

          filter_criteria_low <- lazyeval::interp(~y >= x, .values=list(y = as.name(queryList[[FILTER]]$COLUMN), x = queryList[[FILTER]]$BETWEEN[1]))
          filter_criteria_high <- lazyeval::interp(~y <= x, .values=list(y = as.name(queryList[[FILTER]]$COLUMN), x = queryList[[FILTER]]$BETWEEN[2]))

          db <- db %>% dplyr::filter_(filter_criteria_low)  %>% dplyr::filter_(filter_criteria_high)

        }
      }
    
      if('GROUP_BY' %in% names(queryList)){
        db<- db %>% dplyr::group_by_(.dots=queryList$GROUP_BY$COLUMN)
      }
      
      if('SUMMARIZE' %in% names(queryList)){
  
        if (queryList$SUMMARIZE$IN != 'all'){
          summary_criteria <- lazyeval::interp(~sum(y %in% x), .values=list(y = as.name(queryList$SUMMARIZE$COLUMN), x = queryList$SUMMARIZE$IN))
        } else {
          summary_criteria <- lazyeval::interp(~n())  # must always output n and positive for downstream interpretation!
        }
          
        db <- db %>% dplyr::summarise_(n = lazyeval::interp(~n()), positive = summary_criteria) 
      }
  
      # "pathogen" column is required for incidenceMapR model definitions
        if(!('pathogen' %in% queryList$GROUP_BY$COLUMN)){
          if( 'pathogen' %in% queryList$WHERE$COLUMN){
            db$pathogen <- paste(queryList$WHERE$IN['pathogen' %in% queryList$WHERE$COLUMN],collapse='-')
          } else {
            db$pathogen<-'all' 
          }
        }
    
    }
  
  
  # type harmonization
    for( COLUMN in names(db)[names(db) %in% c('residence_census_tract','residence_cra_name','residence_puma','residence_neighborhood_district_name','residence_city',
                                              'work_census_tract','work_cra_name','work_puma','work_neighborhood_district_name','work_city')]){
      db[[COLUMN]] <- as.character(db[[COLUMN]])
    }

  # drop rows with NA since incidenceMapR (INLA) will ignore them anyway
    if(na.rm){
      
      # fixes #Error in charToDate(x): character string is not in a standard unambiguous format
      dateIdx<- (sapply(db,class)=="Date")
      db[,dateIdx] <- as.character(db[,dateIdx])
      
      db <- db %>% replace(.=='NA', NA) %>% tidyr::drop_na()
      
      # restore type
      if (any(dateIdx)){
        db[,dateIdx] <- as.Date(db[,dateIdx])
      }
    
    }
    
  summarizedData <- list(observedData = db,queryList = c(queryList))

  return(summarizedData)
}



