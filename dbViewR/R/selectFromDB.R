#' selectFromDB: function for fetching data from research DB
#' (Currently pulls from simulated data at https://github.com/seattleflu/simulated-data)
#'
#' STANDARD DB QUERIES WILL ALL LIKELY MIGRATE TO THE HUTCH RESARCH DB BEFORE PRODUCTION.
#'
#' @param queryIn  list or json specifying query  (See example)
#' @param source source database, one of: 'simulated_data' (default) or 'production'
#' @param credentials_path path to your pg_service and pgpass file for production database
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
#'
#' @export
#' @examples
#' return h1n1pdm summary by time and location
#' queryJSON <- jsonlite::toJSON(
#'   list(
#'       SELECT   =list(COLUMN=c('pathogen','encountered_date','PUMA5CE','GEOID')),
#'       MUTATE   =list(COLUMN=c('encountered_date'), AS=c('epi_week')),
#'       GROUP_BY =list(COLUMN=c('epi_week','PUMA5CE','GEOID')),
#'       SUMMARIZE=list(COLUMN='pathogen', IN= c('h1n1pdm'))
#'       )
#'    )
#'    db <- selectFromDB( queryJSON )
#'
selectFromDB <- function( queryIn = jsonlite::toJSON(
                            list(
                              SELECT   =list(COLUMN=c('pathogen','encountered_date','PUMA5CE','GEOID')),
                              MUTATE   =list(COLUMN=c('encountered_date'), AS='epi_week'),
                              GROUP_BY =list(COLUMN=c('epi_week','PUMA5CE','GEOID')),
                              SUMMARIZE=list(COLUMN='pathogen', IN= c('h1n1pdm'))
                            )
                          ), source = 'simulated_data', credentials_path = '.' ){

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

    #service you want to access
    service_string = "seattleflu-production"
    
    #get host and dbname from pg_service file, then get user and password from the pgpass file 
    pg_service_file<-read.table(file.path(credentials_path, "pg_service.conf"), header=FALSE) #read in file
    service_index <-which(str_detect(pg_service_file$V1, service_string)) #get index for specified service
    host_string <- strsplit(as.character(pg_service_file$V1[service_index+1]), "=")[[1]][2] #host string is next item after index for service
    dbname_string <- strsplit(as.character(pg_service_file$V1[service_index+2]), "=")[[1]][2] #dbname string is next item after index for service

    #read in pgpass file
    pgpass_file <- read.table(file.path(credentials_path, "pgpass.conf"), header=FALSE)
    pgpass_file <- strsplit(levels(pgpass_file$V1), ":") #convert from factor to list
    
    #get index for which row has the correct host
    host_index<-which(grepl(host_string, pgpass_file))
    
    # link to credentials file and input credential below
    rawData <- dbConnect(RPostgres::Postgres(), 
                    host=host_string, 
                    dbname = dbname_string, 
                    user=pgpass_file[[host_index]][4], 
                    password=pgpass_file[[host_index]][5])
    
    
    
    db <- dbGetQuery(rawData, "select * from shipping.incidence_model_observation_v1;") # "shipping.incidence_model_observation_v1" seems like something that should be an option
    dbDisconnect(rawData)

  } else {
     print('unknown source database!')
  }

  # run query
  # this logic will probably move to sql queries in the database instead of dplyr after....
    if(queryList$SELECT !="*"){

      db <- db %>% dplyr::select(dplyr::one_of(queryList$SELECT$COLUMN))

      for(FILTER in which(grepl('WHERE',names(queryList)))){

        if( any(grepl('IN',names(queryList[[FILTER]])))){

          filter_criteria <- lazyeval::interp(~y %in% x, .values=list(y = as.name(queryList[[FILTER]]$COLUMN), x = queryList[[FILTER]]$IN))
          db <- db %>% dplyr::filter_(filter_criteria)

        } else if( any(grepl('BETWEEN',names(queryList[[FILTER]])))){

          filter_criteria_low <- lazyeval::interp(~y >= x, .values=list(y = as.name(queryList[[FILTER]]$COLUMN), x = queryList[[FILTER]]$BETWEEN[1]))
          filter_criteria_high <- lazyeval::interp(~y <= x, .values=list(y = as.name(queryList[[FILTER]]$COLUMN), x = queryList[[FILTER]]$BETWEEN[2]))

          db <- db %>% dplyr::filter_(filter_criteria_low)  %>% dplyr::filter_(filter_criteria_high)

        }
      }
    }
    
    # time bin mutations
    if('encountered_date' %in% names(db)){
      db$encountered_date <- as.Date(db$encountered_date)
      db$epi_week <- paste(lubridate::epiyear(db$encountered_date),'-W',sprintf('%02d',lubridate::epiweek(db$encountered_date)),sep='')
      db$iso_week <- format(db$encountered_date, "%G-W%V")
    }
    
    # age bin mutations
    if('age' %in% names(db)){
      db$age_bin <- floor(pmin(db$age,90))
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

    
  # type harmonization
    for( COLUMN in names(db)[names(db) %in% c('GEOID','CRA_NAME','PUMA5CE','NEIGHBORHOOD_DISTRICT_NAME')]){
      db[[COLUMN]] <- as.character(db[[COLUMN]])
    }
    
  summarizedData <- list(observedData = db,queryList = c(queryList))

  return(summarizedData)
}


#' masterSpatialDB: function for fetching spatial data from master source
#'
#' @param shape_level one of "census_tract" (default),"cra_name","neighborhood","city"
#' @param source source database, one of "seattle_geojson" (default), "simulated_data"
#' @param rm_files indicator to remove local files (TRUE == default)
#' @return sf object with shapefile data
#'
#' @import geojsonio
#' @import rgdal
#' @import sf
#'
#' @export
#' @examples
#'    shp <- masterSpatialDB(shape_level = 'census_tract', source = 'simulated_data', rm_files = TRUE)
#'
masterSpatialDB <- function(shape_level = 'census_tract', source = 'simulated_data', rm_files = TRUE){

  if (source == 'seattle_geojson'){
    # connect to database and get the data at correct shape level
    
    sourceURL <- paste('https://raw.githubusercontent.com/seattleflu/seattle-geojson/master/seattle_geojsons/')
    # pumas are missing from repo
    
    validShapeLevels <- c("census_tract","cra_name","neighborhood","city")
    validShapeFilenames<- c("2016_seattle_censusTracts.geojson","2016_seattle_cra.geojson","2016_seattle_neighborhoods.geojson","2016_seattle_city.geojson")
    
    filename<-validShapeFilenames[validShapeLevels %in% shape_level]
    
    sourceURL <- paste('https://raw.githubusercontent.com/seattleflu/seattle-geojson/master/seattle_geojsons/',filename,sep='')
    download.file(url = sourceURL,  destfile = filename)

    shp <- sf::st_as_sf(geojsonio::geojson_read(filename, what = "sp"))

  } else if(source == 'simulated_data' & shape_level == "census_tract"){
    
    filename <- "2016_CensusTracts_KingCountyWa"
    download.file(url = "https://github.com/seattleflu/simulated-data/raw/master/kingCountySpatialData/2016_CensusTracts_KingCountyWa.zip",
                  destfile = paste0(filename,'.zip'))
    unzip(zipfile = paste0(filename,'.zip'))
    shp <- sf::st_read(filename)
    
  } else {
    return('unknown source and shape_level combination!')
  }
  
  if (rm_files){
    #unlink from the database
    unlink(filename, recursive = TRUE)
    unlink(paste0(filename,'.zip'), recursive = TRUE)
  }
  
  levels(shp$NEIGHBO)<-c(levels(shp$NEIGHBO),'NA')
  shp$NEIGHBO[is.na(shp$NEIGHBO)]<-'NA'
  
  levels(shp$CRA_NAM)<-c(levels(shp$CRA_NAM),'NA')
  shp$CRA_NAM[is.na(shp$CRA_NAM)]<-'NA'
  
  shp$GEOID<-as.character(shp$GEOID)
  shp$CRA_NAM<-as.character(shp$CRA_NAM)
  shp$NEIGHBO<-as.character(shp$NEIGHBO)
  shp$PUMA5CE<-as.character(shp$PUMA5CE)
  
  return(shp)
  
}
