#' selectFromDB: function for fetching data from research DB
#' (Currently pulls from simulated data at https://github.com/seattleflu/simulated-data)
#'
#' STANDARD DB QUERIES WILL ALL LIKELY MIGRATE TO THE HUTCH RESARCH DB BEFORE PRODUCTION.
#'
#' @param queryIn  list or json specifying query  (See example)
#' @params source source database, one of: 'simulated_data' (default) or 'production'
#' @return observedData table that has been prepared for defineModels.R
#'
#' @import jsonlite
#' @import dplyr
#' @import lubridate
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
                          ), source = 'simulated_data' ){

  if(class(queryIn) == "json"){
    queryList <- jsonlite::fromJSON(queryIn)
  } else if(class(queryIn) == "list"){
    queryList<-queryIn
  }

  # connect to database
  if(source = 'simulated_data'){

     rawData <- RCurl::getURL("https://raw.githubusercontent.com/seattleflu/simulated-data/master/simulated_subject_database.csv")
     db <- read.table(text = rawData, header=TRUE, sep=",", stringsAsFactors = FALSE)

  } else if(source = 'production'){

    # link to credentials file and input credential below
     rawData <- dbConnect(RPostgres::Postgres(), host="production.db.seattleflu.org", dbname = 'production', user='', password='')

     db <- dbGetQuery(db, "select * from shipping.incidence_model_observation_v1;")
     dbDisconnect(rawData)

    # clean up string formatting and harmonize factors vs character
    
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
      db$epi_week <- paste(lubridate::epiyear(db$encountered_date),'_W',sprintf('%02d',lubridate::epiweek(db$encountered_date)),sep='')
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
#' (Currently pulls from https://github.com/seattleflu/simulated-data/tree/master/kingCountySpatialData)
#'
#' @return sf object with king county shapefile data
#'
#' @importFrom sf st_read
#'
#' @export
#' @examples
#'    shp <- masterSpatialDB()
#'
masterSpatialDB <- function(){
  # should take option of shape level!

  # connect to database
  download.file(url = "https://github.com/seattleflu/simulated-data/raw/master/kingCountySpatialData/2016_CensusTracts_KingCountyWa.zip",
                destfile = "2016_CensusTracts_KingCountyWa.zip")
  unzip(zipfile = "2016_CensusTracts_KingCountyWa.zip")
  shp <- sf::st_read("2016_CensusTracts_KingCountyWa")

  levels(shp$NEIGHBO)<-c(levels(shp$NEIGHBO),'NA')
  shp$NEIGHBO[is.na(shp$NEIGHBO)]<-'NA'

  levels(shp$CRA_NAM)<-c(levels(shp$CRA_NAM),'NA')
  shp$CRA_NAM[is.na(shp$CRA_NAM)]<-'NA'

  unlink('2016_CensusTracts_KingCountyWa', recursive = TRUE)
  unlink('2016_CensusTracts_KingCountyWa.zip')

  names(shp)[names(shp) %in% c('CRA_NAM','NEIGHBO')]<-c('CRA_NAME','NEIGHBORHOOD_DISTRICT_NAME')

  for( COLUMN in names(shp)[names(shp) %in% c('GEOID','CRA_NAME','PUMA5CE','NEIGHBORHOOD_DISTRICT_NAME')]){
    shp[[COLUMN]] <- as.character(shp[[COLUMN]])
  }

  return(shp)


  # this will eventually pull from curated data like: https://github.com/seattleflu/seattle-geojson
}
