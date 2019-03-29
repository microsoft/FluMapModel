#' selectFromDB: function for fetching data from research DB
#' (Currently pulls from simulated data at https://github.com/seattleflu/simulated-data)
#'
#' STANDARD DB QUERIES WILL ALL LIKELY MIGRATE TO THE HUTCH RESARCH DB BEFORE PRODUCTION.
#'
#' @param queryIn  list or json specifying query  (See example)
#' @return observedData table that has been prepared for defineModels.R
#'
#' @import jsonlite
#' @import dplyr
#' @importFrom RCurl getURL
#' @importFrom magrittr %>%
#' @importFrom lazyeval interp
#'
#' @export
#' @examples
#' return h1n1pdm summary by time and location
#' queryJSON <- jsonlite::toJSON(
#'   list(
#'       SELECT   =list(COLUMN=c('pathogen','timeInfected','PUMA5CE','GEOID')),
#'       MUTATE   =list(COLUMN=c('timeInfected'), AS=c('timeBin')),
#'       GROUP_BY =list(COLUMN=c('timeBin','timeRow','PUMA5CE','GEOID')),
#'       SUMMARIZE=list(COLUMN='pathogen', IN= c('h1n1pdm'))
#'       )
#'    )
#'    db <- selectFromDB( queryJSON )
#'
selectFromDB <- function( queryIn = jsonlite::toJSON(
                            list(
                              SELECT   =list(COLUMN=c('pathogen','timeInfected','PUMA5CE','GEOID')),
                              MUTATE   =list(COLUMN=c('timeInfected'), AS=c('timeBin')),
                              GROUP_BY =list(COLUMN=c('timeBin','PUMA5CE','GEOID')),
                              SUMMARIZE=list(COLUMN='pathogen', IN= c('h1n1pdm'))
                            )
                          ) ){

  if(class(queryIn) == "json"){
    queryList <- jsonlite::fromJSON(queryIn)
  } else if(class(queryIn) == "list"){
    queryList<-queryIn
  }

  # connect to database
    rawData <- RCurl::getURL("https://raw.githubusercontent.com/seattleflu/simulated-data/master/simulatedSubjectDatabase.csv")
    db <- read.table(text = rawData, header=TRUE, sep=",", stringsAsFactors = FALSE)

  # run query
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

    if('MUTATE' %in% names(queryList)){
      for( newCol in queryList$MUTATE$COLUMN)
        if(newCol == 'timeInfected'){
          db <- db %>% dplyr::mutate( timeBin = floor((timeInfected)*52)/52)
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

  # type harmonization
    if ("GEOID" %in% names(db)){
      db$GEOID <- as.character(db$GEOID)
    }
    if ("PUMA5CE" %in% names(db)){
      db$PUMA5CE <- as.character(db$PUMA5CE)
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

  shp$GEOID<-as.character(shp$GEOID)
  shp$CRA_NAM<-as.character(shp$CRA_NAM)
  shp$NEIGHBO<-as.character(shp$NEIGHBO)
  shp$PUMA5CE<-as.character(shp$PUMA5CE)
  
  return(shp)


  # this will eventually pull from curated data like: https://github.com/seattleflu/seattle-geojson
}
