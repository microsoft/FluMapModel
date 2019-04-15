#' returnModel function for getting modeled data
#'
#' This function loads a cached model object containing two datasets
#'   (1) observed incidence of flu in ILI
#'   (2) modeled incidence after statistical smoothing
#'
#' @param queryIn JSON query to dbViewR that defines model
#' @param type = "smooth" (default), "latent", "effects"
#' @param format = "csv" (default: pre-defined data-only output) or "model" (incidenceMapR model object)
#' @param version = "mostRecent" (default) or date and time created
#' @param cloudDir = directory where models are stored
#'
#' @return dataJSON json with data
#'
#' @import dbViewR
#' @import jsonlite
#' @import magrittr
#' @importFrom RCurl getURL
#' @importFrom dplyr group_by_at
#' @importFrom tidyr nest
#' @export
#' @examples
#'
returnModel <- function(queryIn = jsonlite::toJSON(
                              list(
                                SELECT   =list(COLUMN=c('sampling_location','GEOID')),
                                WHERE   =list(COLUMN='sampling_location', IN = c('kiosk')),
                                GROUP_BY =list(COLUMN=c('sampling_location','GEOID')),
                                SUMMARIZE=list(COLUMN='sampling_location', IN= c('kiosk'))
                                  )),
                            type = 'smooth',
                            format = 'csv',
                            version = 'most_recent',
                            cloudDir = 'C:/Users/mfamulare/Dropbox (IDM)/SeattleFlu-incidenceMapR/models'){

  # https://www.dropbox.com/sh/5loj4x6j4tar17i/AABy5kP70IlYtSwrePg4m44Ca?dl=0

  # need to solve paths problem for intalled packages!
  # ideally this would point to a web-based repository of models so that anyone can get to it.
  # THIS DOES NOT WORK because of authentication.  Need to explore rdrop2 package!

  # NEED TO Pull multiple formats of data once saving latent fields is implemented in incidenceMapR
  #  ACTUALLY, current plan is to have csv obey format for each model type

  if(class(queryIn)== 'list'){
    queryList <- queryIn
    queryIn <- jsonlite::toJSON(queryIn)
  } else if(class(queryIn)=='json'){
    queryList <- jsonlite::fromJSON(queryIn)
  }

  modelDBfile<-paste(cloudDir,'modelDB.tsv',sep='/')

  modelDB <- read.table(modelDBfile,header=TRUE,sep='\t',stringsAsFactors = FALSE)
  modelDB$created <- as.Date(modelDB$created)


  queryIdx <- gsub('\\\\','',modelDB$queryJSON) %in% gsub('\"','',as.character(queryIn))
  typeIdx <- modelDB$type == type

  matchIdx <- which(queryIdx & typeIdx)

  if(version == 'most_recent'){
    createdIdx <- which.max(modelDB$created[matchIdx])
  } else {
    version = as.Date(version)
    createdIdx <- which.min(abs(version-modelDB$created))
  }
  matchIdx<-matchIdx[createdIdx]


  if(!any(matchIdx)){
    return('unknown model')
  } else {
    filename<-paste(cloudDir,modelDB$filename[matchIdx],sep='/')
  }

  if (format == 'csv'){
    db <- read.csv(paste(filename,'csv',sep='.'))

    dataOut<-list(query = queryList, type = type)

    # flat data format
    dataOut$data <- db

    # reformat for hierarchical JSON
    # dataOut$data <-db %>% dplyr::group_by_at(queryList$GROUP_BY$COLUMN) %>% tidyr::nest(names(db))

    return(jsonlite::toJSON(dataOut,pretty = TRUE))

  } else if(format == 'model'){

    model <- readRDS(paste(filename,'RDS',sep='.'))

    return(model)
  }
}
