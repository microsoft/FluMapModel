library(logging)
basicConfig()

#' loadModelFileById function for getting modeled data
#'
#' This function will load a model from the model_store_dir by Id
#' 
#' Future enhancments will add versioning
#'
#' @param filename = At moment we expect full filename in format ID.extension. 
#'  This is so in future we can more easily support different model save formats
#' @param model_store_dir = directory where models are stored
#'
#' @return model
#'
#' @export
#' @examples
#'
loadModelFileById <- function (filename, model_store_dir = Sys.getenv('MODEL_BIN_DIR', '/home/rstudio/seattle_flu')) {
  # expand path to the full path
  filename <- file.path(model_store_dir, filename)
  # load the data
  db <- read.csv(paste(filename, type,'csv',sep='.'))
  return(db)
}

#' returnModel function for getting modeled data
#'
#' This function loads a cached model object containing two datasets
#'   (1) observed incidence of flu in ILI
#'   (2) modeled incidence after statistical smoothing
#'
#' @param queryIn JSON query to dbViewR that defines model
#' @param type = "smooth" (default), "latent_field", "effects", "inla"
#' @param format = "csv" (default: pre-defined data-only output), "json" (pre-defined data-only output), or "model" (incidenceMapR model object)
#' @param version = "latest" (default) or ISO date and time created (UTC, like "2019-04-19 22:49:19Z")
#' @param cloudDir = directory where models are stored
#'
#' @return model in requested format
#'
#' @import jsonlite
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
                            version = 'latest',
                            cloudDir = Sys.getenv('MODEL_BIN_DIR', '/home/rstudio/seattle_flu')){

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

  modelID <- getModelIdFromQuery(queryList)
  logdebug("ModelID: ", modelID)

  modelDBfile<-paste(cloudDir,'modelDB.tsv',sep='/')

  modelDB <- read.table(modelDBfile,header=TRUE,sep='\t',stringsAsFactors = FALSE)
  modelDB$created <- as.POSIXct(modelDB$created,tz='UTC')


  queryIdx <- gsub('\\\\','',modelDB$queryJSON) %in% gsub('\"','',as.character(queryIn))
  typeIdx <- modelDB$type == type

  matchIdx <- which(queryIdx & typeIdx)

  if(version == 'latest'){
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

  if (format %in% c('csv','json')){
    db <- loadModelFile(modelID, cloudDir)

    dataOut<-list(query = queryList, type = type)

    if(format == 'csv'){
      
      return(dataOut)
      
    } else {
      
      # flat data format
      dataOut$data <- db
      
      # reformat for hierarchical JSON
      # dataOut$data <-db %>% dplyr::group_by_at(queryList$GROUP_BY$COLUMN) %>% tidyr::nest(names(db))
  
      return(jsonlite::toJSON(dataOut,pretty = TRUE))
    }

  } else if(format == 'inla'){

    model <- readRDS(paste(filename,'inla.RDS',sep='.'))

    return(model)
  }
}
