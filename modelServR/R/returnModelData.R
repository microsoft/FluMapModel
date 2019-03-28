#' returnModelData function for getting modeled data
#'
#' This function loads a cached model object containing two datasets
#'   (1) observed incidence of flu in ILI
#'   (2) modeled incidence after statistical smoothing
#'
#' @param queryIn JSON string with description of output data requested from model
#' @return dataJSON json with data
#' @import dbViewR
#' @import jsonlite
#' @import magrittr
#' @importFrom RCurl getURL
#' @importFrom dplyr group_by_at
#' @importFrom tidyr nest
#' @export
#' @examples
#'
returnModelData <- function(queryIn = jsonlite::toJSON(
                              list(
                                SELECT   =list(COLUMN=c('samplingLocation','GEOID')),
                                WHERE   =list(COLUMN='samplingLocation', IN = c('kiosk')),
                                GROUP_BY =list(COLUMN=c('samplingLocation','GEOID')),
                                SUMMARIZE=list(COLUMN='samplingLocation', IN= c('kiosk'))
                                  )),
                            type = 'csv',
                            cloudDir = 'C:/Users/mfamulare/Dropbox (IDM)/SeattleFlu-incidenceMapR/models/'){
  
  # https://www.dropbox.com/sh/5loj4x6j4tar17i/AABy5kP70IlYtSwrePg4m44Ca?dl=0
  
  # need to solve paths problem for intalled packages!
  # ideally this would point to a web-based repository of models so that anyone can get to it.
  # THIS DOES NOT WORK because of authentication.  Need to explore rdrop2 package!
  
  # NEED TO Pull multiple types of data once saving latent fields is implemented in incidenceMapR
  
  if(class(queryIn)== 'list'){
    queryIn <- jsonlite::toJSON(queryIn)
  }
  
  modelDBfile<-paste(cloudDir,'modelDB.tsv',sep='')
  
  modelDB <- read.table(modelDBfile,header=TRUE,sep='\t',stringsAsFactors = FALSE)

 
  idx = gsub('\\\\','',modelDB$queryJSON) %in% gsub('\"','',as.character(queryIn))
  
  # needs to handle versions
  
  if(!any(idx)){ 
    return('unknown model') 
  } else {
    filename<-paste(cloudDir,modelDB$filename[idx],sep='')
  }
  
  if (type == 'csv'){
    db <- read.csv(paste(filename,'csv',sep='.'))
    
    # db <- reformatForJSON(db)
    
    db <- jsonlite::toJSON(db,pretty = TRUE)
    return(db)
    
  } else if(type == 'inla'){
    model <- readRDS(paste(filename,'RDS',sep='.'))
    return(model)
  }
}

# DOESN"T WORK RIGHT NOW!
reformatForJSON <- function(dataIn,query){
  
  dataOut<-query
  dataOut$data <-dataIn %>% group_by_at(query$attributes) %>% nest(query$basis,query$values,.key=data)
  
  return(dataOut)
}