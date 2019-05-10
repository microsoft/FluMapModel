#' appendSmoothData: internal function for adding model$summary.fitted.values to db$observedData from smoothModel fit
#'
#' @param model inla model object
#' @param db object from dbViewer with observedData tibble and query
#' 
#' @import dplyr
#' @import magrittr
#' 
#' @return db with added modeledData tibble
#' 
appendSmoothData <- function(model,modelDefinition){
  
  modeledData <- modelDefinition$observedData
  outputColName <- 'modeled_count'
  
  if(modelDefinition$family[1] == 'binomial'){
    modeledData$fraction <- modeledData$positive/modeledData$n
    outputColName <- 'modeled_fraction'
  }
  
  # summary.fitted.values is only relevant output for smoothModel
  nCol <- ncol(modeledData)
  modeledData[,nCol+1:ncol(model$summary.fitted.values)]<-model$summary.fitted.values
  names(modeledData)[nCol+1:ncol(model$summary.fitted.values)]<-paste(outputColName,names(model$summary.fitted.values),sep='.')
  
  rownames(modeledData)<-c()
  
  # snake_case
  names(modeledData) <- gsub('\\.','_',names(modeledData))
  
  # clean quantile names
  names(modeledData)[grepl('0_',names(modeledData))]<-paste(outputColName,c('lower_95_CI','median','upper_95_CI'),sep='_')
  
  # pretty order 
  columns <- modelDefinition$queryList$GROUP_BY$COLUMN[modelDefinition$queryList$GROUP_BY$COLUMN %in% names(modeledData)]
  modeledData <- modeledData %>% arrange_(.dots=columns)
  
  return(modeledData)
}


#' appendLatentFieldData: internal function for adding model$summary.random to db$observedData from latentFieldModel fit
#'
#' @param model inla model object
#' @param db object from dbViewer with observedData tibble and query
#' @return db with added modeledData tibble
#' 
appendLatentFieldData <- function(model,modelDefinition){
  
  # summary.fitted.values
  modeledData <- appendSmoothData(model,modelDefinition)
  
  outputColName <- 'modeled_intensity'
  
  
  # latent field
  # summary.lincomb.derived
  latentField <- modelDefinition$latentFieldData
  nCol <- ncol(latentField)
  latentField[,nCol+1:ncol(model$summary.lincomb.derived)]<-model$summary.lincomb.derived
  names(latentField)[nCol+1:ncol(model$summary.lincomb.derived)]<-paste(outputColName,names(model$summary.lincomb.derived),sep='.')
  
  rownames(latentField)<-c()
  
  # snake_case
  names(latentField) <- gsub('\\.','_',names(latentField))
  
  # filter out unwanted fields
  latentField <- latentField
  latentField <- latentField[!(names(latentField) %in% c('modeled_intensity_ID','modeled_intensity_kld'))]
  
  # clean quantile names
  names(latentField)[grepl('0_',names(latentField))]<-paste('modeled_intensity',c('lower_95_CI','median','upper_95_CI'),sep='_')
  
  # apply link function
  if (modelDefinition$family[1] == 'poisson'){
    for( COLUMN in names(latentField)[grepl('modeled',names(latentField))]){
      if (grepl('sd',COLUMN)){
        latentField[[COLUMN]] <- exp(latentField$modeled_intensity_median + latentField$modeled_intensity_sd^2/2) * sqrt(exp(latentField$modeled_intensity_sd^2)-1)
      } else if (grepl('mean',COLUMN)){
        latentField[[COLUMN]] <- exp(latentField$modeled_intensity_median + latentField$modeled_intensity_sd^2/2)
      } else {
        latentField[[COLUMN]] <- exp(latentField[[COLUMN]])
      }
    }
    
  } else if (modelDefinition$family[1] == 'binomial'){
    for( COLUMN in names(latentField)[grepl('modeled',names(latentField))]){
      if (grepl('sd',COLUMN)){
        # TODO: transform marginals
        
      } else if (grepl('mean',COLUMN)){
        # TODO: transform marginals
        
      } else {
        latentField[[COLUMN]] <- exp(latentField[[COLUMN]])/(1+latentField[[COLUMN]])
      }
    }
    
  } 
  
  
  # pretty order 
  columns <- modelDefinition$queryList$GROUP_BY$COLUMN[modelDefinition$queryList$GROUP_BY$COLUMN %in% names(latentField)]
  latentField <- latentField %>% arrange_(.dots=columns)
  
  return(list(modeledData = modeledData, latentField = latentField))
}