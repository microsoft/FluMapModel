#' smoothModel: function to define variable smoothing models from dbViewR object
#'
#' @param db dbViewR object with valid column names for INLA model.
#'   Smoothing only makes sense by age, location, and time.  Factor variables cannot be smoothed!
#' @param shp sf object with GEOID shapes (all higher levels assume iid and not local smoothing)
#' @param family non-standard family override (default = NULL). 
#' @param neighborGraph non-standard neighbor graph (default = NULL)
#' 
#' @return modelDefinition object for modelTrainR, list with fields
#'     type = smooth
#'     family : as input
#'     formula : model definition for INLA
#'     inputData : inla-prepared db$observed data
#'     neighborGraph : as input or derived from shp during formula construction
#'     
#' @import INLA
#' @import dbViewR
#'
#' @export
#' @examples
#' return h1n1pdm incidence model by time and location
#'    modelDefinition <- smoothModel(db = dbViewR::selectFromDB(), shp = dbViewR::masterSpatialDB())
#'
smoothModel <- function(db = dbViewR::selectFromDB(), shp = dbViewR::masterSpatialDB(), family = NULL, neighborGraph = NULL){

  
  #INLA data frame that may get augmented columns we don't need to see when we're done
  inputData <- db$observedData
  
  # identify intended family
  if(is.null(family)){
    if (all(inputData$n == inputData$positive)){
      family = 'poisson'
    } else if (any(inputData$n > inputData$positive)){
      family = 'binomial'
    } else if (any(inputData$n < inputData$positive)){
      return('n < positive !!!  invald db$observedData.')
    }
  }
  
  # construct priors
  hyper=list()
  hyper$global <- list(prec = list( prior = "pc.prec", param = 1/10, alpha = 0.01))
  hyper$local <- list(prec = list( prior = "pc.prec", param = 1/100, alpha = 0.01))
  hyper$age <- list(prec = list( prior = "pc.prec", param = 1/100, alpha = 0.01))
  
  
  # build formula
  formula <- positive ~ 1

  for(COLUMN in names(inputData)[!(names(inputData) %in% c('positive','n'))]){
    
    # random effects
    if(COLUMN == 'timeRow'){
      
      #INLA needs one column per random effect
      inputData$timeRow_rw2 <- inputData$timeRow
      inputData$timeRow_IID <- inputData$timeRow
      
      formula <- update(formula,  ~ . + f(timeRow_rw2, model='rw2', hyper=modelDefinition$hyper$global) +
                          f(timeRow_IID, model='iid', hyper=modelDefinition$hyper$local) )
    }
    
    if(COLUMN == 'ageRow'){
      
      inputData$ageRow_rw2 <- inputData$ageRow
      inputData$ageRow_IID <- inputData$ageRow
      
      formula <- update(formula,  ~ . + f(ageRow_rw2, model='rw2', hyper=modelDefinition$hyper$age) +
                          f(ageRow_IID, model='iid', hyper=modelDefinition$hyper$local) )
    }
    
    if(COLUMN %in% c('PUMA5CE','CRA_NAME','NEIGHBORHOOD_DISTRICT_NAME')){
      
      inputData$PUMA5CERow <- match(inputData$PUMA5CE,unique(inputData$PUMA5CE))
      
      if('timeRow' %in% names(inputData)){
        
        inputData$timeRow_PUMA5CE <- inputData$timeRow
        
        formula <- update(formula,  ~ . + f(PUMA5CERow, model='iid', hyper=modelDefinition$global, constr = TRUE,
                                            group = timeRow_PUMA5CE, control.group=list(model="rw2")))
      } else {
        
        formula <- update(formula,  ~ . + f(PUMA5CERow, model='iid', hyper=modelDefinition$hyper$global))
      }
    }
    
    # Do we want the option of neighbor smoothing at larger scales?
    if(COLUMN == 'GEOID'){
      if(exists('shp')){
        neighborGraph <- constructAdjacencyNetwork(shp) 
        inputData$GEOIDRow <- shp$rowID[match(inputData$GEOID,shp$GEOID)]
        
        if('timeRow' %in% names(inputData)){
          
          inputData$timeRow_GEOID <- inputData$timeRow
          
          formula <- update(formula,  ~ . + f(GEOIDRow, model='besag', graph=modelDefinition$neighborGraph, constr = TRUE, hyper=modelDefinition$hyper$local,
                                              group = timeRow_GEOID, control.group=list(model="rw2")))
        } else {
          formula <- update(formula,  ~ . + f(GEOIDRow, model='bym2', graph=modelDefinition$neighborGraph, constr = TRUE, hyper=modelDefinition$hyper$local))
        }
      } else {
        
        inputData$GEOIDRow <- match(inputData$GEOID,unique(inputData$GEOID))
        
        if('timeRow' %in% names(inputData)){
          
          inputData$timeRow_GEOID <- inputData$timeRow
          
          formula <- update(formula,  ~ . + f(GEOIDRow, model='iid', graph=modelDefinition$neighborGraph, hyper=modelDefinition$hyper$local,
                                              group = timeRow_GEOID, control.group=list(model="rw2")))
        } else {
          formula <- update(formula,  ~ . + f(GEOIDRow, model='iid', graph=modelDefinition$neighborGraph, hyper=modelDefinition$hyper$local))
        }
        
      }
    }
    
  }
  
  modelDefinition <- list(type='smooth', family = family, formula = formula, 
                          inputData = inputData, neighborGraph=neighborGraph, hyper=hyper,
                          queryList = db$queryList)
  
  return(modelDefinition)
}


#' appendSmoothData: internal function for adding model$summary.fitted.values to db$observedData from smoothModel fit
#'
#' @param model inla model object
#' @param db object from dbViewer with observedData tibble and query
#' @return db with added modeledData tibble
#'
appendSmoothData <- function(model,db){

  modeledData <- db$observedData
  
  # summary.fitted.values is only relevant output for smoothModel
  nCol <- ncol(db$observedData)
  modeledData[,nCol+1:ncol(model$summary.fitted.values)]<-model$summary.fitted.values
  names(modeledData)[nCol+1:ncol(model$summary.fitted.values)]<-paste('fitted.values',names(model$summary.fitted.values),sep='.')
  
  rownames(modeledData)<-c()
  return(modeledData)
}
