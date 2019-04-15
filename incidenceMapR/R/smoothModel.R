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
#' @import magrittr
#' @import dplyr
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
    } else if (all(db$observedData$n >= db$observedData$positive)){
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
 
  
  # we smooth across factor levels with random effects replicates: http://www.r-inla.org/models/tools#TOC-Models-with-more-than-one-type-of-likelihood
  validFactorNames <- c('pathogen','sampling_location','flu_shot','sex','has_fever','has_cough','has_myalgia')
  factorIdx <- validFactorNames %in% names(db$observedData) 
  
  # combine factors for independent intercepts
  if(any(factorIdx)){
    inputData$levelIntercept <- db$observedData %>% select(validFactorNames[factorIdx]) %>% interaction
  } else {
    inputData$levelIntercept <- as.factor('(Intercept)')
  }
  levelSet       <- levels(inputData$levelIntercept)
  numLevels      <- length(levelSet)
  
  # set family across all levels
  family <- rep(family,numLevels)

  # build outcome matrix and replicate list for multiple likelihoods
  outcome      <- matrix(NA,nrow(inputData),numLevels)
  replicateIdx <- matrix(NA,nrow(inputData),1)
  
  for( k in levelSet){
    idx <- inputData$levelIntercept %in% k
    count <- which(levelSet %in% k)
    outcome[idx, count] <- inputData$positive[idx]
    replicateIdx[idx]<-count
  }
  
  # initialize formula for each level
  if (numLevels>1){
    outcomeStr <- paste('cbind(',paste(paste('outcome',1:numLevels,sep='.'),sep='',collapse=', '),')',sep='',collapse = '')
    formula <- as.formula(paste(outcomeStr,'~','levelIntercept - 1',sep=' '))
  } else {
    formula <- outcome ~ 1
  }
 
  # build out smoothing formula  
  for(COLUMN in names(inputData)[!(names(inputData) %in% c('positive','n'))]){
    
    if(COLUMN == 'time_row'){
      
      #INLA needs one column per random effect
      inputData$time_row_rw2 <- inputData$time_row
      inputData$time_row_IID <- inputData$time_row
      
      formula <- update(formula,  ~ . + f(time_row_rw2, model='rw2', hyper=modelDefinition$hyper$global, replicate=replicateIdx) +
                          f(time_row_IID, model='iid', hyper=modelDefinition$hyper$local, replicate=replicateIdx, constr = TRUE) )
    }
    
    if(COLUMN == 'age_row'){
      
      inputData$age_row_rw2 <- inputData$age_row
      inputData$age_row_IID <- inputData$age_row
      
      formula <- update(formula,  ~ . + f(age_row_rw2, model='rw2', hyper=modelDefinition$hyper$age, replicate=replicateIdx) +
                          f(age_row_IID, model='iid', hyper=modelDefinition$hyper$local, replicate=replicateIdx, constr = TRUE) )
    }
    
    if(COLUMN %in% c('PUMA5CE')){
      
      inputData$PUMA5CERow <- match(inputData$PUMA5CE,unique(inputData$PUMA5CE))
      
      if('time_row' %in% names(inputData)){
        
        inputData$time_row_PUMA5CE <- inputData$time_row
        
        formula <- update(formula,  ~ . + f(PUMA5CERow, model='iid', hyper=modelDefinition$global, constr = TRUE, replicate=replicateIdx,
                                            group = time_row_PUMA5CE, control.group=list(model="rw2")))
      } else {
        
        formula <- update(formula,  ~ . + f(PUMA5CERow, model='iid', hyper=modelDefinition$hyper$global, replicate=replicateIdx))
      }
    }
    
    if(COLUMN %in% c('CRA_NAME')){
      
      inputData$CRA_NAMERow <- match(inputData$CRA_NAME,unique(inputData$CRA_NAME))
      
      if('time_row' %in% names(inputData)){
        
        inputData$time_row_CRA_NAME <- inputData$time_row
        
        formula <- update(formula,  ~ . + f(CRA_NAMERow, model='iid', hyper=modelDefinition$global, constr = TRUE, replicate=replicateIdx,
                                            group = time_row_CRA_NAME, control.group=list(model="rw2")))
      } else {
        
        formula <- update(formula,  ~ . + f(CRA_NAMERow, model='iid', hyper=modelDefinition$hyper$global, replicate=replicateIdx))
      }
    }
    
    if(COLUMN %in% c('NEIGHBORHOOD_DISTRICT_NAME')){
      
      inputData$NEIGHBORHOOD_DISTRICT_NAMERow <- match(inputData$NEIGHBORHOOD_DISTRICT_NAME,unique(inputData$NEIGHBORHOOD_DISTRICT_NAME))
      
      if('time_row' %in% names(inputData)){
        
        inputData$time_row_NEIGHBORHOOD_DISTRICT_NAME <- inputData$time_row
        
        formula <- update(formula,  ~ . + f(NEIGHBORHOOD_DISTRICT_NAMERow, model='iid', hyper=modelDefinition$global, constr = TRUE, replicate=replicateIdx,
                                            group = time_row_NEIGHBORHOOD_DISTRICT_NAME, control.group=list(model="rw2")))
      } else {
        
        formula <- update(formula,  ~ . + f(NEIGHBORHOOD_DISTRICT_NAMERow, model='iid', hyper=modelDefinition$hyper$global, replicate=replicateIdx))
      }
    }
    
    # Do we want the option of neighbor smoothing at larger scales?
    if(COLUMN == 'GEOID'){
      if(exists('shp')){
        neighborGraph <- constructAdjacencyNetwork(shp) 
        inputData$GEOIDRow <- shp$rowID[match(inputData$GEOID,shp$GEOID)]
        
        if('time_row' %in% names(inputData)){
          
          inputData$time_row_GEOID <- inputData$time_row
          
          formula <- update(formula,  ~ . + f(GEOIDRow, model='besag', graph=modelDefinition$neighborGraph, constr = TRUE, hyper=modelDefinition$hyper$local, replicate=replicateIdx,
                                              group = time_row_GEOID, control.group=list(model="rw2")))
        } else {
          formula <- update(formula,  ~ . + f(GEOIDRow, model='bym2', graph=modelDefinition$neighborGraph, constr = TRUE, hyper=modelDefinition$hyper$local, replicate=replicateIdx))
        }
      } else {
        
        inputData$GEOIDRow <- match(inputData$GEOID,unique(inputData$GEOID))
        
        if('time_row' %in% names(inputData)){
          
          inputData$time_row_GEOID <- inputData$time_row
          
          formula <- update(formula,  ~ . + f(GEOIDRow, model='iid', graph=modelDefinition$neighborGraph, hyper=modelDefinition$hyper$local, replicate=replicateIdx,
                                              group = time_row_GEOID, control.group=list(model="rw2")))
        } else {
          formula <- update(formula,  ~ . + f(GEOIDRow, model='iid', graph=modelDefinition$neighborGraph, hyper=modelDefinition$hyper$local, replicate=replicateIdx))
        }
        
      }
    }
  }
  
  df <- data.frame(outcome = outcome, inputData, replicateIdx)
  
  modelDefinition <- list(type='smooth', family = family, formula = formula, lincomb = c(),
                          inputData = df, neighborGraph=neighborGraph, hyper=hyper,
                          queryList = db$queryList, 
                          observedData = db$observedData)
  
  return(modelDefinition)
}


#' appendSmoothData: internal function for adding model$summary.fitted.values to db$observedData from smoothModel fit
#'
#' @param model inla model object
#' @param db object from dbViewer with observedData tibble and query
#' @return db with added modeledData tibble
#' 
appendSmoothData <- function(model,modelDefinition){

  modeledData <- modelDefinition$observedData
  
  if(modelDefinition$family[1] == 'binomial'){
    modeledData$fraction <- modeledData$positive/modeledData$n
  }
  
  # summary.fitted.values is only relevant output for smoothModel
  nCol <- ncol(modeledData)
  modeledData[,nCol+1:ncol(model$summary.fitted.values)]<-model$summary.fitted.values
  names(modeledData)[nCol+1:ncol(model$summary.fitted.values)]<-paste('fitted.values',names(model$summary.fitted.values),sep='.')
  
  rownames(modeledData)<-c()
  
  # snake_case
  names(modeledData) <- gsub('\\.','_',names(modeledData))
  
  return(modeledData)
}
