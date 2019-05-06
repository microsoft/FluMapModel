#' smoothModel: function to define variable smoothing models from dbViewR object
#'
#' @param db dbViewR object with valid column names for INLA model.
#'   Smoothing only makes sense by age, location, and time.  Factor variables cannot be smoothed!
#' @param shp sf object with residence_census_tract shapes (all higher levels assume iid and not local smoothing)
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
    if (all(inputData$n == inputData$positive | is.na(db$observedData$positive))){
      family = 'poisson'
    } else if (all(db$observedData$n >= db$observedData$positive | is.na(db$observedData$positive))){
      family = 'binomial'
    } else if (any(inputData$n < inputData$positive | is.na(db$observedData$positive))){
      return('n < positive !!!  invald db$observedData.')
    }
  }
  
  # construct priors
  hyper=list()
  hyper$global <- list(prec = list( prior = "pc.prec", param = 1/10, alpha = 0.01))
  hyper$local <- list(prec = list( prior = "pc.prec", param = 1/100, alpha = 0.01))
  hyper$age <- list(prec = list( prior = "pc.prec", param = 1/100, alpha = 0.01))
 
  
  # we smooth across factor levels with random effects replicates: http://www.r-inla.org/models/tools#TOC-Models-with-more-than-one-type-of-likelihood
  validFactorNames <- names(db$observedData)[ !( (names(db$observedData) %in% c('pathogen','n','positive')) | 
                                                              grepl('age',names(db$observedData)) | 
                                                              grepl('residence_',names(db$observedData)) | 
                                                              grepl('work_',names(db$observedData)) |
                                                              grepl('encounter',names(db$observedData))  )]
  
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
    
    if(COLUMN %in% c('residence_puma')){
      
      inputData$residence_pumaRow <- match(inputData$residence_puma,unique(inputData$residence_puma))
      
      if('time_row' %in% names(inputData)){
        
        inputData$time_row_residence_puma <- inputData$time_row
        
        formula <- update(formula,  ~ . + f(residence_pumaRow, model='iid', hyper=modelDefinition$global, constr = TRUE, replicate=replicateIdx,
                                            group = time_row_residence_puma, control.group=list(model="rw2")))
      } else {
        
        formula <- update(formula,  ~ . + f(residence_pumaRow, model='iid', hyper=modelDefinition$hyper$global, replicate=replicateIdx))
      }
    }
    
    if(COLUMN %in% c('residence_cra_name')){
      
      inputData$residence_cra_nameRow <- match(inputData$residence_cra_name,unique(inputData$residence_cra_name))
      
      if('time_row' %in% names(inputData)){
        
        inputData$time_row_residence_cra_name <- inputData$time_row
        
        formula <- update(formula,  ~ . + f(residence_cra_nameRow, model='iid', hyper=modelDefinition$global, constr = TRUE, replicate=replicateIdx,
                                            group = time_row_residence_cra_name, control.group=list(model="rw2")))
      } else {
        
        formula <- update(formula,  ~ . + f(residence_cra_nameRow, model='iid', hyper=modelDefinition$hyper$global, replicate=replicateIdx))
      }
    }
    
    if(COLUMN %in% c('residence_neighborhood_district_name')){
      
      inputData$residence_neighborhood_district_nameRow <- match(inputData$residence_neighborhood_district_name,unique(inputData$residence_neighborhood_district_name))
      
      if('time_row' %in% names(inputData)){
        
        inputData$time_row_residence_neighborhood_district_name <- inputData$time_row
        
        formula <- update(formula,  ~ . + f(residence_neighborhood_district_nameRow, model='iid', hyper=modelDefinition$global, constr = TRUE, replicate=replicateIdx,
                                            group = time_row_residence_neighborhood_district_name, control.group=list(model="rw2")))
      } else {
        
        formula <- update(formula,  ~ . + f(residence_neighborhood_district_nameRow, model='iid', hyper=modelDefinition$hyper$global, replicate=replicateIdx))
      }
    }
    
    # Do we want the option of neighbor smoothing at larger scales?
    if(COLUMN == 'residence_census_tract'){
      if(exists('shp')){
        neighborGraph <- constructAdjacencyNetwork(shp) 
        inputData$residence_census_tractRow <- shp$rowID[match(inputData$residence_census_tract,shp$residence_census_tract)]
        
        if('time_row' %in% names(inputData)){
          
          inputData$time_row_residence_census_tract <- inputData$time_row
          
          formula <- update(formula,  ~ . + f(residence_census_tractRow, model='besag', graph=modelDefinition$neighborGraph, constr = TRUE, hyper=modelDefinition$hyper$local, replicate=replicateIdx,
                                              group = time_row_residence_census_tract, control.group=list(model="rw2")))
        } else {
          formula <- update(formula,  ~ . + f(residence_census_tractRow, model='bym2', graph=modelDefinition$neighborGraph, constr = TRUE, hyper=modelDefinition$hyper$local, replicate=replicateIdx))
        }
      } else {
        
        inputData$residence_census_tractRow <- match(inputData$residence_census_tract,unique(inputData$residence_census_tract))
        
        if('time_row' %in% names(inputData)){
          
          inputData$time_row_residence_census_tract <- inputData$time_row
          
          formula <- update(formula,  ~ . + f(residence_census_tractRow, model='iid', graph=modelDefinition$neighborGraph, hyper=modelDefinition$hyper$local, replicate=replicateIdx,
                                              group = time_row_residence_census_tract, control.group=list(model="rw2")))
        } else {
          formula <- update(formula,  ~ . + f(residence_census_tractRow, model='iid', graph=modelDefinition$neighborGraph, hyper=modelDefinition$hyper$local, replicate=replicateIdx))
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


