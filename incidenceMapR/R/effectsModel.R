#' effectsModel: function to define regression models to study effects of covariates from dbViewR object

#GHH copy from latentFieldModel
#' latentFieldModel: function to define latent field models from dbViewR object
#'
#' @param db dbViewR object with valid column names for INLA model.
#'   Smoothing only makes sense by age, location, and time.  Factor variables cannot be smoothed!
#' @param shp sf object with residence_census_tract shapes (all higher levels assume iid and not local smoothing)
#' @param family non-standard family override (default = NULL). 
#' @param neighborGraph non-standard neighbor graph (default = NULL)
#' 
#' @return modelDefinition object for modelTrainR, list with fields
#'     type = latentField
#'     family : as input
#'     formula : model definition for INLA
#'     inputData : inla-prepared db$observed data
#'     neighborGraph : as input or derived from shp during formula construction
#'     
#' @import INLA
#'
#' @export
#' @examples
#' return h1n1pdm incidence model by time and location
#'    modelDefinition <- smoothModel(db = dbViewR::selectFromDB(), shp = dbViewR::masterSpatialDB())
#'
effectsModel <- function(db , shp, family = NULL, neighborGraph = NULL){
  
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
  hyper$local <- list(prec = list( prior = "pc.prec", param = 1/200, alpha = 0.01))
  hyper$age <- list(prec = list( prior = "pc.prec", param = 1, alpha = 0.01))
  hyper$time <- list(prec = list( prior = "pc.prec", param = 1/50, alpha = 0.01))
  
  
  # unlike smoothing model, we only replicate latent fields across pathogens, but treat all other factors as fixed effects
  
  # find pathogen types
  if('pathogen' %in% names(db$observedData)){
    levelSet       <- levels(as.factor(inputData$pathogen))
    numLevels      <- length(levelSet)
    
    validLatentFieldColumns <- c('pathogen')
    
  } else {
    return('error!  must provide "pathogen" column.')
  }
  
  # set family across all levels
  family <- rep(family,numLevels)
  
  # build outcome matrix and replicate list for multiple likelihoods
  outcome      <- matrix(NA,nrow(inputData),numLevels)
  replicateIdx <- matrix(NA,nrow(inputData),1)
  
  for( k in levelSet){
    idx <- inputData$pathogen %in% k
    count <- which(levelSet %in% k)
    outcome[idx, count] <- inputData$positive[idx]
    replicateIdx[idx]<-count
  }
  
  # initialize formula for each level
  if(numLevels>1){
    outcomeStr <- paste('cbind(',paste(paste('outcome',1:numLevels,sep='.'),sep='',collapse=', '),')',sep='',collapse = '')
    formula <- as.formula(paste(outcomeStr,'~','pathogen - 1 + catchment',sep=' '))
  } else { # why does R do inconsistent stuff with column names!?!!
    #formula <- as.formula('outcome ~ 1 + catchment')
    formula <- as.formula('outcome ~ 1')
  }
  
  # factors as fixed effects, assuming no interaction terms
  validFactorNames <- names(db$observedData)[ !( (names(db$observedData) %in% c('pathogen','n','positive')) | 
                                                   grepl('row',names(db$observedData)) |
                                                   grepl('age',names(db$observedData)) | 
                                                   grepl('residence_',names(db$observedData)) | 
                                                   grepl('work_',names(db$observedData)) |
                                                   grepl('encounter',names(db$observedData))  )]
  
  factorIdx <- names(db$observedData) %in% validFactorNames
  for(COLUMN in names(db$observedData)[factorIdx]){
    formula <- as.formula(paste(as.character(formula)[2],'~',paste(as.character(formula)[3],COLUMN,sep='+')))
  }
  
  
  df <- data.frame(outcome = outcome, inputData, replicateIdx)
  
  if(any(grepl('residence', names(inputData)) | grepl('work', names(inputData)))){
    spatial_domain<-shp$domain[1]
  } else {
    spatial_domain <- NULL
  }
  
  modelDefinition <- list(type='effects', family = family, formula = formula, lincomb = c(),
                          inputData = df, neighborGraph=neighborGraph, hyper=hyper, 
                          observedData = db$observedData,
                          queryList = db$queryList,
                          spatial_domain = spatial_domain)
  
  return(modelDefinition)
}


