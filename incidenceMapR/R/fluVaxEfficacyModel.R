#' fluVaxEfficacyModel: function to define latent field model tailored to test-negative study
#' for flu vax efficacy from dbViewR object
#'
#' @param db dbViewR object with valid column names for INLA model. required fields: "pathogen" and "flu_shot"
#' @param shp sf object with residence_census_tract shapes (all higher levels assume iid and not local smoothing)
#' @param family non-standard family override (default = NULL). 
#' @param neighborGraph non-standard neighbor graph (default = NULL)
#' 
#' @return modelDefinition object for modelTrainR, list with fields
#'     type = vaccine_efficacy
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
fluVaxEfficacyModel <- function(db , shp=NULL, neighborGraph = NULL){
  
  #INLA data frame that may get augmented columns we don't need to see when we're done
  inputData <- db$observedData
  
  # confirmd data is appropriate for binomial model
  if(any(inputData$n > inputData$positive)){
    family = 'binomial'
  } else {
    return('invald db$observedData for required binomial model.')
  }

  # construct priors
  hyper=list()
  hyper$global <- list(prec = list( prior = "pc.prec", param = 1/10, alpha = 0.01))
  hyper$local <- list(prec = list( prior = "pc.prec", param = 1/200, alpha = 0.01))
  hyper$age <- list(prec = list( prior = "pc.prec", param = 1, alpha = 0.01))
  hyper$time <- list(prec = list( prior = "pc.prec", param = 1/50, alpha = 0.01))
  


  validFluVaxEfficacyColumns <- c('pathogen','flu_shot')
  if(!all(validFluVaxEfficacyColumns %in% names(inputData))) {
    return('error! fluVaxEfficacyModel requires pathogen and flu_shot columns.')
  }
  
  # construct factors for latent field replicates
  validFactorNames <- names(db$observedData)[ !( (names(db$observedData) %in% c('pathogen','n','positive')) | 
                                                   grepl('row',names(db$observedData)) |
                                                   grepl('age',names(db$observedData)) | 
                                                   grepl('residence_',names(db$observedData)) | 
                                                   grepl('work_',names(db$observedData)) |
                                                   grepl('encounter',names(db$observedData))  )]
  
  factorIdx <- validFactorNames %in% names(db$observedData) 
  
  # combine factors for independent intercepts
  inputData$levelIntercept <- db$observedData %>% select(validFactorNames[factorIdx]) %>% interaction
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
    return('error!  fluVaxEfficacy requres at least two levels for comparison.')
  }
  

  # latent fields
  for(COLUMN in names(inputData)[!(names(inputData) %in% c('positive','n'))]){
    
    if(COLUMN == 'time_row'){
      
      #INLA needs one column per random effect
      inputData$time_row_rw2 <- inputData$time_row
      inputData$time_row_IID <- inputData$time_row
      
      formula <- update(formula,  ~ . + f(time_row_rw2, model='rw2', hyper=modelDefinition$hyper$time, replicate=replicateIdx) +
                          f(time_row_IID, model='iid', hyper=modelDefinition$hyper$local, replicate=replicateIdx, constr = TRUE) )
      validFluVaxEfficacyColumns <- c(validFluVaxEfficacyColumns,'time_row_rw2','time_row_IID')
    }
    
    if(COLUMN == 'age_row'){
      
      inputData$age_row_rw2 <- inputData$age_row
      inputData$age_row_IID <- inputData$age_row
      
      formula <- update(formula,  ~ . + f(age_row_rw2, model='rw2', hyper=modelDefinition$hyper$age, replicate=replicateIdx) +
                          f(age_row_IID, model='iid', hyper=modelDefinition$hyper$local, replicate=replicateIdx, constr = TRUE) )
      validFluVaxEfficacyColumns <- c(validFluVaxEfficacyColumns,'age_row_rw2','age_row_IID')
    }
    
    if(COLUMN %in% c('residence_puma')){
      
      inputData$residence_pumaRow <- match(inputData$residence_puma,unique(inputData$residence_puma))
      
      if('time_row' %in% names(inputData)){
        
        inputData$time_row_residence_puma <- inputData$time_row
        
        formula <- update(formula,  ~ . + f(residence_pumaRow, model='iid', hyper=modelDefinition$local, constr = TRUE, replicate=replicateIdx,
                                            group = time_row_residence_puma, control.group=list(model="rw2")))
        validFluVaxEfficacyColumns <- c(validFluVaxEfficacyColumns,'residence_pumaRow','time_row_residence_puma')
      } else {
        
        formula <- update(formula,  ~ . + f(residence_pumaRow, model='iid', hyper=modelDefinition$hyper$global, replicate=replicateIdx))
        validFluVaxEfficacyColumns <- c(validFluVaxEfficacyColumns,'residence_pumaRow')
      }
    }
    
    if(COLUMN %in% c('residence_cra_name')){
      
      inputData$residence_cra_nameRow <- match(inputData$residence_cra_name,unique(inputData$residence_cra_name))
      
      if('time_row' %in% names(inputData)){
        
        inputData$time_row_residence_cra_name <- inputData$time_row
        
        formula <- update(formula,  ~ . + f(residence_cra_nameRow, model='iid', hyper=modelDefinition$local, constr = TRUE, replicate=replicateIdx,
                                            group = time_row_residence_cra_name, control.group=list(model="rw2")))
        validFluVaxEfficacyColumns <- c(validFluVaxEfficacyColumns,'residence_cra_nameRow','time_row_residence_cra_name')
      } else {
        
        formula <- update(formula,  ~ . + f(residence_cra_nameRow, model='iid', hyper=modelDefinition$hyper$global, replicate=replicateIdx))
        validFluVaxEfficacyColumns <- c(validFluVaxEfficacyColumns,'residence_cra_nameRow')
      }
    }
    
    if(COLUMN %in% c('residence_neighborhood_district_name')){
      
      inputData$residence_neighborhood_district_nameRow <- match(inputData$residence_neighborhood_district_name,unique(inputData$residence_neighborhood_district_name))
      
      if('time_row' %in% names(inputData)){
        
        inputData$time_row_residence_neighborhood_district_name <- inputData$time_row
        
        formula <- update(formula,  ~ . + f(residence_neighborhood_district_nameRow, model='iid', hyper=modelDefinition$local, constr = TRUE, replicate=replicateIdx,
                                            group = time_row_residence_neighborhood_district_name, control.group=list(model="rw2")))
        validFluVaxEfficacyColumns <- c(validFluVaxEfficacyColumns,'residence_neighborhood_district_nameRow','time_row_residence_neighborhood_district_name')
      } else {
        
        formula <- update(formula,  ~ . + f(residence_neighborhood_district_nameRow, model='iid', hyper=modelDefinition$hyper$global, replicate=replicateIdx))
        validFluVaxEfficacyColumns <- c(validFluVaxEfficacyColumns,'residence_neighborhood_district_nameRow')
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
          validFluVaxEfficacyColumns <- c(validFluVaxEfficacyColumns,'residence_census_tractRow','time_row_residence_census_tract')
        } else {
          formula <- update(formula,  ~ . + f(residence_census_tractRow, model='bym2', graph=modelDefinition$neighborGraph, constr = TRUE, hyper=modelDefinition$hyper$local, replicate=replicateIdx))
          validFluVaxEfficacyColumns <- c(validFluVaxEfficacyColumns,'residence_census_tractRow')
        }
      } else {
        
        inputData$residence_census_tractRow <- match(inputData$residence_census_tract,unique(inputData$residence_census_tract))
        
        if('time_row' %in% names(inputData)){
          
          inputData$time_row_residence_census_tract <- inputData$time_row
          
          formula <- update(formula,  ~ . + f(residence_census_tractRow, model='iid', graph=modelDefinition$neighborGraph, hyper=modelDefinition$hyper$local, replicate=replicateIdx,
                                              group = time_row_residence_census_tract, control.group=list(model="rw2")))
          validFluVaxEfficacyColumns <- c(validFluVaxEfficacyColumns,'residence_census_tractRow','time_row_residence_census_tract')
        } else {
          formula <- update(formula,  ~ . + f(residence_census_tractRow, model='iid', graph=modelDefinition$neighborGraph, hyper=modelDefinition$hyper$local, replicate=replicateIdx))
          validFluVaxEfficacyColumns <- c(validFluVaxEfficacyColumns,'residence_census_tractRow')
        }
        
      }
    }
  }
  
  # I'm modeling flu_shot as a fixed effect with a replicate by age for each level, and so the contrasts have no covariance.
  # Thus, I'm calcuting the odds ratio contrasts from summary.linear.predictor in appendFluVaxEfficacyModel after running modelTrainR
  # instead of figuring out how to code the linear combinations here.  But we should revisit this, as it might make sense to model
  # the interaction itself between flu_shot levels as random effects aswell to be coherent across covariates.
  
  # Strata data 
  lc.colIdx <- (names(inputData) %in% c('pathogen',db$queryList$GROUP_BY$COLUMN)) | (names(inputData) %in% validFactorNames) & !(names(inputData) %in% 'flu_shot')
  lc.colIdx <- lc.colIdx & !(names(inputData) %in% 'flu_shot')
  lc.data<-inputData %>% distinct_(.dots=names(inputData)[lc.colIdx])
  rownames(lc.data)<-c()
  
  df <- data.frame(outcome = outcome, inputData, replicateIdx)
  
  if(any(grepl('residence', names(inputData)) | grepl('work', names(inputData)))){
    spatial_domain<-shp$domain[1]
  } else {
    spatial_domain <- NULL
  }
  
  modelDefinition <- list(type='vaccine_efficacy', family = family, formula = formula,
                          inputData = df, neighborGraph=neighborGraph, hyper=hyper, 
                          vaxEfficacyData = lc.data,  
                          observedData = db$observedData,
                          queryList = db$queryList,
                          spatial_domain = spatial_domain)

  return(modelDefinition)
}


