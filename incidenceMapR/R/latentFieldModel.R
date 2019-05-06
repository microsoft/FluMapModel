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
#' @import dbViewR
#'
#' @export
#' @examples
#' return h1n1pdm incidence model by time and location
#'    modelDefinition <- smoothModel(db = dbViewR::selectFromDB(), shp = dbViewR::masterSpatialDB())
#'
latentFieldModel <- function(db = dbViewR::selectFromDB(), shp = dbViewR::masterSpatialDB(), family = NULL, neighborGraph = NULL){
  
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
    formula <- as.formula('outcome ~ 1 + catchment')
  }
  
  # factors as fixed effects, assuming no interaction terms
  validFactorNames <- names(db$observedData)[ !( (names(db$observedData) %in% c('pathogen','n','positive')) | 
                                                                grepl('age',names(db$observedData)) | 
                                                                grepl('residence_',names(db$observedData)) | 
                                                                grepl('work_',names(db$observedData)) |
                                                                grepl('encounter',names(db$observedData))  )]
  
  factorIdx <- names(db$observedData) %in% validFactorNames
  for(COLUMN in names(db$observedData)[factorIdx]){
    formula <- as.formula(paste(as.character(formula)[2],'~',paste(as.character(formula)[3],COLUMN,sep='+')))
  }
  
  
  # latent fields
  for(COLUMN in names(inputData)[!(names(inputData) %in% c('positive','n'))]){
    
    if(COLUMN == 'time_row'){
      
      #INLA needs one column per random effect
      inputData$time_row_rw2 <- inputData$time_row
      inputData$time_row_IID <- inputData$time_row
      
      formula <- update(formula,  ~ . + f(time_row_rw2, model='rw2', hyper=modelDefinition$hyper$time, replicate=replicateIdx) +
                          f(time_row_IID, model='iid', hyper=modelDefinition$hyper$local, replicate=replicateIdx, constr = TRUE) )
      validLatentFieldColumns <- c(validLatentFieldColumns,'time_row_rw2','time_row_IID')
    }
    
    if(COLUMN == 'age_row'){
      
      inputData$age_row_rw2 <- inputData$age_row
      inputData$age_row_IID <- inputData$age_row
      
      formula <- update(formula,  ~ . + f(age_row_rw2, model='rw2', hyper=modelDefinition$hyper$age, replicate=replicateIdx) +
                          f(age_row_IID, model='iid', hyper=modelDefinition$hyper$local, replicate=replicateIdx, constr = TRUE) )
      validLatentFieldColumns <- c(validLatentFieldColumns,'age_row_rw2','age_row_IID')
    }
    
    if(COLUMN %in% c('residence_puma')){
      
      inputData$residence_pumaRow <- match(inputData$residence_puma,unique(inputData$residence_puma))
      
      if('time_row' %in% names(inputData)){
        
        inputData$time_row_residence_puma <- inputData$time_row
        
        formula <- update(formula,  ~ . + f(residence_pumaRow, model='iid', hyper=modelDefinition$local, constr = TRUE, replicate=replicateIdx,
                                            group = time_row_residence_puma, control.group=list(model="rw2")))
        validLatentFieldColumns <- c(validLatentFieldColumns,'residence_pumaRow','time_row_residence_puma')
      } else {
        
        formula <- update(formula,  ~ . + f(residence_pumaRow, model='iid', hyper=modelDefinition$hyper$global, replicate=replicateIdx))
        validLatentFieldColumns <- c(validLatentFieldColumns,'residence_pumaRow')
      }
    }
    
    if(COLUMN %in% c('residence_cra_name')){
      
      inputData$residence_cra_nameRow <- match(inputData$residence_cra_name,unique(inputData$residence_cra_name))
      
      if('time_row' %in% names(inputData)){
        
        inputData$time_row_residence_cra_name <- inputData$time_row
        
        formula <- update(formula,  ~ . + f(residence_cra_nameRow, model='iid', hyper=modelDefinition$local, constr = TRUE, replicate=replicateIdx,
                                            group = time_row_residence_cra_name, control.group=list(model="rw2")))
        validLatentFieldColumns <- c(validLatentFieldColumns,'residence_cra_nameRow','time_row_residence_cra_name')
      } else {
        
        formula <- update(formula,  ~ . + f(residence_cra_nameRow, model='iid', hyper=modelDefinition$hyper$global, replicate=replicateIdx))
        validLatentFieldColumns <- c(validLatentFieldColumns,'residence_cra_nameRow')
      }
    }
    
    if(COLUMN %in% c('residence_neighborhood_district_name')){
      
      inputData$residence_neighborhood_district_nameRow <- match(inputData$residence_neighborhood_district_name,unique(inputData$residence_neighborhood_district_name))
      
      if('time_row' %in% names(inputData)){
        
        inputData$time_row_residence_neighborhood_district_name <- inputData$time_row
        
        formula <- update(formula,  ~ . + f(residence_neighborhood_district_nameRow, model='iid', hyper=modelDefinition$local, constr = TRUE, replicate=replicateIdx,
                                            group = time_row_residence_neighborhood_district_name, control.group=list(model="rw2")))
        validLatentFieldColumns <- c(validLatentFieldColumns,'residence_neighborhood_district_nameRow','time_row_residence_neighborhood_district_name')
      } else {
        
        formula <- update(formula,  ~ . + f(residence_neighborhood_district_nameRow, model='iid', hyper=modelDefinition$hyper$global, replicate=replicateIdx))
        validLatentFieldColumns <- c(validLatentFieldColumns,'residence_neighborhood_district_nameRow')
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
          validLatentFieldColumns <- c(validLatentFieldColumns,'residence_census_tractRow','time_row_residence_census_tract')
        } else {
          formula <- update(formula,  ~ . + f(residence_census_tractRow, model='bym2', graph=modelDefinition$neighborGraph, constr = TRUE, hyper=modelDefinition$hyper$local, replicate=replicateIdx))
          validLatentFieldColumns <- c(validLatentFieldColumns,'residence_census_tractRow')
        }
      } else {
        
        inputData$residence_census_tractRow <- match(inputData$residence_census_tract,unique(inputData$residence_census_tract))
        
        if('time_row' %in% names(inputData)){
          
          inputData$time_row_residence_census_tract <- inputData$time_row
          
          formula <- update(formula,  ~ . + f(residence_census_tractRow, model='iid', graph=modelDefinition$neighborGraph, hyper=modelDefinition$hyper$local, replicate=replicateIdx,
                                              group = time_row_residence_census_tract, control.group=list(model="rw2")))
          validLatentFieldColumns <- c(validLatentFieldColumns,'residence_census_tractRow','time_row_residence_census_tract')
        } else {
          formula <- update(formula,  ~ . + f(residence_census_tractRow, model='iid', graph=modelDefinition$neighborGraph, hyper=modelDefinition$hyper$local, replicate=replicateIdx))
          validLatentFieldColumns <- c(validLatentFieldColumns,'residence_census_tractRow')
        }
        
      }
    }
  }
  
  # linear combination of pathogen and latent fields

    # find unique rows after discarding factors that are being averaged over
    lc.data <- data.frame(inputData[,names(inputData) %in% validLatentFieldColumns], replicateIdx = replicateIdx)
    lc.rowIdx <- !duplicated(lc.data)
    lc.data <- lc.data[lc.rowIdx,]
    
    # generate list of desired linear combinations # https://groups.google.com/forum/#!topic/r-inla-discussion-group/_e2C2L7Wc30
    lcIdx=c()
    spentColumn<-rep(FALSE,length(validLatentFieldColumns))
    for(COLUMN in validLatentFieldColumns){
      
      if(COLUMN %in% c('pathogen') ){
        
        if(numLevels>1){
          # need to promote pathogen levels to independent columns! https://groups.google.com/forum/#!topic/r-inla-discussion-group/IaTSakB7qy4
          pathogenNames <- paste('pathogen',levelSet,sep='')
        } else { # why does R do inconsistent thing with column names?????!
          pathogenNames <- '(Intercept)'
        }
        
      } else if (!(COLUMN == 'time_row_residence_puma' )) {
        groupIdx<-grepl( paste0('_',gsub('Row','',COLUMN)) ,validLatentFieldColumns)  # this nasty thing will get refactored: https://github.com/seattleflu/incidence-mapper/issues/13
        if(any(groupIdx & !spentColumn)){ # grouped?
          lcIdx[[COLUMN]] <- inla.idx(lc.data[[COLUMN]], group = lc.data[[validLatentFieldColumns[groupIdx]]], replicate = lc.data$replicateIdx)          
          spentColumn[groupIdx]<-TRUE
          
        } else if(!spentColumn[validLatentFieldColumns %in% COLUMN]) {
          lcIdx[[COLUMN]] <- inla.idx(lc.data[[COLUMN]], replicate = lc.data$replicateIdx)          
        }
      }
      spentColumn[validLatentFieldColumns %in% COLUMN]<-TRUE
    }
    
  # generate list of desired linear combinations # https://groups.google.com/forum/#!topic/r-inla-discussion-group/_e2C2L7Wc30
    lc.latentField <- vector("list", nrow(lc.data))
    
    w<-vector("list", length(names(lcIdx))+1)
    w[[length(names(lcIdx))+1]]<-1 #pathogen
    
    for(k in 1:nrow(lc.data)){

      for(n in 1:length(names(lcIdx))){
        w[[n]]<-rep(0,nrow(lc.data))
        w[[n]][lcIdx[[n]][k]]<-1
      }
      names(w) <- c(names(lcIdx),pathogenNames[lc.data$replicateIdx[k]])

      lc <- inla.make.lincomb(w)
      names(lc)<- paste0('latent_field',k)
      lc.latentField[k]<-lc
      lc.data$latentField[k]<-names(lc)
      
      if( (k %% 100) == 0){
        print(k/nrow(lc.data))
      }
    }

    
  # get original values for linear combination categories
  lc.colIdx <- (names(inputData) %in% db$queryList$GROUP_BY$COLUMN) & !(names(inputData) %in% validFactorNames)
  lc.data <- inputData[lc.rowIdx,lc.colIdx]
    
  df <- data.frame(outcome = outcome, inputData, replicateIdx)
  
  modelDefinition <- list(type='latent_field', family = family, formula = formula, lincomb = lc.latentField,
                          inputData = df, neighborGraph=neighborGraph, hyper=hyper, 
                          latentFieldData = lc.data,  
                          observedData = db$observedData,
                          queryList = db$queryList)

  return(modelDefinition)
}


