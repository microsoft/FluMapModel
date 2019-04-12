#' latentFieldModel: function to define latent field models from dbViewR object
#'
#' @param db dbViewR object with valid column names for INLA model.
#'   Smoothing only makes sense by age, location, and time.  Factor variables cannot be smoothed!
#' @param shp sf object with GEOID shapes (all higher levels assume iid and not local smoothing)
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
  if (numLevels>1){
    outcomeStr <- paste('cbind(',paste(paste('outcome',1:numLevels,sep='.'),sep='',collapse=', '),')',sep='',collapse = '')
    formula <- as.formula(paste(outcomeStr,'~','pathogen - 1 + catchment',sep=' '))
  } else {
    formula <- outcome ~ 1 + catchment
  }
  
  
  # factors as fixed effects, assuming no interaction terms
  validFactorNames <- c('samplingLocation','fluShot','sex','hasFever','hasCough','hasMyalgia')
  factorIdx <- names(db$observedData) %in% validFactorNames
  for(COLUMN in names(db$observedData)[factorIdx]){
    formula <- as.formula(paste(as.character(formula)[2],'~',paste(as.character(formula)[3],COLUMN,sep='+')))
  }
  
  
  # latent fields
  for(COLUMN in names(inputData)[!(names(inputData) %in% c('positive','n'))]){
    
    if(COLUMN == 'timeRow'){
      
      #INLA needs one column per random effect
      inputData$timeRow_rw2 <- inputData$timeRow
      inputData$timeRow_IID <- inputData$timeRow
      
      formula <- update(formula,  ~ . + f(timeRow_rw2, model='rw2', hyper=modelDefinition$hyper$global, replicate=replicateIdx) +
                          f(timeRow_IID, model='iid', hyper=modelDefinition$hyper$local, replicate=replicateIdx, constr = TRUE) )
      validLatentFieldColumns <- c(validLatentFieldColumns,'timeRow_rw2','timeRow_IID')
    }
    
    if(COLUMN == 'ageRow'){
      
      inputData$ageRow_rw2 <- inputData$ageRow
      inputData$ageRow_IID <- inputData$ageRow
      
      formula <- update(formula,  ~ . + f(ageRow_rw2, model='rw2', hyper=modelDefinition$hyper$age, replicate=replicateIdx) +
                          f(ageRow_IID, model='iid', hyper=modelDefinition$hyper$local, replicate=replicateIdx, constr = TRUE) )
      validLatentFieldColumns <- c(validLatentFieldColumns,'ageRow_rw2','ageRow_IID')
    }
    
    if(COLUMN %in% c('PUMA5CE')){
      
      inputData$PUMA5CERow <- match(inputData$PUMA5CE,unique(inputData$PUMA5CE))
      
      if('timeRow' %in% names(inputData)){
        
        inputData$timeRow_PUMA5CE <- inputData$timeRow
        
        formula <- update(formula,  ~ . + f(PUMA5CERow, model='iid', hyper=modelDefinition$global, constr = TRUE, replicate=replicateIdx,
                                            group = timeRow_PUMA5CE, control.group=list(model="rw2")))
        validLatentFieldColumns <- c(validLatentFieldColumns,'PUMA5CERow','timeRow_PUMA5CE')
      } else {
        
        formula <- update(formula,  ~ . + f(PUMA5CERow, model='iid', hyper=modelDefinition$hyper$global, replicate=replicateIdx))
        validLatentFieldColumns <- c(validLatentFieldColumns,'PUMA5CERow')
      }
    }
    
    if(COLUMN %in% c('CRA_NAME')){
      
      inputData$CRA_NAMERow <- match(inputData$CRA_NAME,unique(inputData$CRA_NAME))
      
      if('timeRow' %in% names(inputData)){
        
        inputData$timeRow_CRA_NAME <- inputData$timeRow
        
        formula <- update(formula,  ~ . + f(CRA_NAMERow, model='iid', hyper=modelDefinition$global, constr = TRUE, replicate=replicateIdx,
                                            group = timeRow_CRA_NAME, control.group=list(model="rw2")))
        validLatentFieldColumns <- c(validLatentFieldColumns,'CRA_NAMERow','timeRow_CRA_NAME')
      } else {
        
        formula <- update(formula,  ~ . + f(CRA_NAMERow, model='iid', hyper=modelDefinition$hyper$global, replicate=replicateIdx))
        validLatentFieldColumns <- c(validLatentFieldColumns,'CRA_NAMERow')
      }
    }
    
    if(COLUMN %in% c('NEIGHBORHOOD_DISTRICT_NAME')){
      
      inputData$NEIGHBORHOOD_DISTRICT_NAMERow <- match(inputData$NEIGHBORHOOD_DISTRICT_NAME,unique(inputData$NEIGHBORHOOD_DISTRICT_NAME))
      
      if('timeRow' %in% names(inputData)){
        
        inputData$timeRow_NEIGHBORHOOD_DISTRICT_NAME <- inputData$timeRow
        
        formula <- update(formula,  ~ . + f(NEIGHBORHOOD_DISTRICT_NAMERow, model='iid', hyper=modelDefinition$global, constr = TRUE, replicate=replicateIdx,
                                            group = timeRow_NEIGHBORHOOD_DISTRICT_NAME, control.group=list(model="rw2")))
        validLatentFieldColumns <- c(validLatentFieldColumns,'NEIGHBORHOOD_DISTRICT_NAMERow','timeRow_NEIGHBORHOOD_DISTRICT_NAME')
      } else {
        
        formula <- update(formula,  ~ . + f(NEIGHBORHOOD_DISTRICT_NAMERow, model='iid', hyper=modelDefinition$hyper$global, replicate=replicateIdx))
        validLatentFieldColumns <- c(validLatentFieldColumns,'NEIGHBORHOOD_DISTRICT_NAMERow')
      }
    }
    
    # Do we want the option of neighbor smoothing at larger scales?
    if(COLUMN == 'GEOID'){
      if(exists('shp')){
        neighborGraph <- constructAdjacencyNetwork(shp) 
        inputData$GEOIDRow <- shp$rowID[match(inputData$GEOID,shp$GEOID)]
        
        if('timeRow' %in% names(inputData)){
          
          inputData$timeRow_GEOID <- inputData$timeRow
          
          formula <- update(formula,  ~ . + f(GEOIDRow, model='besag', graph=modelDefinition$neighborGraph, constr = TRUE, hyper=modelDefinition$hyper$local, replicate=replicateIdx,
                                              group = timeRow_GEOID, control.group=list(model="rw2")))
          validLatentFieldColumns <- c(validLatentFieldColumns,'GEOIDRow','timeRow_GEOID')
        } else {
          formula <- update(formula,  ~ . + f(GEOIDRow, model='bym2', graph=modelDefinition$neighborGraph, constr = TRUE, hyper=modelDefinition$hyper$local, replicate=replicateIdx))
          validLatentFieldColumns <- c(validLatentFieldColumns,'GEOIDRow')
        }
      } else {
        
        inputData$GEOIDRow <- match(inputData$GEOID,unique(inputData$GEOID))
        
        if('timeRow' %in% names(inputData)){
          
          inputData$timeRow_GEOID <- inputData$timeRow
          
          formula <- update(formula,  ~ . + f(GEOIDRow, model='iid', graph=modelDefinition$neighborGraph, hyper=modelDefinition$hyper$local, replicate=replicateIdx,
                                              group = timeRow_GEOID, control.group=list(model="rw2")))
          validLatentFieldColumns <- c(validLatentFieldColumns,'GEOIDRow','timeRow_GEOID')
        } else {
          formula <- update(formula,  ~ . + f(GEOIDRow, model='iid', graph=modelDefinition$neighborGraph, hyper=modelDefinition$hyper$local, replicate=replicateIdx))
          validLatentFieldColumns <- c(validLatentFieldColumns,'GEOIDRow')
        }
        
      }
    }
  }
  
  # linear combination of pathogen and latent fields

    lc.data <- data.frame(inputData[,names(inputData) %in% validLatentFieldColumns], replicateIdx = replicateIdx)
    lc.data <- lc.data[!duplicated(lc.data),]
    
    
    # generate list of desired linear combinations # https://groups.google.com/forum/#!topic/r-inla-discussion-group/_e2C2L7Wc30
    lcIdx=c()
    spentColumn<-rep(FALSE,length(validLatentFieldColumns))
    for(COLUMN in validLatentFieldColumns){
      
      if(COLUMN %in% c('pathogen') ){
        
        # need to promote pathogen levels to independent columns! https://groups.google.com/forum/#!topic/r-inla-discussion-group/IaTSakB7qy4
        pathogenNames <- paste('pathogen',levelSet,sep='')
        
      } else if (!(COLUMN == 'timeRow_PUMA5CE' )) {
        groupIdx<-grepl( paste0('_',gsub('Row','',COLUMN)) ,validLatentFieldColumns) 
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
    lc.latentField <- c()
    for(k in 1:nrow(lc.data)){
      w<-list()
      
      for(n in 1:length(names(lcIdx))){
        w[[n]]<-rep(0,nrow(lc.data))
        w[[n]][lcIdx[[n]][k]]<-1
      }
      
      A <- c(x=1, w)
      names(A) <- c(pathogenNames[lc.data$replicateIdx[k]],names(lcIdx))

      lc <- inla.make.lincomb(A)
      names(lc)<- paste0('latentField',k)
      lc.latentField<-c(lc.latentField,lc)
    }

  df <- data.frame(outcome = outcome, inputData, replicateIdx)
  
  modelDefinition <- list(type='latentField', family = family, formula = formula, lincomb = lc.latentField,
                          inputData = df, neighborGraph=neighborGraph, hyper=hyper, 
                          latentFieldData = inputData[rownames(inputData) %in% rownames(lc.data),],  # clean up formatting, but this will be useful for exporting latentField csv
                          queryList = db$queryList)

  return(modelDefinition)
}


#' appendLatentFieldData: internal function for adding model$summary.random to db$observedData from latentFieldModel fit
#'
#' @param model inla model object
#' @param db object from dbViewer with observedData tibble and query
#' @return db with added modeledData tibble
#' 
#' @import lubridate
#'
appendLatentFieldData <- function(model,db, family = 'poisson'){
  
  modeledData <- db$observedData
  
  if(family[1] == 'binomial'){
    modeledData$fraction <- modeledData$positive/modeledData$n
  }
  
  # summary.fitted.values
  nCol <- ncol(modeledData)
  modeledData[,nCol+1:ncol(model$summary.fitted.values)]<-model$summary.fitted.values
  names(modeledData)[nCol+1:ncol(model$summary.fitted.values)]<-paste('fitted.values',names(model$summary.fitted.values),sep='.')
  
  rownames(modeledData)<-c()
  
  # summary.random
  # TO-DO
  
  return(modeledData)
}