#' modelTrainR: function for training poisson and logistic regression models on data from dbViewR
#'
#' @param family regression model family (binomial (default) or poisson)
#' @param db dbViewR object with valid column names for INLA model
#' @param shp sf object with GEOID shapes (all higher levels assume iid and not local smoothing)
#' @return observedData table that has been prepared for defineModels.R
#'
#' @import INLA
#' @import dbViewR
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom lazyeval interp
#'
#' @export
#' @examples
#' return h1n1pdm incidence model by time and location
#'    db <- modelTrainR(db = dbViewR::selectFromDB(), shp = dbViewR::masterSpatialDB())
#'
modelTrainR <- function(family = 'binomial', db = dbViewR::selectFromDB(), shp = dbViewR::masterSpatialDB(),
                        formula = NULL, inputData = NULL, neighborGraph = NULL){
  library(INLA)
  
  # if formula and inputData (and neighborGraph if required) isn't passed,
  # create formula from table
  if(is.null(formula) & is.null(inputData) ){
    #INLA data frame that may get augmented columns we don't need to see when we're done
    inputData <- db$observedData
    
    # formula should be moved to it's own function!
    
    # build formula
    formula <- positive ~ 1

    # construct priors
    global.effect.hyper <- list(prec = list( prior = "pc.prec", param = 0.2, alpha = 0.01))
    local.effect.hyper <- list(prec = list( prior = "pc.prec", param = 0.05, alpha = 0.01))
    age.effect.hyper <- list(prec = list( prior = "pc.prec", param = 0.05, alpha = 0.01))
    
    
    # build formula
    for(COLUMN in names(inputData)[!(names(inputData) %in% c('positive','n'))]){
      
      
      # factors: might do this differently!
      if(COLUMN %in% c('fluShot','sex','hasFever','hasCough','hasMyalgia')){
        formula <- update(formula, paste("~ . +",COLUMN))
      }
      
      
      samplingLocation = unique(linelist$observedData$samplingLocation)
      
      # offset 
      if(COLUMN %in% c('offset')){
        formula <- update(formula, paste("~ . + offset(",COLUMN,')'))
      }
      
      # random effects
      if(COLUMN == 'timeRow'){
        
        #INLA needs one column per random effect
        inputData$timeRow_rw2 <- inputData$timeRow
        inputData$timeRow_IID <- inputData$timeRow
        
        formula <- update(formula,  ~ . + f(timeRow_rw2, model='rw2', hyper=global.effect.hyper) +
                            f(timeRow_IID, model='iid', hyper=local.effect.hyper) )
      }
      
      if(COLUMN == 'ageRow'){
        
        #INLA needs one column per random effect
        inputData$ageRow_rw2 <- inputData$ageRow
        inputData$ageRow_IID <- inputData$ageRow
        
        formula <- update(formula,  ~ . + f(ageRow_rw2, model='rw2', hyper=age.effect.hyper) +
                            f(ageRow_IID, model='iid', hyper=local.effect.hyper) )
      }
      
      if(COLUMN == 'PUMA5CE'){
        
        inputData$PUMA5CERow <- match(inputData$PUMA5CE,unique(inputData$PUMA5CE))
        
        if('timeRow' %in% names(inputData)){
          
          inputData$timeRow_PUMA5CE <- inputData$timeRow
          
          formula <- update(formula,  ~ . + f(PUMA5CERow, model='iid', hyper = global.effect.hyper, constr = TRUE,
                                              group = timeRow_PUMA5CE, control.group=list(model="rw2")))
        } else {
          
          formula <- update(formula,  ~ . + f(PUMA5CERow, model='iid', hyper = global.effect.hyper))
        }
      }
      
      # this needs to flexibly handle the level based on the provided shp!
      if(COLUMN == 'GEOID'){
        if(exists('shp')){
          neighborGraph <- constructAdjacencyNetwork(shp) 
          inputData$GEOIDRow <- shp$rowID[match(inputData$GEOID,shp$GEOID)]
          
          if('timeRow' %in% names(inputData)){
            
            inputData$timeRow_GEOID <- inputData$timeRow
            
            formula <- update(formula,  ~ . + f(GEOIDRow, model='besag', graph=neighborGraph, constr = TRUE, hyper=local.effect.hyper,
                                                group = timeRow_GEOID, control.group=list(model="rw2")))
          } else {
            formula <- update(formula,  ~ . + f(GEOIDRow, model='bym2', graph=neighborGraph, constr = TRUE, hyper=local.effect.hyper))
          }
        } else {
          
          inputData$GEOIDRow <- match(inputData$GEOID,unique(inputData$GEOID))
          
          if('timeRow' %in% names(inputData)){
            
            inputData$timeRow_GEOID <- inputData$timeRow
            
            formula <- update(formula,  ~ . + f(GEOIDRow, model='iid', graph=neighborGraph, hyper=local.effect.hyper,
                                                group = timeRow_GEOID, control.group=list(model="rw2")))
          } else {
            formula <- update(formula,  ~ . + f(GEOIDRow, model='iid', graph=neighborGraph, hyper=local.effect.hyper))
          }
          
        }
      }
      
    }
  }

  # run model
  model <- inla(formula = formula,family = family, data = inputData, Ntrials = inputData$n,
                control.predictor=list(compute=TRUE,link=1),
                control.compute=list(config=TRUE,dic=TRUE),verbose = TRUE,
                control.inla=list(int.strategy="eb", strategy = "gaussian"))
  
  # format output
  # this needs to also output latent field!
  modeledData <- appendModelData(model,db)
  
  # return output data
  return(list(queryList = db$queryList, modeledData = modeledData, inla = model,formula = formula))
}


#' constructAdjacencyNetwork:  function for constructing adjacency graph for INLA GMRF from shapefile
#'
#' @param shp sf object with polygon geometry
#' @return neighborGraph in INLA format
#'
#' @importFrom INLA inla.read.graph
#' @import dbViewR
#' @importFrom spdep poly2nb nb2INLA
#' @export
#'
#' @examples
#' return neighbor census tract adjacency network for King County, WA
#'    neighborGraph <- constructAdjacencyNetwork( shp = dbViewR::masterSpatialDB() )
#'
constructAdjacencyNetwork <- function( shp ){
  
  #Create adjacency matrix
  adj <- spdep::poly2nb(as(shp,"Spatial"))
  
  # TODO: add edges for islands
  # TODO: allow alternate defintions of neighbor
  
  #Convert the adjacency matrix into a file in the INLA format
  spdep::nb2INLA("shp.adj", adj)
  
  #read in INLA graph
  neighborGraph <- INLA::inla.read.graph("shp.adj")
  
  # delete file
  unlink("shp.adj")
  
  return(neighborGraph)
  
}

#' appendModelData: internal function for adding model$summary.fitted.values to db
#'
#' @param db object from dbViewer with observedData tibble and query
#' @param model inla model object
#' @return db with added modeledData tibble
#'
appendModelData <- function(model,db){
  # this needs to also output latent fields.
  # which requires some science formatting depending on what the fields are...
  # these are important as the may represent sampling-adjusted incidence that we want to show!
  
  modeledData <- db$observedData

  # summary.fitted.values
  nCol <- ncol(db$observedData)
  modeledData[,nCol+1:ncol(model$summary.fitted.values)]<-model$summary.fitted.values
  names(modeledData)[nCol+1:ncol(model$summary.fitted.values)]<-paste('fitted.values',names(model$summary.fitted.values),sep='.')
  
  # latent fields
  latentFields<-names(model$summary.random)
  for(FIELD in latentFields){
    nCol <- ncol(db$observedData)
    # format depending on PUMAS and GEOID or time or bym2 vs besag vs IID.....
  }
  
  
  rownames(modeledData)<-c()
  return(modeledData)
}

#' saveModel: function to save models and register them in modelDB.csv
#'
#' @param model INLA object
#' @param db dbViewR object
#'
#' @import digest
#' @importFrom jsonlite toJSON
#' 
#' @export
#'
saveModel <- function(modelList, cloudDir = 'C:/Users/mfamulare/Dropbox (IDM)/SeattleFlu-incidenceMapR/models/'){
  # https://www.dropbox.com/sh/5loj4x6j4tar17i/AABy5kP70IlYtSwrePg4m44Ca?dl=0
  
  # this needs to also output latent field!
  ts <- as.character(Sys.time())
  filename <- digest::digest(paste(digest::digest(modelList$modeledData),ts,sep=''))
                             
  saveRDS(modelList$inla,paste(cloudDir,filename,'.RDS',sep=''))
  write.csv(modelList$modeledData,paste(cloudDir,filename,'.csv',sep=''),row.names = FALSE)

  newRow <- list(filename=filename,queryJSON=as.character(jsonlite::toJSON(modelList$queryList)), created = ts)
  modelDBfilename<-paste(cloudDir,'modelDB.tsv',sep='')
  if(!file.exists(modelDBfilename)){
    write.table(newRow,file=modelDBfilename,sep='\t',row.names = FALSE, col.names = TRUE)
  } else {
    write.table(newRow,file=modelDBfilename,sep='\t',row.names = FALSE, col.names = FALSE, append=TRUE)
  }

}

