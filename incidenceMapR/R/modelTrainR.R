#' modelTrainR: function for training INLA models from modelDefintion objects
#'
#' @param modelDefinition modelDefinition object from one of smoothModel.R, latentFieldModel.R, or effectModel.R
#' @return observedData table that has been prepared for defineModels.R
#'
#' @import INLA
#'
#' @export
#' @examples
#' return h1n1pdm incidence model by time and location
#'    db <- modelTrainR(db = dbViewR::selectFromDB(), shp = dbViewR::masterSpatialDB())
#'
modelTrainR <- function(modelDefinition){
  
  # run model
  model <- inla(formula = modelDefinition$formula,
                family = modelDefinition$family, 
                data = modelDefinition$inputData, 
                lincomb = modelDefinition$lincomb,
                Ntrials = modelDefinition$inputData$n,
                control.predictor=list(compute=TRUE,link=1),
                control.compute=list(config=TRUE,dic=TRUE),verbose = TRUE,
                control.inla=list(int.strategy="eb", strategy = "gaussian"))
  
  # format output
  if(modelDefinition$type =='smooth'){
    modeledData <- appendSmoothData(model,db, family = modelDefinition$family)
  } else if (modelDefinition$type == 'latentField'){
    modeledData <- appendLatentFieldData(model,db, family = modelDefinition$family)
  } else if (modelDefinition$type == 'effects'){
    
  }
  
  # return output data
  return(list(modeledData = modeledData, inla = model, modelDefinition = modelDefinition))
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
saveModel <- function(model, cloudDir = 'C:/Users/mfamulare/Dropbox (IDM)/SeattleFlu-incidenceMapR/models'){
  # https://www.dropbox.com/sh/5loj4x6j4tar17i/AABy5kP70IlYtSwrePg4m44Ca?dl=0
  
  ts <- as.character(Sys.time())
  filename <- digest::digest(paste(digest::digest(model$modeledData),ts,sep=''))
                             
  saveRDS(model,paste(cloudDir,'/',filename,'.RDS',sep=''))
  write.csv(model$modeledData,paste(cloudDir,'/',filename,'.csv',sep=''),row.names = FALSE,quote = FALSE)

  # register in modelDB
  newRow <- list(filename=filename,
                 queryJSON=as.character(jsonlite::toJSON(model$modelDefinition$queryList)),
                 type = model$modelDefinition$type,
                 created = ts)
  modelDBfilename<-paste(cloudDir,'/','modelDB.tsv',sep='')
  if(!file.exists(modelDBfilename)){
    write.table(newRow,file=modelDBfilename,sep='\t',row.names = FALSE, col.names = TRUE,quote = FALSE)
  } else {
    write.table(newRow,file=modelDBfilename,sep='\t',row.names = FALSE, col.names = FALSE,quote = FALSE, append=TRUE)
  }

}

