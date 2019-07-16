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
                control.inla=list(int.strategy="auto", strategy = "gaussian"))
  
  # format output
  if(modelDefinition$type =='smooth'){
    
    modeledData <- appendSmoothData(model,modelDefinition)
    
    # return output data
    return(list(modeledData = modeledData, inla = model, modelDefinition = modelDefinition))
  
  } else if (modelDefinition$type == 'latent_field'){
    
    modeledDataList <- appendLatentFieldData(model,modelDefinition)
    
    # return output data
    return(list(modeledData = modeledDataList$modeledData, latentField = modeledDataList$latentField,
                inla = model, modelDefinition = modelDefinition))
    
  } else if (modelDefinition$type == 'effects'){
    #same as smooth data for now?
    
    modeledData <- appendSmoothData(model,modelDefinition)
    
    # return output data
    return(list(modeledData = modeledData, inla = model, modelDefinition = modelDefinition))
    
  }
  
 
}



