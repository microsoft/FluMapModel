library(logging)
basicConfig()
setLevel("FINEST")

#' getHumanReadableModelIdFromModel: return human readable verion of model from query
#'
#' @param model INLA model object contaomomg the modelDefinition$queryList properties
#' 
#' @export
#'
getHumanReadableModelIdFromModel <- function(model) {
  return (getHumanReadableModelIdFromQuery(getModelQueryObjectFromModel(model)))
}

#' getHumanReadableModelIdFromQuery: return human readable verion of model from query
#'
#' @param query query object container $SELECT and $GROUP_BY propertiers 
#' 
#' @export
#'
getHumanReadableModelIdFromQuery <- function(query) {
  props <- getModelQueryObjectFromQuery(query)
  result <- tolower(sprintf("%s-%s", 
                            paste(props$type,collapse = "."),
                            paste(props$observed, collapse = ".")))
  return(result)
}


getModelQueryObjectFromModel<- function(model, latent = FALSE) {
  # maybe  we should do something more like
  # m <- getModelQueryObjectFromQuery(model$modelDefinition$queryList)
  # m$type <- model$modelDefinition$type ?
  
  result <- newEmptyObject()
  if (latent) {
    result$model_type <- jsonlite::unbox("latent")
    result$observed <- sort(colnames(model$modelDefinition$latentFieldData))
    
  }
  else {
    result$model_type <- jsonlite::unbox("smooth")
    result$observed <- sort(colnames(model$modelDefinition$observedData))
  }
  
  return(result)
}
#' getModelQueryObjectFromQuery: return a model query object with just the fields that make up the unique id
#'
#' @param query query object container $SELECT and $GROUP_BY propertiers 
#' 
#' @export
#'
getModelQueryObjectFromQuery <- function(query) {
  logdebug("getModelQueryObjectFromQuery Src:", str(query))
  logdebug("$observed", attr(query, "observed"))
  result <- newEmptyObject()
  result$observed <- sort(query$observed)
  result$model_type <- query$model_type
  logdebug("getModelQueryObjectFromQuery result:", str(result))
  return(result)
}

#' getModelIdFromModel: function to get model id from a model objejct
#'
#' @param model INLA object
#' 
#' @export
#'
getModelIdFromModel <- function(model) {
  return(getModelIdFromQuery(getModelQueryObjectFromModel(model)))
}

#' getModelIdFromQuery: function to save models and register them in modelDB.csv
#'
#' @param query query object container $SELECT and $GROUP_BY propertiers 
#'
#' @import digest
#' @importFrom jsonlite toJSON
#' 
#' @export
#'
getModelIdFromQuery <- function(query) {
  #props <- getModelQueryObjectFromQuery(query)
  modelId <- as.character(jsonlite::toJSON(query, simplifyDataFrame=))
  logdebug("Model ID JSON:", jsonlite::toJSON(query, simplifyDataFrame=))
  modelId <- digest::digest(modelId, serialize=FALSE)
  logdebug("Model ID Hash:", modelId)
  return(modelId)
}

#' saveModel: function to save models and register them in modelDB.csv
#'
#' @param model INLA object
#' @param db dbViewR object
#' 
#' @export
#'
saveModel <- function(model, db = NULL, cloudDir =  Sys.getenv('MODEL_BIN_DIR', '/home/rstudio/seattle_flu')) {
  ts <- Sys.time()
  attr(ts, "tzone") <- 'UTC'
  ts <- paste0(as.character(ts), 'Z')
  
  # we always dump to our directory. We then use the python upload script to post 
  # trained models to production
  modelDBfilename <- paste(cloudDir, '/', 'modelDB.tsv', sep = '')
  
  # create an id that is predictable based on the query the produced the model
  name <- getHumanReadableModelIdFromModel(model)
  modelQuery <- getModelQueryObjectFromModel(model)
  modelId <- getModelIdFromQuery(modelQuery)
  # extract json in sorted order
  # with only fields that matter
  filename <-modelId

  
  # all models output inla
  newRow <- data.frame(
    filename = filename,
    name = name,
    queryJSON = as.character(jsonlite::toJSON(modelQuery)),
    type = 'inla',
    created = ts
  )
  
  print("Saving RDS")
  outfile <- xzfile(paste(cloudDir, '/', filename, '.RDS', sep = ''), 'wb', compress=9, encoding = 'utf8')
  saveRDS(model,file = outfile)
  close(outfile)
  
  
  print("Saving smooth model")
  # all models output smooth
  newRow$latent <- FALSE
  write.csv(
    model$modeledData,
    paste(cloudDir, '/', filename, '.csv', sep = ''),
    row.names = FALSE,
    quote = FALSE
  )
  write.table(
    newRow, file = modelDBfilename, sep = '\t', row.names = FALSE, col.names = !file.exists(modelDBfilename),
    quote = FALSE, append = file.exists(modelDBfilename)
  )
  
  # If we have a latent_field type, write out that csv
  if (model$modelDefinition$type == 'latent_field') {
    modelQuery <- getModelQueryObjectFromModel(model, TRUE)
    modelId <- getModelIdFromQuery(modelQuery)
    name <- getHumanReadableModelIdFromModel(model)
    filename <-modelId
    newRow <- data.frame(
      filename = filename,
      name = name,
      queryJSON = as.character(jsonlite::toJSON(modelQuery)),
      type = 'inla',
      created = ts
    )
    newRow$latent <- TRUE
    
    print("Saving latent model")
    
    write.csv(
      model$latentField,
      paste(cloudDir, '/', filename, '.csv', sep = ''),
      row.names = FALSE,
      quote = FALSE
    )
    
    # write to our model db file
    write.table(
      newRow, file = modelDBfilename, sep = '\t', row.names = FALSE, col.names = !file.exists(modelDBfilename),
      quote = FALSE, append = file.exists(modelDBfilename)
    )
  }
}


