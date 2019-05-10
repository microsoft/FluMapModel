#' getHumanReadableModelIdFromModel: return human readable verion of model from query
library(logging)


#'
#' @param model INLA model object that will generatie id from
#'
#' @return Unique String representing model in human readable format
#' @export
#'
getHumanReadableModelIdFromModel <- function(model) {
  return (getHumanReadableModelIdFromQuery(getModelQueryObjectFromModel(model)))
}

#' getHumanReadableModelIdFromQuery: return human readable verion of model from query
#'
#' @param query query object container the observed and the model_type attributes
#'
#' @return Unique String representing model in human readable format
#' @export
#'
getHumanReadableModelIdFromQuery <- function(query) {
  props <- getModelQueryObjectFromQuery(query)
  result <- tolower(sprintf("%s-%s",
                            paste(props$model_type,collapse = "."),
                            paste(props$observed, collapse = ".")))
  return(result)
}

#' getModelQueryObjectFromModel: return query object from a model.
#' This is the object we use to generate our unique ids.
#'
#' @param model = Model object to get query object for
#' @param model_type = Model Type string. Default to inla
#' @param latent = Bool determing if we are saving a latent model or a smooth model
#'
#' @return An object containing the observed and the model_type fields
#' @export
#'
getModelQueryObjectFromModel<- function(model, model_type = 'inla', latent = FALSE) {

  result <- newEmptyObject()
  if (latent) {
    result$model_type <- jsonlite::unbox(paste(model_type, "latent", collapse = "_"))
    result$observed <- sort(colnames(model$modelDefinition$latentFieldData))

  }
  else {
    result$model_type <- jsonlite::unbox(model_type)
    result$observed <- sort(colnames(model$modelDefinition$observedData))
  }

  return(result)
}
#' getModelQueryObjectFromQuery: Reformate a query object to ensure it is in proper order
#' before generating id.
#'
#' @param query query object container the observed and the model_type attributes
#'
#' @import logging
#'
#' @return An object containing the observed and the model_type fields
#' @export
#'
getModelQueryObjectFromQuery <- function(query) {
  basicConfig()
  setLevel("FINEST")

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
#' @param query query object container the observed and the model_type attributes
#'
#' @import digest
#' @import logging
#' @importFrom jsonlite toJSON
#' 
#' @export
#'
getModelIdFromQuery <- function(query) {
  basicConfig()
  setLevel("FINEST")

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
#'
#' @export
#'
saveModel <- function(model, modelStoreDir =  Sys.getenv('MODEL_STORE', '/home/rstudio/seattle_flu/test_model_store')) {
  basicConfig()
  setLevel("FINEST")
  ts <- Sys.time()
  attr(ts, "tzone") <- 'UTC'
  ts <- paste0(as.character(ts), 'Z')
  
  # we always dump to our directory. We then use the python upload script to post
  # trained models to production
  modelDBfilename <- paste(modelStoreDir, '/', 'modelDB.tsv', sep = '')

  # create an id that is predictable based on the query the produced the model
  name <- getHumanReadableModelIdFromModel(model)
  modelQuery <- getModelQueryObjectFromModel(model)
  modelId <- getModelIdFromQuery(modelQuery)
  # extract json in sorted order
  # with only fields that matter
  filename <-modelId

  #ensure our model store directory exists
  dir.create(modelStoreDir, showWarnings = FALSE)
  
  # all models output inla
  newRow <- data.frame(
    filename = filename,
    name = name,
    queryJSON = as.character(jsonlite::toJSON(modelQuery)),
    type = 'inla',
    created = ts
  )

  loginfo("Saving RDS")
  outfile <- xzfile(paste(modelStoreDir, '/', filename, '.RDS', sep = ''), 'wb', compress=9, encoding = 'utf8')
  saveRDS(model,file = outfile)
  close(outfile)
  

  loginfo("Saving smooth model")
  # all models output smooth
  newRow$latent <- FALSE
  write.csv(
    model$modeledData,
    paste(modelStoreDir, '/', filename, '.csv', sep = ''),
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
      type = 'inla_latent',
      created = ts
    )
    newRow$latent <- TRUE
    
    print("Saving latent model")

    write.csv(
      model$latentField,
      paste(modelStoreDir, '/', filename, '.csv', sep = ''),
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



