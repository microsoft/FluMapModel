library(logging)
basicConfig()
setLevel(10)

#' getHumanReadableModelIdFromModel: return human readable verion of model from query
#'
#' @param model INLA model object contaomomg the modelDefinition$queryList properties
#' 
#' @export
#'
getHumanReadableModelIdFromModel <- function(model) {
  return (getHumanReadableModelIdFromQuery(model$modelDefinition$queryList))
}

#' getHumanReadableModelIdFromQuery: return human readable verion of model from query
#'
#' @param query query object container $SELECT and $GROUP_BY propertiers 
#' 
#' @export
#'
getHumanReadableModelIdFromQuery <- function(query) {
  props <- getModelQueryObjectFromQuery(query)
  # pathogen_geo_resolution_sequential_variable_outcome
  result <- tolower(sprintf("%s-%s", 
                            paste(props$select, collapse = "."), 
                            paste(props$groupby,collapse = ".")))
  return(result)
}


getModelQueryObjectFromModel<- function(model) {
  # maybe  we should do something more like
  # m <- getModelQueryObjectFromQuery(model$modelDefinition$queryList)
  # m$type <- model$modelDefinition$type ?
  return (getModelQueryObjectFromQuery(model$modelDefinition$queryList))
}
#' getModelQueryObjectFromQuery: return a model query object with just the fields that make up the unique id
#'
#' @param query query object container $SELECT and $GROUP_BY propertiers 
#' 
#' @export
#'
getModelQueryObjectFromQuery <- function(query) {
  logdebug("Src:", str(query))
  result <- newEmptyObject()
  # when building the query object, we only care aboout the select and group by
  # since those are the fields that drive uniqueness of model
  # the where fields are filters are data that we could then later use to filter
  # outputs or query but not in the id
  
  if (!is.null(query$GROUP_BY$COLUMN)) {
    fields <- c()
    for (p in query$GROUP_BY$COLUMN) {
      props <- fields
      fields <- append(props, p)
    }
    result$groupby <- sort(fields)
  }
  
  if (!is.null(query$SELECT$COLUMN)) {
    fields <- c()
    for (p in query$SELECT$COLUMN) {
      props <- fields
      fields <- append(props, p)
    }
    result$select <- sort(fields)
  }
  
  #if (!is.null(query$type)) {
  #  props$type <- query$type
  #}
  logdebug("Result:", str(result))
  return(result)
}

#' getModelIdFromModel: function to get model id from a model objejct
#'
#' @param model INLA object
#' 
#' @export
#'
getModelIdFromModel <- function(model) {
  return(getModelIdFromQuery(model$modelDefinition$queryList))
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
  props <- getModelQueryObjectFromQuery(query)
  modelId <- as.character(jsonlite::toJSON(props))
  modelId <- digest::digest(modelId)
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
  
  # create an id that is predictable based on the query the produced the model
  name <- getHumanReadableModelIdFromModel(model)
  modelId <- getModelIdFromModel(model)
  # extract json in sorted order
  # with only fields that matter
  filename <-modelId

  
  # all models output inla
  newRow <- data.frame(
    filename = filename,
    name = name,
    queryJSON = as.character(jsonlite::toJSON(getModelQueryObjectFromModel(model))),
    type = 'inla',
    created = ts
  )
  
  newRow$rds <- TRUE
  outfile <- xzfile(paste(cloudDir, '/', filename, '.RDS', sep = ''), 'wb', compress=9, encoding = 'utf8')
  saveRDS(model,file = outfile)
  close(outfile)
  
  
  # all models output smooth
  newRow$smooth <- TRUE
  write.csv(
    model$modeledData,
    paste(cloudDir, '/', filename, '.csv', sep = ''),
    row.names = FALSE,
    quote = FALSE
  )
  
  # If we have a latent_field type, write out that csv
  newRow$latent_field <- model$modelDefinition$type == 'latent_field'
  if (model$modelDefinition$type == 'latent_field') {
    write.csv(
      model$latentField,
      paste(cloudDir, '/', filename, '.latent_field.csv', sep = ''),
      row.names = FALSE,
      quote = FALSE
    )
  }
  

  # we always dump to our directory. We then use the python upload script to post 
  # trained models to production
  modelDBfilename <- paste(cloudDir, '/', 'modelDB.tsv', sep = '')
  write.table(
    newRow, file = modelDBfilename, sep = '\t', row.names = FALSE, col.names = !file.exists(modelDBfilename),
    quote = FALSE, append = file.exists(modelDBfilename)
  )
  
  
}


