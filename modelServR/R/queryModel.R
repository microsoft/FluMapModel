#' queryModelById function for getting modeled data by a known id
#'
#' This function loads a cached model object
#'
#' This function is intended for use by the API server. 
#' @param modelId modelId to load
#' @param outputFile File to save the query result to
#' @param queryFile Path to JSON query to filter the model data for. 
#' @param format = "csv" (default: pre-defined data-only output), "json" (pre-defined data-only output), or "model" (incidenceMapR model object)
#' @param outputDir = directory where to save result to
#'
#' @return TRUE if output was written to outputFile
#'
#' @import jsonlite
#' @export
#' @examples
#'
queryModelById <-
  function(modelId,
           outputFile,
           queryFile = NULL,
           format = "csv",
           outputDir = Sys.getenv('WORKER_DIR', '/tmp')) {
    db <- loadModelFileById(modelId, data_dir)
    return(queryLoadedModel(model, queryFile, outputFile, format, outputDir))
  }

#' queryLoadedModel Function querys a model that is already loaded. Used by API
#' @param model modelId to query
#' @param outputFile File to save the query result to
#' @param queryFile Path to JSON query to filter the model data for. 
#' @param format = "csv" (default: pre-defined data-only output), "json" (pre-defined data-only output), or "model" (incidenceMapR model object)
#' @param outputDir = directory where to save result to
#'
#' @return TRUE if output was written to outputFile
#'
#' @import jsonlite
#' @export
#' @examples
#'
queryLoadedModel <-
  function(model,
           outputFile,
           format = "csv",
           queryFile = NULL,
           outputDir = Sys.getenv('WORKER_DIR', '/tmp')) {
    # create lock file so server knows we are fetching model
    lock_file = file.path(outputDir, paste(outputFile,'.lock', sep=""))
    file.create(lock_file)
    if(!is.null(queryFile)) {
      # load the query from the specified fiel
      query <-
        jsonlite::read_json(file.path(outputDir, queryFile))
      # perform our query here
      # at moment we do NO filtering just return the full model
    }
    
    #the write our result
    if (format == "csv") {
      write.csv(
        model,
        file.path(outputDir, outputFile),
        row.names = FALSE,
        quote = FALSE
      )
    } else if (format == 'json') {
      
      jsonlite::write_json(model, file.path(outputDir, outputFile))
    }
    file.remove(lock_file)
    return(TRUE)
  }