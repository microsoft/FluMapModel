#' function for getting modeled incidence and observations used for training
#'
#' This function loads a cached model object containing two datasets
#'   (1) observed incidence of flu in ILI
#'   (2) modeled incidence after statistical smoothing
#'
#' @param query JSON string with description of output data requested from model
#' @return A tibble with requested data, formatted to translate nicely to hierarchical json
#' @import magrittr
#' @importFrom dplyr group_by_at
#' @importFrom tidyr nest
#' @export
#' @examples
#' query <- jsonlite::toJSON(  list(output='incidence', attributes=c('Census_Tract','vax_status'), basis=c('year'), values=c('incidence_median','incidence_sd') ))
#' predictModel(query)
#'

predictModel <- function(query){

  # model object is stared in R/sysdata.rda and is only available to functions in the package (and doesn't need to be explicitly loaded)

  if(query$output == "incidence"){
    return(reformatForJSON(model$incidence,query))
  } else if (query$output == "observed"){
    return(reformatForJSON(model$observed,query))
  } else {
    return(list(type = 'error', message = 'invalid output request!'))
  }
}

reformatForJSON <- function(dataIn,query){

  dataOut<-query
  dataOut$data <-dataIn %>% group_by_at(query$attributes) %>% nest(query$basis,query$values,.key=data)

  return(dataOut)
}
