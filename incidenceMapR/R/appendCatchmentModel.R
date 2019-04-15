#' appendCatchmentModel: function for adding catchment covariate estimated by smoothModel to dbViewR object for use in latentFieldModel
#' 
#' @param db object from dbViewer with observedData tibble and query
#' @param shp shapefile object from masterShapeDB
#' @return db with added catchment column to observedData
#' 
#' @import INLA
#' @import dbViewR
#' @import magrittr
#' @import dplyr
#'
#' @export
#'
appendCatchmentModel <- function(db,shp = NULL){
  
  validGeoLevels <- c('PUMA5CE','CRA_NAME','NEIGHBORHOOD_DISTRICT_NAME','GEOID')
  geo <- validGeoLevels[validGeoLevels %in% names(db$observedData)]
  
  # get pathogen list
  queryIn <- list(
    SELECT   =list(COLUMN=c('pathogen')),
    GROUP_BY =list(COLUMN=c('pathogen')),
    SUMMARIZE=list(COLUMN='pathogen', IN= 'all')
  )
  pathogens <- unique(selectFromDB(  queryIn )$observedData$pathogen)
  
  # find catchment maps for each sampling_location and geoLevel
  # catchment represented by all samples not from target virus. Idea being participation due to other pathogens is assumed
  # to be uncorrelated with pathogen of interest. Social dynamics could violate this assumption.
  queryIn <- list(
    SELECT   =list(COLUMN=c('pathogen','sampling_location',geo)),
    WHERE    =list(COLUMN=c('pathogen'), IN = setdiff(pathogens, unique(db$observedData$pathogen))),
    GROUP_BY =list(COLUMN=c('sampling_location',geo)),
    SUMMARIZE=list(COLUMN='sampling_location', IN= 'all')
  )
  catchmentDb <- expandDB( selectFromDB(  queryIn ) )
  
  # at some point, we maybe should check if the catchment map is already saved, 
  # although this is a cheap computation relative to everything else, so that may never matter
  
  catchmentModelDefinition <- smoothModel(db=catchmentDb, shp=shp)
  catchmentModel <- modelTrainR(catchmentModelDefinition)
  
  # append catchment as intercept covariate
  db$observedData <- db$observedData %>% left_join(catchmentModel$modeledData %>% select(sampling_location, geo, fitted_values_0_5quant))
  names(db$observedData)[names(db$observedData) %in% 'fitted_values_0_5quant'] <- 'catchment'
  db$observedData$catchment <- log(db$observedData$catchment)
  db$observedData$catchment <- (db$observedData$catchment - mean(db$observedData$catchment))/sd(db$observedData$catchment)
  
  return(db)
}
