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
  
  # find catchment maps for each samplingLocation and geoLevel
  queryIn <- list(
    SELECT   =list(COLUMN=c('samplingLocation',geo)),
    GROUP_BY =list(COLUMN=c('samplingLocation',geo)),
    SUMMARIZE=list(COLUMN='samplingLocation', IN= 'all')
  )
  catchmentDb <- expandDB( selectFromDB(  queryIn ) )
  
  # at some point, we maybe should check if the catchment map is already saved, 
  # although this is a cheap computation relative to everything else, so that may never matter
  
  catchmentModelDefinition <- smoothModel(db=catchmentDb, shp=shp)
  catchmentModel <- modelTrainR(catchmentModelDefinition)
  
  saveModel(catchmentModel)
  
  # append catchment as intercept covariate
  db$observedData <- db$observedData %>% left_join(catchmentModel$modeledData %>% select(samplingLocation, geo, fitted.values.0.5quant))
  names(db$observedData)[names(db$observedData) %in% 'fitted.values.0.5quant'] <- 'catchment'
  db$observedData$catchment <- log(db$observedData$catchment)
  db$observedData$catchment <- (db$observedData$catchment - mean(db$observedData$catchment))/sd(db$observedData$catchment)
  
  return(db)
}
