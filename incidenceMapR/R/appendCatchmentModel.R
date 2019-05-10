#' appendCatchmentModel: function for adding catchment covariate estimated by smoothModel to dbViewR object for use in latentFieldModel
#' 
#' @param db object from dbViewer with observedData tibble and query
#' @param shp shapefile object from masterShapeDB
#' @param source = 'simulated_data' (default) or 'production
#' @param na.rm = TRUE (default) or FALSE.  Remove NA pathogens
#' @return db with added catchment column to observedData
#' 
#' @import INLA
#' @import dbViewR
#' @import magrittr
#' @import dplyr
#'
#' @export
#'
appendCatchmentModel <- function(db,shp = NULL, source='simulated_data', na.rm=TRUE){
  
  validGeoLevels <- c('residence_puma','residence_cra_name','residence_neighborhood_district_name','residence_census_tract','residence_city',
                      'work_puma','work_cra_name','work_neighborhood_district_name','work_census_tract','work_city')
  geo <- validGeoLevels[validGeoLevels %in% names(db$observedData)]
  
  # get pathogen list
  queryIn <- list(
    SELECT   =list(COLUMN=c('pathogen')),
    GROUP_BY =list(COLUMN=c('pathogen')),
    SUMMARIZE=list(COLUMN='pathogen', IN= 'all')
  )
  pathogens <- unique(selectFromDB(  queryIn, source=source, na.rm=na.rm)$observedData$pathogen)
  
  # find catchment maps for each site_type and geoLevel
  # catchment represented by all samples not from target virus. Idea being participation due to other pathogens is assumed
  # to be uncorrelated with pathogen of interest. Social dynamics could violate this assumption.
  
  # making timeseries inference work for pathogen == 'all even if it maybe shouldn't...
    outGroup<-setdiff(pathogens, unique(db$observedData$pathogen))
    if (length(outGroup)==0){
      outGroup = 'all'
    }
  
  queryIn <- list(
    SELECT   =list(COLUMN=c('pathogen','site_type',geo)),
    WHERE    =list(COLUMN=c('pathogen'), IN = outGroup),
    GROUP_BY =list(COLUMN=c('site_type',geo)),
    SUMMARIZE=list(COLUMN='site_type', IN= 'all')
  )
  catchmentDb <- expandDB( selectFromDB(  queryIn, source=source, na.rm=na.rm ), shp=shp )
  
  # at some point, we maybe should check if the catchment map is already saved, 
  # although this is a cheap computation relative to everything else, so that may never matter
  
  catchmentModelDefinition <- smoothModel(db=catchmentDb, shp=shp)
  catchmentModel <- modelTrainR(catchmentModelDefinition)
  
  # append catchment as intercept covariate
  db$observedData <- db$observedData %>% left_join(catchmentModel$modeledData %>% select(site_type, geo, modeled_count_0_5quant))
  names(db$observedData)[names(db$observedData) %in% 'modeled_count_0_5quant'] <- 'catchment'
  db$observedData$catchment <- log(db$observedData$catchment) - mean(log(db$observedData$catchment))
  # db$observedData$catchment <- db$observedData$catchment/sd(db$observedData$catchment)
  
  return(db)
}
