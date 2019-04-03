#' expandDB: function to expand database to fill in unobserved elements for prediction/interpolation
#'
#' @param db tibble with valid column names for INLA model
#' @return db tibble with filled in NaNs or Zeros as required
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom lazyeval interp
#'
#' @export
#' @examples
#'
expandDB <- function( db = dbViewR::selectFromDB(), 
                      linelist = dbViewR::selectFromDB(db$queryList[names(db$queryList) %in% c("SELECT","WHERE","MUTATE")]),
                      shp = dbViewR::masterSpatialDB() ){
  
  # bounded columns
  validColumnData <- list(
    num_date = unique(linelist$observedData$num_date),
    samplingLocation = unique(linelist$observedData$samplingLocation),
    sex = unique(linelist$observedData$sex),
    fluShot = unique(linelist$observedData$fluShot),
    age = unique(linelist$observedData$age),
    hasFever = unique(linelist$observedData$hasFever),
    hasCough = unique(linelist$observedData$hasCough),
    hasMyalgia = unique(linelist$observedData$hasMyalgia),
    GEOID = shp$GEOID,
    CRA_NAME = unique(shp$CRA_NAM),
    NEIGHBORHOOD_DISTRICT_NAME = unique(shp$NEIGHBO),
    PUMA5CE = unique(shp$PUMA5CE),
    pathogen = unique(linelist$observedData$pathogen)
  ) 
  
  # don't expand on nested shape variables
  if ('GEOID' %in% names(db$observedData) ){
    nestedVariables <- list(CRA_NAME = "GEOID", NEIGHBORHOOD_DISTRICT_NAME = "GEOID", PUMA5CE = "GEOID")
  } else if ('CRA_NAME' %in% names(db$observedData) & !('GEOID' %in% names(db$observedData)) ){
    nestedVariables <- list(NEIGHBORHOOD_DISTRICT_NAME = "CRA_NAME", PUMA5CE = "GEOID")
  } else if ('NEIGHBORHOOD_DISTRICT_NAME' %in% names(db$observedData) & !any(c('CRA_NAME','GEOID') %in% names(db$observedData)) ){
    nestedVariables <- list(PUMA5CE = "GEOID")
  } else {
    nestedVariables <- list()
  }
  
  # transformations
  if(any(grepl('num_date',names(db$observedData))) & !any(grepl('timeBin',names(db$observedData)))){
    db$observedData$timeBin <- floor((db$observedData$num_date)*52)/52
    timeBin <- floor((validColumnData$num_date)*52)/52
    validColumnData$timeBin <- seq(min(timeBin),max(timeBin)+4/52,by= 1/52) 
  }
  if(any(grepl('age',names(db$observedData))) & !any(grepl('ageBin',names(db$observedData)))){
    db$observedData$ageBin <- pmin(floor(db$observedData$age),90)
    validColumnData$ageBin <- seq(0,90,by=1)
  }
  
  # expand.grid for non-nested variables
  colIdx <- ( names(validColumnData) %in% names(db$observedData) ) & 
    !( names(validColumnData) %in% names(nestedVariables)) &
    !( names(validColumnData) %in% c('num_date','age'))
  tmp<-expand.grid(validColumnData[colIdx])
                                   
  
  # join
  db$observedData <- dplyr::left_join(tmp,db$observedData, by=names(validColumnData)[colIdx])
  
  # sample size as zero instead of NaN
  db$observedData$n[is.na(db$observedData$n)]<-0
    
    # positives as 0 instead of NaN when positive count is total count always (eg catchments) 
    idx <- !is.na(db$observedData$positive)
    if(all(db$observedData$positive[idx]==db$observedData$n[idx])){
      db$observedData$positive[!idx]<-0
    }
  
  # age and num_date
    if(any(names(db$obervedData) == 'ageBin' )){
      db$observedData$age <- db$observedData$ageBin
    }
    
    if(any(names(db$obervedData) == 'timeBin' )){
      db$observedData$num_date <- db$observedData$timeBin
    }
    
  # nested variables
  if(length(nestedVariables) > 0) {
    colIdx <- which(( names(validColumnData) %in% names(db$observedData) ) & ( names(validColumnData) %in% names(nestedVariables) ) )
    for( k in colIdx){
      colName <- names(validColumnData)[k]
      db$observedData[[colName]] <- shp[[colName]][match(db$observedData[[nestedVariables[[colName]]]],as.character(shp[[nestedVariables[[colName]]]]))]
    }
  }
  
  # row indices for INLA
  validColumnData$timeRow <- 1:length(validColumnData$timeBin)
  validColumnData$ageRow <- 1:length(validColumnData$ageBin)
  
  if(any(grepl('timeBin',names(db$observedData)))){
    db$observedData$timeRow <- validColumnData$timeRow[match(db$observedData$timeBin,validColumnData$timeBin)]
  }
  if(any(grepl('ageBin',names(db$observedData)))){
    db$observedData$ageRow <- validColumnData$ageRow[match(db$observedData$ageBin,validColumnData$ageBin)]
  }
  
  return(db)
}
