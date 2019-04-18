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
    encountered_date = unique(linelist$observedData$encountered_date),
    sampling_location = unique(linelist$observedData$sampling_location),
    sex = unique(linelist$observedData$sex),
    flu_shot = unique(linelist$observedData$flu_shot),
    age = unique(linelist$observedData$age),
    has_fever = unique(linelist$observedData$has_fever),
    has_cough = unique(linelist$observedData$has_cough),
    has_myalgia = unique(linelist$observedData$has_myalgia),
    GEOID = shp$GEOID,
    CRA_NAME = unique(shp$CRA_NAME),
    NEIGHBORHOOD_DISTRICT_NAME = unique(shp$NEIGHBORHOOD_DISTRICT_NAME),
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
  if('epi_week' %in% names(db$observedData)){
    validColumnData$epi_week <- sort(unique(db$observedData$epi_week))
    validColumnData$time_row <- 1:(length(validColumnData$epi_week)+4)
    
    # format predict epi week str
    tmp<-sapply(strsplit(validColumnData$epi_week[length(validColumnData$epi_week)],'-W'),as.numeric)
    for(k in 1:4){
      ew <- (tmp[2] + k) %% 52
      ey <- tmp[1] + floor((tmp[2] + k)/52)
      validColumnData$epi_week[length(validColumnData$epi_week)+1]<-paste(ey,'-W',ew,sep='')
    }
  }
  
  if('iso_week' %in% names(db$observedData)){
    validColumnData$iso_week <- sort(unique(db$observedData$iso_week))
    validColumnData$time_row <- 1:(length(validColumnData$iso_week)+4)
    
    # format predict iso week str (# there is surely a better way to do this!)
    tmp<-sapply(strsplit(validColumnData$iso_week[length(validColumnData$iso_week)],'_W'),as.numeric)
    for(k in 1:4){
      ew <- (tmp[2] + k) %% 52
      ey <- tmp[1] + floor((tmp[2] + k)/52)
      validColumnData$iso_week[length(validColumnData$iso_week)+1]<-paste(ey,'-W',ew,sep='')
    }
  }
  
  # age bin
  if(any(grepl('age',names(db$observedData)))) {
    validColumnData$age_bin <- seq(0,90,by=1)
    validColumnData$age_row <- 1+seq(0,90,by=1)
  }
  
  # expand.grid for non-nested variables
  colIdx <- ( names(validColumnData) %in% names(db$observedData) ) & 
    !( names(validColumnData) %in% names(nestedVariables)) &
    !( names(validColumnData) %in% c('encountered_date','age'))
  tmp<-expand.grid(validColumnData[colIdx],stringsAsFactors = FALSE)
  
  # join
  db$observedData <- dplyr::left_join(tmp,db$observedData, by=names(validColumnData)[colIdx])
  
  # sample size as zero instead of NaN
  db$observedData$n[is.na(db$observedData$n)]<-0
    
    # positives as 0 instead of NaN when positive count is total count always (eg catchments) 
    idx <- !is.na(db$observedData$positive)
    if(all(db$observedData$positive[idx]==db$observedData$n[idx])){
      db$observedData$positive[!idx]<-0
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
  if(any(grepl('iso_week',names(db$observedData)))){
    if(any(grepl('epi_week',names(db$observedData)))){
      print('iso_week and epi_week both present!  time_row defined by iso_week by default.')
    }
    db$observedData$time_row <- validColumnData$time_row[match(db$observedData$iso_week,validColumnData$iso_week)]
  } else if(any(grepl('epi_week',names(db$observedData)))){
    db$observedData$time_row <- validColumnData$time_row[match(db$observedData$epi_week,validColumnData$epi_week)]
  }
    
  if(any(grepl('age',names(db$observedData)))){
    db$observedData$age_row <- validColumnData$age_row[match(db$observedData$age_bin,validColumnData$age_bin)]
  }
  
  return(db)
}
