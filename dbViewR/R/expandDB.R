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
                      linelist = dbViewR::selectFromDB(db$queryList[names(db$queryList) %in% c("SELECT","WHERE")]),
                      shp = dbViewR::masterSpatialDB(shape_level = 'census_tract', source = 'king_county_geojson') ){
  
  
  # list of valid column data for expanding and joining
    
    validColumnData <- list()
    
    # encountered_week
    if ("encountered_week" %in% names(db$observedData)){
      weeks <- unique(sort(db$observedData$encountered_week))
      minYear <- year<-as.numeric(gsub('-W[0-9]{2}','',weeks[1]))
      maxYear <- year<-as.numeric(gsub('-W[0-9]{2}','',weeks[length(weeks)]))
      minWeek <- as.numeric(gsub('[0-9]{4}-W','',weeks[1] ))
      maxWeek <- as.numeric(gsub('[0-9]{4}-W','',weeks[length(weeks)] )) + (maxYear-minYear)*52 + 4 # 4 week look-ahead
      
      weeks <- 1+( (seq(minWeek,maxWeek,by=1)-1) %% 52)
      yearBreaks <- c(0,which(diff(weeks)<1), length(weeks))
      years=c()
      for (k in 2:length(yearBreaks)){
          years <- c(years, rep(minYear+(k-2), yearBreaks[k]-yearBreaks[k-1]  ))
      }    
      validColumnData$encountered_week <- paste(years,'-W',sprintf("%02d",weeks),sep='')
      validColumnData$time_row <- 1:length(validColumnData$encountered_week)
    }

    # age
      validColumnData$age = seq(0,90,by=1)
    
    # age bin
    if(any(grepl('age',names(db$observedData)))) {
      validColumnData$age_bin <- seq(0,90,by=5)
      validColumnData$age_row <- 1:length(validColumnData$age_bin)
    }
      
    # geography
      validColumnData$residence_census_tract = shp$residence_census_tract
      validColumnData$residence_cra_name = sort(unique(shp$residence_cra_name)) 
      validColumnData$residence_neighborhood_district_name = sort(unique(shp$residence_neighborhood_district_name))
      validColumnData$residence_puma = sort(unique(shp$residence_puma))
      validColumnData$residence_city = sort(unique(shp$residence_city))
      validColumnData$work_census_tract = shp$work_census_tract
      validColumnData$work_cra_name = sort(unique(shp$work_cra_name))
      validColumnData$work_neighborhood_district_name = sort(unique(shp$work_neighborhood_district_name))
      validColumnData$work_puma = sort(unique(shp$work_puma))
      validColumnData$work_city = sort(unique(shp$work_city))
      
      validColumnData$residence_cra_name = validColumnData$residence_cra_name[validColumnData$residence_cra_name!='NA']
      validColumnData$residence_neighborhood_district_name = validColumnData$residence_neighborhood_district_name[validColumnData$residence_neighborhood_district_name!='NA']
      validColumnData$residence_city = validColumnData$residence_city[validColumnData$residence_city!='NA']
      
      validColumnData$work_cra_name = validColumnData$work_cra_name[validColumnData$work_cra_name!='NA']
      validColumnData$work_neighborhood_district_name = validColumnData$work_neighborhood_district_name[validColumnData$work_neighborhood_district_name!='NA']
      validColumnData$work_city = validColumnData$work_city[validColumnData$work_city!='NA']
      
    # factors (these don't get interpolated by the models, so we only want the valid levels for the dataset at hand)
      factorNames <- names(db$observedData)[ !( (names(db$observedData) %in% c('n','positive')) | 
                                                grepl('age',names(db$observedData)) | 
                                                grepl('residence_',names(db$observedData)) | 
                                                grepl('work_',names(db$observedData)) |
                                                grepl('encounter',names(db$observedData))  )]
      for ( COLUMN in factorNames){ 
        validColumnData[[COLUMN]] <- sort(unique(db$observedData[[COLUMN]]))
      }

  
  # don't expand on nested shape variables
    nestedVariables <- c('cra_name','neighborhood_district_name','puma','city')

  # expand.grid for non-nested variables
    colIdx <- ( names(validColumnData) %in% names(db$observedData) ) &  !( names(validColumnData) %in% nestedVariables) 
    tmp<-expand.grid(validColumnData[colIdx],stringsAsFactors = FALSE)
    
  # join
  db$observedData <- dplyr::left_join(tmp,db$observedData, by=names(validColumnData)[colIdx])
  
  # sample size as zero instead of NaN
  if ("n" %in% names(db$observedData)){
    db$observedData$n[is.na(db$observedData$n)]<-0
      
      # positives as 0 instead of NaN when positive count is total count always (eg catchments) 
      idx <- !is.na(db$observedData$positive)
      if(all(db$observedData$positive[idx]==db$observedData$n[idx])){
        db$observedData$positive[!idx]<-0
      }
  }
  
  # nested variables
    colIdx <- which(( names(validColumnData) %in% names(db$observedData) ) & ( names(validColumnData) %in% nestedVariables ) )
    for( k in colIdx){
      colName <- names(validColumnData)[k]
      db$observedData[[colName]] <- shp[[colName]][match(db$observedData[[nestedVariables[nestedVariables == colName]]],as.character(shp[[nestedVariables[nestedVariables == colName]]]))]
    }
  
  # row indices for INLA
  if(any(grepl('encountered_week',names(db$observedData)))){
    db$observedData$time_row <- validColumnData$time_row[match(db$observedData$encountered_week,validColumnData$encountered_week)]
  }

  if(any(grepl('age_bin',names(db$observedData)))){
    db$observedData$age_row <- validColumnData$age_row[match(db$observedData$age_bin,validColumnData$age_bin)]
  }
  
  return(db)
}
