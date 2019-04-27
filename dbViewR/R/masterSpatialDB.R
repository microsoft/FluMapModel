#' masterSpatialDB: function for fetching spatial data from master source
#'
#' @param shape_level one of "census_tract" (default),"cra_name","neighborhood","city"
#' @param source source database, one of "seattle_geojson" (default), "simulated_data"
#' @param rm_files indicator to remove local files (TRUE == default)
#' @return sf object with shapefile data
#'
#' @import geojsonio
#' @import rgdal
#' @import sf
#'
#' @export
#' @examples
#'    shp <- masterSpatialDB(shape_level = 'census_tract', source = 'simulated_data', rm_files = TRUE)
#'
masterSpatialDB <- function(shape_level = 'census_tract', source = 'simulated_data', rm_files = TRUE){
  
  if (source == 'seattle_geojson'){
    # connect to database and get the data at correct shape level
    
    sourceURL <- paste('https://raw.githubusercontent.com/seattleflu/seattle-geojson/master/seattle_geojsons/')
    # pumas are missing from repo
    
    validShapeLevels <- c("census_tract","cra_name","neighborhood","city")
    validShapeFilenames<- c("2016_seattle_censusTracts.geojson","2016_seattle_cra.geojson","2016_seattle_neighborhoods.geojson","2016_seattle_city.geojson")
    
    filename<-validShapeFilenames[validShapeLevels %in% shape_level]
    
    sourceURL <- paste('https://raw.githubusercontent.com/seattleflu/seattle-geojson/master/seattle_geojsons/',filename,sep='')
    download.file(url = sourceURL,  destfile = filename)
    
    shp <- sf::st_as_sf(geojsonio::geojson_read(filename, what = "sp"))
    
  } else if(source == 'simulated_data' & shape_level == "census_tract"){
    
    filename <- "2016_CensusTracts_KingCountyWa"
    download.file(url = "https://github.com/seattleflu/simulated-data/raw/master/kingCountySpatialData/2016_CensusTracts_KingCountyWa.zip",
                  destfile = paste0(filename,'.zip'))
    unzip(zipfile = paste0(filename,'.zip'))
    shp <- sf::st_read(filename)
    
  } else {
    return('unknown source and shape_level combination!')
  }
  
  if (rm_files){
    #unlink from the database
    unlink(filename, recursive = TRUE)
    unlink(paste0(filename,'.zip'), recursive = TRUE)
  }
  
  levels(shp$NEIGHBO)<-c(levels(shp$NEIGHBO),'NA')
  shp$NEIGHBO[is.na(shp$NEIGHBO)]<-'NA'
  
  levels(shp$CRA_NAM)<-c(levels(shp$CRA_NAM),'NA')
  shp$CRA_NAM[is.na(shp$CRA_NAM)]<-'NA'
  
  # harmonize shp names with database names for joins down the line
  for( NAME in names(shp)){
    if (grepl('GEOID',NAME,ignore.case = TRUE)){
      FIELDNAME <- names(shp)[grepl('GEOID',names(shp),ignore.case = TRUE)]
      shp$residence_census_tract <- as.character(shp[[FIELDNAME]])
      shp$work_census_tract <- as.character(shp[[FIELDNAME]])
    } else if (grepl('CRA_NAM',NAME,ignore.case = TRUE)){
      FIELDNAME <- names(shp)[grepl('CRA_NAM',names(shp),ignore.case = TRUE)]
      shp$residence_cra_name <- as.character(shp[[FIELDNAME]])
      shp$work_cra_name <- as.character(shp[[FIELDNAME]])
    } else if (grepl('NEIGHBO',NAME,ignore.case = TRUE)){
      FIELDNAME <- names(shp)[grepl('NEIGHBO',names(shp),ignore.case = TRUE)]
      shp$residence_neighborhood_district_name <- as.character(shp[[FIELDNAME]])
      shp$work_neighborhood_district_name <- as.character(shp[[FIELDNAME]])
    } else if (grepl('PUMA',NAME,ignore.case = TRUE)){
      FIELDNAME <- names(shp)[grepl('PUMA',names(shp),ignore.case = TRUE)]
      shp$residence_puma5ce <- as.character(shp[[FIELDNAME]])
      shp$work_puma5ce <- as.character(shp[[FIELDNAME]])
    } else if (grepl('CITY',NAME,ignore.case = TRUE)){
      FIELDNAME <- names(shp)[grepl('CITY',names(shp),ignore.case = TRUE)]
      shp$residence_city <- as.character(shp[[FIELDNAME]])
      shp$work_city <- as.character(shp[[FIELDNAME]])
    }
  }
  
  return(shp)
  
}

