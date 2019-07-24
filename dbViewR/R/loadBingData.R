#' addCensusData: function for loading bing search queries and categorizing which census tract it's in
#'
#' @param geography geography to bin the bing queries, default value - 'tract'  
#' @param bing_queries_path path to your file with Census API key, you can get your own census api key here: https://api.census.gov/data/key_signup.html  
#' @return db dataframe with number of bing queries per tract
#'
#' @import rgeos
#' @import sp
#' @import rgdal
#' @import dplyr
#'
#' @export
#' @examples
#' 

loadBingData <- function( geography = "tract", 
                           bing_queries_path = '/home/rstudio/seattle_flu')
  
{
  
  bing_data <- read.csv(file.path(bing_queries_path, "Bing_queries.csv"), header=FALSE)
  #edit which columns are which
  bing_queries_latlong <- data.frame(bing_data)
  coordinates(bing_queries_latlong)<- ~V3 + V2
 
   #this url is broken
  #url <-'https://github.com/seattleflu/simulated-data/blob/master/kingCountySpatialData/2016_CensusTracts_KingCountyWa.zip'
  
  #for now download from original source 
  url <-'https://www.seattle.gov/Documents/Departments/OPCD/Demographics/GeographicFilesandMaps/KingCountyTractsShapefiles.zip'
  file <- basename(url)
  download.file(url, file)
  unzip(file, exdir="KingCountyTractsShapefiles")
  
  
  #read in shapefile
  if (geography == "tract"){
    kc_shp <- readOGR(dsn='KingCountyTractsShapefiles', layer = "kc_tract_10")
  }
  #reproject bing queries onto king county shape file coordinate system 
  proj4string(bing_queries_latlong) <- CRS("+proj=longlat")
  bing_queries_latlong <- spTransform(bing_queries_latlong, proj4string(kc_shp))
  
  #get the census tracts for each of the bing queries
  bing_queries_censustracts <- over(bing_queries_latlong, kc_shp)
  
  #now join with the input data
  #for some reason there are 398 tracts in the king county shapefile and only 397 in mike's shapefile
  bing_queries_pertract <- as.data.frame(table(bing_queries_censustracts$GEOID10))
  #rename columns
  bing_queries_pertract$Var1 <- as.character(bing_queries_pertract$Var1)
  bing_queries_pertract <- rename(bing_queries_pertract, num_bing_queries = Freq)
  bing_queries_pertract <- rename(bing_queries_pertract, GEOID = Var1)
  
 return(bing_queries_pertract)
}