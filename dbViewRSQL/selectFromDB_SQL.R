

#' masterSpatialDB: function for fetching spatial data from master source (MIke F)
#' (Currently pulls from https://github.com/seattleflu/simulated-data/tree/master/kingCountySpatialData)
#'
#' @return sf object with king county shapefile data
#'
#' @importFrom sf st_read
#'
#' @export
#' @examples
#'    shp <- masterSpatialDB()
#'
masterSpatialDB <- function(){
  # should take option of shape level!

  # connect to database
  download.file(url = "https://github.com/seattleflu/simulated-data/raw/master/kingCountySpatialData/2016_CensusTracts_KingCountyWa.zip",
                destfile = "2016_CensusTracts_KingCountyWa.zip")
  unzip(zipfile = "2016_CensusTracts_KingCountyWa.zip")
  shp <- sf::st_read("2016_CensusTracts_KingCountyWa")

  #clean up NA from the columns
  levels(shp$NEIGHBO)<-c(levels(shp$NEIGHBO),'NA')
  shp$NEIGHBO[is.na(shp$NEIGHBO)]<-'NA'

  levels(shp$CRA_NAM)<-c(levels(shp$CRA_NAM),'NA')
  shp$CRA_NAM[is.na(shp$CRA_NAM)]<-'NA'

  #unlink from the database
  unlink('2016_CensusTracts_KingCountyWa', recursive = TRUE)
  unlink('2016_CensusTracts_KingCountyWa.zip')

  #change data type
  shp$GEOID<-as.character(shp$GEOID)
  shp$CRA_NAM<-as.character(shp$CRA_NAM)
  shp$NEIGHBO<-as.character(shp$NEIGHBO)
  shp$PUMA5CE<-as.character(shp$PUMA5CE)
  
  return(shp)


  # this will eventually pull from curated data like: https://github.com/seattleflu/seattle-geojson
}


# selectFromDB_SQL: function for fetching data from Hutch DB
# data dictionary from kiosk app can be found here https://github.com/seattleflu/documentation

library('DBI')
library('RPostgres')
library('tidyverse')
library('lubridate')
library('RColorBrewer')

#enter in your credentials here
db <- dbConnect(RPostgres::Postgres(), host="production.db.seattleflu.org", dbname = 'production', user='', password='')

#load entire dataset
all_data <- dbGetQuery(db, "select * from shipping.incidence_model_observation_v1;")
dbDisconnect(db)

##below some helpful filtering of the overall dataset
#see a few lines of data 
head(all_data)
#see structure of the data
str(all_data)
#see data type
typeof(str(all_data$sex[1]))


#example add new column
all_data <- mutate(all_data, new_encountered_date = as_date(all_data$encountered_date))

#example filters
filter(all_data, sex=="female")
filter(all_data, encountered_date <= as.Date('2019-01-15') | encountered_date >=as.Date('2019-01-20'))
filter(all_data, site_type=="collegeCampus")
filter(all_data, sex %in% c("female"))
filter(all_data, race %in% c("{asian}")) #must be perfect match

#remove curly braces from race and symptoms columns
all_data$race <- gsub("[{}]", "", all_data$race)
all_data$symptoms <- gsub("[{}]", "", all_data$symptoms)

#find if text present (so you don't have to remove the curly braces)
Feverish_index <- grepl("feelingFeverish",all_data$symptoms)

##example actually useful query
# return feelingFeverish summary by age and location
Feverish <- all_data %>% filter(Feverish_index) %>% select(symptoms, age, residence_census_tract)
#count the number of entries for each census tract
Feverish_residence <-table(Feverish$residence_census_tract)
Feverish_residence <-as.data.frame(table(Feverish$residence_census_tract))
names(Feverish_residence[1]) <- "residence_census_tract" #rename variable

#get king county shapefile using function from Mike's selectFromDB(pasted below)
shp <- masterSpatialDB()
#plot the samples on the map

#example plotting
plot(shp$geometry, col=colors()[1:397])
plot(shp$geometry, col=rainbow(397))
plot(shp$geometry, col=heat.colors(397))

