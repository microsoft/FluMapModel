#pull spatial data from https://github.com/seattleflu/seattle-geojson
#takes shape_level which can be a string chosen from c("censusTracts", "cra_name", "neighborhood", "city")

SpatialDB <- function(shape_level){
  # connect to database and get the data
  library(geojsonio)
  library(rgdal)

  # connect to database and get the data at correct shape level
  if ( shape_level == "censusTracts") {
    download.file(url = "https://raw.githubusercontent.com/seattleflu/seattle-geojson/master/seattle_geojsons/2016_seattle_censusTracts.geojson",
                  destfile = "2016_seattle_censusTracts.geojson")
    shp <- geojsonio::geojson_read("2016_seattle_censusTracts.geojson", what = "sp")
  } else if ( shape_level == "cra_name") {
    download.file(url = "https://raw.githubusercontent.com/seattleflu/seattle-geojson/master/seattle_geojsons/2016_seattle_cra.geojson",
                  destfile = "2016_seattle_cra.geojson")
    shp <- geojsonio::geojson_read("2016_seattle_cra.geojson", what = "sp")
  } else if ( shape_level == "neighborhood") {
    download.file(url = "https://raw.githubusercontent.com/seattleflu/seattle-geojson/master/seattle_geojsons/2016_seattle_neighborhoods.geojson",
                  destfile = "2016_seattle_neighborhoods.geojson")
    shp <- geojsonio::geojson_read("2016_seattle_neighborhoods.geojson", what = "sp")
  } else { #only other option is city, refine this if we have other choices for shape_level
    download.file(url = "https://raw.githubusercontent.com/seattleflu/seattle-geojson/master/seattle_geojsons/2016_seattle_city.geojson",
                  destfile = "2016_seattle_city.geojson")
    shp <- geojsonio::geojson_read("2016_seattle_city.geojson", what = "sp")
  }
  
  
#unlink from the database
unlink('2016_seattle_censusTracts', recursive = TRUE)
unlink('2016_seattle_censusTracts.geojson')


#clean up NA from the columns (same as from original Mike F)
levels(shp$NEIGHBO)<-c(levels(shp$NEIGHBO),'NA')
shp$NEIGHBO[is.na(shp$NEIGHBO)]<-'NA'

levels(shp$CRA_NAM)<-c(levels(shp$CRA_NAM),'NA')
shp$CRA_NAM[is.na(shp$CRA_NAM)]<-'NA'

#change data type (same as from original Mike F)
shp$GEOID<-as.character(shp$GEOID)
shp$CRA_NAM<-as.character(shp$CRA_NAM)
shp$NEIGHBO<-as.character(shp$NEIGHBO)
shp$PUMA5CE<-as.character(shp$PUMA5CE)

return(shp)

}







# selectFromDB_SQL: function for fetching data from Hutch DB
# data dictionary from kiosk app can be found here https://github.com/seattleflu/documentation

library('DBI')
library('RPostgres')
library('tidyverse')
library('lubridate')
library('RColorBrewer')
library('rlist')


#location of your pg_service and pgpass file - users should edit this as appropriate
user_path = "/home/rstudio/seattle_flu"

#service you want to access
service_string = "seattleflu-production"



#get host and dbname from pg_service file, then get user and password from the pgpass file 
pg_service_file<-read.table(file.path(user_path, ".pg_service.conf"), header=FALSE) #read in file
service_index <-which(str_detect(pg_service_file$V1, service_string)) #get index for specified service
host_string <- strsplit(as.character(pg_service_file$V1[service_index+1]), "=") #host string is next item after index for service
host_string <- host_string[[1]][2]
dbname_string <- strsplit(as.character(pg_service_file$V1[service_index+2]), "=") #dbname string is next item after index for service
dbname_string <- dbname_string[[1]][2]

#read in pgpass file
pgpass_file <- read.table(file.path(user_path, ".pgpass"), header=FALSE)
pgpass_file <- strsplit(levels(pgpass_file$V1), ":") #convert from factor to list

#get index for which row has the correct host
host_index<-which(grepl(host_string, pgpass_file))
db <- DBI::dbConnect(RPostgres::Postgres(), 
                host=host_string, 
                dbname = dbname_string, 
                user=pgpass_file[[host_index]][4], 
                password=pgpass_file[[host_index]][5])


#load entire dataset
all_data <- DBI::dbGetQuery(db, "select * from shipping.incidence_model_observation_v1;")
dbDisconnect(db)

##below some helpful filtering of the overall dataset
#see a few lines of data 
head(all_data)
#see structure of the data
str(all_data)
#see data type
typeof(str(all_data$sex[1]))

#get all column names
colnames(all_data)


#example add new column
all_data <- mutate(all_data, new_encountered_date = as_date(all_data$encountered_date))

#example filters
filter(all_data, sex=="female")
filter(all_data, encountered_date <= as.Date('2019-01-15') | encountered_date >=as.Date('2019-01-20'))
filter(all_data, site_type=="collegeCampus")
filter(all_data, race %in% c("{asian}")) #must be perfect match

#remove curly braces from race and symptoms columns
all_data$race <- gsub("[{}]", "", all_data$race)
all_data$symptoms <- gsub("[{}]", "", all_data$symptoms)

#find if text present (so you don't have to remove the curly braces)
Feverish_index <- grepl("feelingFeverish",all_data$symptoms)
Feverish_index <- str_detect(all_data$symptoms, "feelingFeverish")


##example actually useful query
# return feelingFeverish summary by age and location
Feverish <- all_data %>% filter(Feverish_index) %>% select(symptoms, age, residence_census_tract)
#count the number of entries for each census tract
Feverish_residence <-table(Feverish$residence_census_tract)
Feverish_residence <-as.data.frame(table(Feverish$residence_census_tract))
names(Feverish_residence[1]) <- "residence_census_tract" #rename variable

#get king county shapefile using function from Mike's selectFromDB(pasted below)
#shp <- masterSpatialDB()

#get shapefile from seattleflu/geo-json 
#takes shape_level which can be a string chosen from c("censusTracts", "cra_name", "neighborhood", "city")
shp <- SpatialDB("neighborhood")

#example plotting
plot(shp)
