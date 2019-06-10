


library("tidycensus")
library("tigris")
library("sf")
library("mapview")
#library(totalcensus) #downloads the entire survey data
#download the 2015 ACS 5-year survey data, which is about 50 GB.
#download_census("acs5year", 2015)

options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

#census api guide https://www.census.gov/data/developers/guidance/api-user-guide.html 
#get your own census api key here: https://api.census.gov/data/key_signup.html  
#save the key to a txt file named census_api_key.txt

#enter your own user path here
user_path = "C:/Users/grhuynh"

#get the census api key value from the file
census_api_key_value<-read.table(file.path(user_path, "census_api_key.txt"), header=FALSE) #read in file
census_api_key_value<- as.character(census_api_key_value$V1)

#read in the value into the tidycensus library
census_api_key(census_api_key_value)


#read in the shape file for the desired geography, in this case tracts of king county
state_name = "WA"
county_name = "King"
county_tracts <-tracts(state_name, county_name)


#to see all variables
#v17 <- load_variables(2010, "sf1", cache = TRUE) #2010 decennial census
#v17_acs <- load_variables(2017, "acs5", cache = TRUE) # acs census 
#view(v17_acs)


#get the total population, P001001 in decennial census
#if need multiple states alternative syntax here https://github.com/walkerke/tidycensus/issues/121
#if need multiple variable syntax here https://github.com/walkerke/tidycensus/issues/129 
pop <- get_decennial(geography = "tract", state = "WA", county = county_name, 
                     variables = "P001001", year = 2010, geometry = TRUE)

#also can get population data from acs
pop_acs <- get_acs(geography = "tract", state = state_name, county = county_name, 
                   variables = "B01003_001", year = 2017, geometry = TRUE)


#plot population graph
pop %>%
  ggplot(aes(fill = value)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "magma") 

pop_acs %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "magma") 

