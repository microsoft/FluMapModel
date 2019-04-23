# testQuery
# script to test queries

library(dbViewR)
library(dplyr)

########################################################
####         test selectFromDB       ###################
########################################################

## return all
  queryJSON <- jsonlite::toJSON(list(SELECT  =c("*")))
  db <- selectFromDB( queryJSON )
  head(db$observedData)
  

## return subset
  queryJSON <- jsonlite::toJSON(
    list(
      SELECT  =list(COLUMN=c('id','pathogen','encountered_date','sampling_location','sex','flu_shot','age','has_fever','has_cough','has_myalgia')),
      WHERE   =list(COLUMN='pathogen', IN = c('h1n1pdm', 'h3n2')),
      WHERE   =list(COLUMN='encountered_date', BETWEEN = c(2019,2019.2)),
      WHERE   =list(COLUMN='sampling_location', IN='hospital')
    )
  )
  db <- selectFromDB( queryJSON )
  head(db$observedData)
  
########################################################
####         test summarizeDB       ###################
########################################################


## return h1n1pdm summary by time and location
  queryIn <- list(
      SELECT   =list(COLUMN=c('pathogen','encountered_date','PUMA5CE','GEOID')),
      GROUP_BY =list(COLUMN=c('encountered_date','PUMA5CE','GEOID')),
      SUMMARIZE=list(COLUMN='pathogen', IN= c('h1n1pdm'))
    )
  db <- selectFromDB( queryIn )
  head(db$observedData)
  


## return has_fever summary by age and location
  queryJSON <- jsonlite::toJSON(
    list(
      SELECT   =list(COLUMN=c('has_fever','age','PUMA5CE','GEOID')),
      GROUP_BY =list(COLUMN=c('age','PUMA5CE','GEOID')),
      SUMMARIZE=list(COLUMN='has_fever', IN= c(TRUE))
    )
  )
  db <- selectFromDB( queryJSON )
  db$observedData

  
########################################################
####         test expandDb       ###################
########################################################


## return h1n1pdm summary by time and location
queryIn <- list(
  SELECT   =list(COLUMN=c('pathogen','encountered_week','PUMA5CE','GEOID')),
  GROUP_BY =list(COLUMN=c('encountered_week','PUMA5CE','GEOID')),
  SUMMARIZE=list(COLUMN='pathogen', IN= c('h1n1pdm'))
)
db <- expandDB(selectFromDB( queryIn ))
head(db$observedData)

## return has_fever summary by age and location
queryJSON <- jsonlite::toJSON(
  list(
    SELECT   =list(COLUMN=c('has_fever','age','PUMA5CE','GEOID')),
    GROUP_BY =list(COLUMN=c('age','PUMA5CE','GEOID')),
    SUMMARIZE=list(COLUMN='has_fever', IN= 'all')
  )
)
db <- expandDB(selectFromDB( queryJSON ))
head(db$observedData)

########################################################
####         test masterShapeDB and joins      ###################
########################################################

## return h1n1pdm summary by GEOID
queryIn <- list(
  SELECT   =list(COLUMN=c('pathogen','PUMA5CE','CRA_NAME','GEOID')),
  GROUP_BY =list(COLUMN=c('PUMA5CE','CRA_NAME','GEOID')),
  SUMMARIZE=list(COLUMN='pathogen', IN= c('h1n1pdm'))
)
db <- selectFromDB( queryIn )

shp<-masterSpatialDB(shape_level = 'census_tract', source = "seattle_geojson")
plotDat<- sf::st_as_sf(db$observedData %>% left_join(shp %>% select('GEOID','geometry')))
plot(plotDat)

## return h1n1pdm summary by CRA_NAME
queryIn <- list(
  SELECT   =list(COLUMN=c('pathogen','PUMA5CE','CRA_NAME')),
  GROUP_BY =list(COLUMN=c('PUMA5CE','CRA_NAME')),
  SUMMARIZE=list(COLUMN='pathogen', IN= c('h1n1pdm'))
)
db <- selectFromDB( queryIn )
# CRA_NAME
shp<-masterSpatialDB(shape_level = 'cra_name', source = "seattle_geojson")
names(shp)[names(shp) == 'CRA_NAM']<-'CRA_NAME'
plotDat<- sf::st_as_sf(db$observedData %>% left_join(shp %>% select('CRA_NAME','geometry')))
plot(plotDat)
