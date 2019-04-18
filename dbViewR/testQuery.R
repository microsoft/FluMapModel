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

## return subset
  queryJSON <- jsonlite::toJSON(
    list(
      SELECT  =list(COLUMN=c('id','pathogen','num_date','samplingLocation','sex','fluShot','age','hasFever','hasCough','hasMyalgia')),
      WHERE   =list(COLUMN='pathogen', IN = c('h1n1pdm', 'h3n2')),
      WHERE   =list(COLUMN='num_date', BETWEEN = c(2019,2019.2)),
      WHERE   =list(COLUMN='samplingLocation', IN='hospital')
    )
  )
  db <- selectFromDB( queryJSON )

########################################################
####         test summarizeDB       ###################
########################################################


## return h1n1pdm summary by time and location
  queryIn <- list(
      SELECT   =list(COLUMN=c('pathogen','num_date','PUMA5CE','GEOID')),
      MUTATE   =list(COLUMN=c('num_date'), AS=c('timeBin')),
      GROUP_BY =list(COLUMN=c('timeBin','PUMA5CE','GEOID')),
      SUMMARIZE=list(COLUMN='pathogen', IN= c('h1n1pdm'))
    )
  db <- selectFromDB( queryIn )


## return hasFever summary by age and location
  queryJSON <- jsonlite::toJSON(
    list(
      SELECT   =list(COLUMN=c('hasFever','age','PUMA5CE','GEOID')),
      GROUP_BY =list(COLUMN=c('age','PUMA5CE','GEOID')),
      SUMMARIZE=list(COLUMN='hasFever', IN= c(TRUE))
    )
  )
  db <- selectFromDB( queryJSON )


  
########################################################
####         test expandDb       ###################
########################################################


## return h1n1pdm summary by time and location
queryIn <- list(
  SELECT   =list(COLUMN=c('pathogen','num_date','PUMA5CE','GEOID')),
  MUTATE   =list(COLUMN=c('num_date'), AS=c('timeBin')),
  GROUP_BY =list(COLUMN=c('timeBin','PUMA5CE','GEOID')),
  SUMMARIZE=list(COLUMN='pathogen', IN= c('h1n1pdm'))
)
db <- expandDB(selectFromDB( queryIn ))


## return hasFever summary by age and location
queryJSON <- jsonlite::toJSON(
  list(
    SELECT   =list(COLUMN=c('hasFever','age','PUMA5CE','GEOID')),
    GROUP_BY =list(COLUMN=c('age','PUMA5CE','GEOID')),
    SUMMARIZE=list(COLUMN='hasFever', IN= c(TRUE))
  )
)
db <- expandDB(selectFromDB( queryJSON ))


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
