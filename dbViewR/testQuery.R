# testQuery
# script to test queries

library(dbViewR)
library(dplyr)

########################################################
####         test selectFromDB       ###################
########################################################

########################################################
####     simulated-data          #######################
########################################################

## return all
  queryJSON <- jsonlite::toJSON(list(SELECT  =c("*")))
  db <- selectFromDB( queryJSON )
  head(db$observedData)
  

## return subset
  queryJSON <- jsonlite::toJSON(
    list(
      SELECT  =list(COLUMN=c('individual','pathogen','encountered_date','sampling_location','sex','flu_shot','age','has_fever','has_cough','has_myalgia')),
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
      SELECT   =list(COLUMN=c('pathogen','encountered_date','residence_puma5ce','residence_census_tract')),
      GROUP_BY =list(COLUMN=c('encountered_date','residence_puma5ce','residence_census_tract')),
      SUMMARIZE=list(COLUMN='pathogen', IN= c('h1n1pdm'))
    )
  db <- selectFromDB( queryIn )
  head(db$observedData)
  


## return has_fever summary by age and location
  queryJSON <- jsonlite::toJSON(
    list(
      SELECT   =list(COLUMN=c('has_fever','age','residence_puma5ce','residence_census_tract')),
      GROUP_BY =list(COLUMN=c('age','residence_puma5ce','residence_census_tract')),
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
  SELECT   =list(COLUMN=c('pathogen','encountered_week','residence_puma5ce','residence_census_tract')),
  GROUP_BY =list(COLUMN=c('encountered_week','residence_puma5ce','residence_census_tract')),
  SUMMARIZE=list(COLUMN='pathogen', IN= c('h1n1pdm'))
)
db <- expandDB(selectFromDB( queryIn ))
head(db$observedData)

## return has_fever summary by age and location
queryJSON <- jsonlite::toJSON(
  list(
    SELECT   =list(COLUMN=c('has_fever','age','residence_puma5ce','residence_census_tract')),
    GROUP_BY =list(COLUMN=c('age','residence_puma5ce','residence_census_tract')),
    SUMMARIZE=list(COLUMN='has_fever', IN= 'all')
  )
)
db <- expandDB(selectFromDB( queryJSON ))
head(db$observedData)

########################################################
####         test masterShapeDB and joins      ###################
########################################################

## return h1n1pdm summary by residence_census_tract
queryIn <- list(
  SELECT   =list(COLUMN=c('pathogen','residence_puma5ce','residence_cra_name','residence_census_tract')),
  GROUP_BY =list(COLUMN=c('residence_puma5ce','residence_cra_name','residence_census_tract')),
  SUMMARIZE=list(COLUMN='pathogen', IN= c('h1n1pdm'))
)
db <- selectFromDB( queryIn )

shp<-masterSpatialDB(shape_level = 'census_tract', source = "seattle_geojson")
plotDat<- sf::st_as_sf(db$observedData %>% left_join(shp %>% select('residence_census_tract','geometry')))
plot(plotDat)

## return h1n1pdm summary by residence_cra_name
queryIn <- list(
  SELECT   =list(COLUMN=c('pathogen','residence_puma5ce','residence_cra_name')),
  GROUP_BY =list(COLUMN=c('residence_puma5ce','residence_cra_name')),
  SUMMARIZE=list(COLUMN='pathogen', IN= c('h1n1pdm'))
)
db <- selectFromDB( queryIn )
# residence_cra_name
shp<-masterSpatialDB(shape_level = 'cra_name', source = "seattle_geojson")
plotDat<- sf::st_as_sf(db$observedData %>% left_join(shp %>% select('residence_cra_name','geometry')))
plot(plotDat)



########################################################
####     production              #######################
########################################################

## return all
queryJSON <- jsonlite::toJSON(list(SELECT  =c("*")))
db <- selectFromDB( queryJSON, source = 'production')
head(db$observedData)

names(db$observedData)


## return subset
queryJSON <- jsonlite::toJSON(
  list(
    SELECT  =list(COLUMN=c('individual','encountered_date','site_type','sex','flu_shot','age')),
    WHERE   =list(COLUMN='encountered_date', BETWEEN = c('2019-01-01','2019-02-28')),
    WHERE   =list(COLUMN='site_type', IN='childrensHospital')
  )
)
db <- selectFromDB( queryJSON, source = 'production' )
head(db$observedData)


## return subset
queryJSON <- jsonlite::toJSON(
  list(
    SELECT  =list(COLUMN=c('encountered_week','site_type','sex','flu_shot','age')),
    WHERE   =list(COLUMN='encountered_week', BETWEEN = c('2018-W51','2019-W10'))
  )
)
db <- selectFromDB( queryJSON, source = 'production' )
head(db$observedData)


## space-time summary of encounters
queryJSON <- jsonlite::toJSON(
  list(
    SELECT  =list(COLUMN=c('encountered_week','residence_census_tract','site_type','flu_shot')),
    GROUP_BY=list(COLUMN=c('encountered_week','residence_census_tract','site_type','flu_shot')),
    SUMMARIZE=list(COLUMN='site_type', IN= 'all')
  )
)
db <- selectFromDB( queryJSON, source = 'production' )
head(db$observedData)



########################################################
####         test expandDb       ###################
########################################################


## return encounters summary for catchment modeling
queryJSON <- jsonlite::toJSON(
  list(
    SELECT  =list(COLUMN=c('encountered_week','residence_census_tract','site_type','flu_shot')),
    GROUP_BY=list(COLUMN=c('encountered_week','residence_census_tract','site_type','flu_shot')),
    SUMMARIZE=list(COLUMN='site_type', IN= 'all')
  )
)
db <- expandDB( selectFromDB( queryJSON, source = 'production' ))
head(db$observedData)

