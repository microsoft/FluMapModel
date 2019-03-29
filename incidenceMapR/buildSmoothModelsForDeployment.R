# buildSmoothModelsForDeployment.R
# this script (and similar others?) controls standardized database queries and model training for web deployment

library(dbViewR)
library(incidenceMapR)

shp<-masterSpatialDB()

# test: simulated data kiosk catchment map pushed to IDM P drive
#  (where still working on model storage access outside IDM--very likely dropbox with permissions)
queryIn <- list(
  SELECT   =list(COLUMN=c('samplingLocation','GEOID')),
  WHERE   =list(COLUMN='samplingLocation', IN = c('kiosk')),
  GROUP_BY =list(COLUMN=c('samplingLocation','GEOID')),
  SUMMARIZE=list(COLUMN='samplingLocation', IN= c('kiosk'))
)
db <- expandDB( selectFromDB(  queryIn ) )

modelDefinition <- smoothModel(db=db, shp=shp)
model <- modelTrainR(modelDefinition)

# cache model object
# this needs to handle multiple model types and point to accessible data store
saveModel(model, cloudDir = 'P:/Seattle-Flu-Incidence-Mapper/models/')


##################################################
#########   catchment maps #######################
##################################################

# get samplingLocations
  queryIn <- list(
    SELECT   =list(COLUMN=c('samplingLocation')),
    GROUP_BY =list(COLUMN=c('samplingLocation')),
    SUMMARIZE=list(COLUMN='samplingLocation', IN= "all")
  )
  db <- selectFromDB(queryIn)

  columnLevels <- db$observedData$samplingLocation

  geoLevels <- c('GEOID','PUMA5CE','CRA_NAME','NEIGHBORHOOD_DISTRICT_NAME')  

# find catchment maps for each samplingLocation and geoLevel
  for(geo in geoLevels){
    for(level in columnLevels){
      queryIn <- list(
        SELECT   =list(COLUMN=c('samplingLocation',geo)),
        WHERE   =list(COLUMN='samplingLocation', IN = level),
        GROUP_BY =list(COLUMN=c('samplingLocation',geo)),
        SUMMARIZE=list(COLUMN='samplingLocation', IN= level)
      )
      db <- expandDB( selectFromDB(  queryIn ) )
      
      modelDefinition <- smoothModel(db=db, shp=shp)
      model <- modelTrainR(modelDefinition)
      
      saveModel(model, cloudDir = 'C:/Users/mfamulare/Dropbox (IDM)/SeattleFlu-incidenceMapR/models')
      
    }
  }


  
  ##################################################
  #########   age distributions ####################
  ##################################################
  
  # get pathogens
  queryIn <- list(
    SELECT   =list(COLUMN=c('pathogen')),
    GROUP_BY =list(COLUMN=c('pathogen')),
    SUMMARIZE=list(COLUMN='pathogen', IN= "all")
  )
  db <- selectFromDB(queryIn)
  
  columnLevels <- db$observedData$pathogen
  
  # find age distributions for each pathogen
  for(level in columnLevels){
    queryIn <- list(
      SELECT   =list(COLUMN=c('pathogen','age')),
      GROUP_BY =list(COLUMN=c('age')),
      SUMMARIZE=list(COLUMN='pathogen', IN= level)
    )
    db <- expandDB( selectFromDB(  queryIn ) )
    db$observedData$pathogen <- level
    
    modelDefinition <- smoothModel(db=db, shp=shp)
    model <- modelTrainR(modelDefinition)
    
    saveModel(model, cloudDir = 'C:/Users/mfamulare/Dropbox (IDM)/SeattleFlu-incidenceMapR/models')
    
    ggplot(model$modeledData) + geom_line(aes(x=ageBin,y=fitted.values.mean)) + geom_point(aes(x=ageBin,y=positive/n))
    
  }
