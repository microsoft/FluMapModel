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



# factors: do a likelihood for each factor level
# this should happen at a higher level for parallelization 
if(COLUMN %in% c('pathogen','samplingLocation','fluShot','sex','hasFever','hasCough','hasMyalgia')){
}