# buildDeployedModels.R
# this script (and similar others?) controls standardized database queries and model training for web deployment

library(dbViewR)
library(incidenceMapR)


# test: simulated data kiosk catchment map pushed to IDM P drive
#  (where still working on model storage access outside IDM--very likely dropbox with permissions)
queryIn <- list(
  SELECT   =list(COLUMN=c('samplingLocation','GEOID')),
  WHERE   =list(COLUMN='samplingLocation', IN = c('kiosk')),
  GROUP_BY =list(COLUMN=c('samplingLocation','GEOID')),
  SUMMARIZE=list(COLUMN='samplingLocation', IN= c('kiosk'))
)
db <- expandDB( selectFromDB(  queryIn ) )

model <- modelTrainR(family='poisson',db=db, shp=shp)

# cache model object
saveModel(model, cloudDir = 'P:/Seattle-Flu-Incidence-Mapper/models/')
