# testModelTrainR
# script to test incidenceMapR package

library(dbViewR)
library(incidenceMapR)
library(modelTestR)
library(modelServR)
library(dplyr)
library(magrittr)

shp <- masterSpatialDB()  # census-tract shapefiles
neighborGraph <- constructAdjacencyNetwork(shp)

# ggplot build eventually will be replaced by function ggplotSmoothSequential
library(ggplot2)
plotSettings <- ggplot() + theme_bw() +  theme(panel.border = element_blank()) + xlab('')


###################################
#### timeseries latent field ######
###################################

geoLevels <- c('PUMA5CE','NEIGHBORHOOD_DISTRICT_NAME','CRA_NAME','NEIGHBORHOOD_DISTRICT_NAME','GEOID')

  # geoLevels<-c('PUMA5CE')
  # geoLevels<-c('NEIGHBORHOOD_DISTRICT_NAME')
  # geoLevels<-c('CRA_NAME')
  # geoLevels <- c('GEOID')


# get pathogen list
queryIn <- list(
  SELECT   =list(COLUMN=c('pathogen')),
  GROUP_BY =list(COLUMN=c('pathogen')),
  SUMMARIZE=list(COLUMN='pathogen', IN= 'all')
)
pathogens <- unique(selectFromDB(  queryIn )$observedData$pathogen)

for(geo in geoLevels){

  for( k in 1:length(pathogens)){
    # query pathogen, time, levels you want to play with
    queryIn <- list(
      SELECT   =list(COLUMN=c('encountered_week','pathogen','sampling_location','flu_shot',geo)),
      WHERE    =list(COLUMN=c('pathogen'), IN=pathogens[k]),
      GROUP_BY =list(COLUMN=c('pathogen','encountered_week','sampling_location','flu_shot',geo)),
      SUMMARIZE=list(COLUMN='pathogen', IN= pathogens[k])
    )
    db <- expandDB( db<-selectFromDB(  queryIn ))

    db <- appendCatchmentModel(db,shp=shp)

    # build latent field model
    modelDefinition <- latentFieldModel(db=db, shp=shp)
    model <- modelTrainR(modelDefinition)

    # model<-returnModel(db$queryList,format='model',type='latent_field',cloudDir='~/data')
    print(summary(model$inla))


    idx <- model$modeledData$sampling_location=='hospital' & model$modeledData$flu_shot==1
    plot(log(model$modeledData$fitted_values_mode[idx]))
    plot(model$latentField$latent_field_mode)

    plot(model$latentField$latent_field_mode,
         log(model$modeledData$fitted_values_mode[idx]))


    saveModel(model, cloudDir = './data')


    if (geo =='GEOID'){
      for(m in unique(model$modeledData$sampling_location)){
        for (n in unique(model$modeledData$time_row)){
          tmp<-list(modeledData = model$modeledData[model$modeledData$sampling_location==m & model$modeledData$time_row == n & model$modeledData$flu_shot==0,])
          ggplotSmoothMap(tmp,shp,paste(m,n))
        }
      }
    }
  }
}
