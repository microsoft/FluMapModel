# buildSmoothModelsForDeployment.R
# this script (and similar others?) controls standardized database queries and model training for web deployment

library(dbViewR)
library(incidenceMapR)
library(modelServR)
library(dplyr)
library(modelTestR)
library(ggplot2)

shp<-masterSpatialDB()

# test: simulated data kiosk catchment map pushed to IDM P drive
#  (where still working on model storage access outside IDM--very likely dropbox with permissions)
queryIn <- list(
  SELECT   =list(COLUMN=c('sampling_location','GEOID')),
  WHERE   =list(COLUMN='sampling_location', IN = c('kiosk')),
  GROUP_BY =list(COLUMN=c('sampling_location','GEOID')),
  SUMMARIZE=list(COLUMN='sampling_location', IN= c('kiosk'))
)
db <- expandDB( selectFromDB(  queryIn ) )

modelDefinition <- smoothModel(db=db, shp=shp)
model <- modelTrainR(modelDefinition)

ggplotSmoothMap(model,shp,'kiosk')

# cache model object
# this needs to handle multiple model types and point to accessible data store
saveModel(model, cloudDir = './data')


##################################################
#########   catchment maps #######################
##################################################

# find catchment maps for each sampling_location and geoLevel
  geoLevels <- c('GEOID','PUMA5CE','CRA_NAME','NEIGHBORHOOD_DISTRICT_NAME')  

  for(geo in geoLevels){
    queryIn <- list(
      SELECT   =list(COLUMN=c('sampling_location',geo)),
      GROUP_BY =list(COLUMN=c('sampling_location',geo)),
      SUMMARIZE=list(COLUMN='sampling_location', IN= 'all')
    )
    db <- expandDB( selectFromDB(  queryIn ) )

    modelDefinition <- smoothModel(db=db, shp=shp)
    model <- modelTrainR(modelDefinition)
    summary(model$inla)
        
    saveModel(model, cloudDir = './data')
    
    if (geo =='GEOID'){
      for(k in unique(model$modeledData$sampling_location)){
        tmp<-list(modeledData = model$modeledData[model$modeledData$sampling_location==k,])
        ggplotSmoothMap(tmp,shp,k)
      }
    }
  }

  
#############################################
##### trying out interaction of two factors ########
#############################################

  # get sampling_locations and has_fever
  queryIn <- list(
    SELECT   =list(COLUMN=c('has_fever','sampling_location','GEOID')),
    GROUP_BY =list(COLUMN=c('has_fever','sampling_location','GEOID')),
    SUMMARIZE=list(COLUMN='sampling_location', IN= "all")
  )
  db <- expandDB(selectFromDB(queryIn))
  
  modelDefinition <- smoothModel(db=db, shp=shp)
  model <- modelTrainR(modelDefinition)

  # plot
  for(k in unique(model$modeledData$sampling_location)){
    for(n in unique(model$modeledData$has_fever)){
      tmp<-list(modeledData = model$modeledData[model$modeledData$sampling_location==k & model$modeledData$has_fever==n,])
      modelTestR::ggplotSmoothMap(tmp,shp,paste(k,n,sep = ' '))
    }
  }
  

  
  ##################################################
  #########   age distributions ####################
  ##################################################
  
  # find age distributions for each pathogen
  queryIn <- list(
    SELECT   =list(COLUMN=c('pathogen','age')),
    GROUP_BY =list(COLUMN=c('pathogen','age')),
    SUMMARIZE=list(COLUMN='pathogen', IN= 'all')
  )
  db<- selectFromDB(  queryIn ) 
  
  # get all ages denominator
  # I'm not sure how to implement this as single query..
    tmp<-db$observedData %>% group_by(age_bin) %>% summarize(n=sum(n))
    db$observedData <- db$observedData %>% select(-n) %>% left_join(tmp,by='age_bin')

  db <- expandDB(db)

  modelDefinition <- smoothModel(db=db, shp=shp)
  model <- modelTrainR(modelDefinition)
  summary(model$inla)
  
  saveModel(model, cloudDir = './data')
    
  ggplot(model$modeledData) + geom_line(aes(x=age_bin,y=fitted_values_mode, group=pathogen)) + geom_point(aes(x=age_bin,y=fraction, group=pathogen)) + facet_wrap("pathogen")
    
