# buildSmoothModelsForDeployment.R
# this script (and similar others?) controls standardized database queries and model training for web deployment

library(dbViewR)
library(incidenceMapR)
library(dplyr)
library(modelTestR)
library(ggplot2)

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

ggplotSmoothMap(model,shp,'kiosk')

# cache model object
# this needs to handle multiple model types and point to accessible data store
saveModel(model, cloudDir = 'P:/Seattle-Flu-Incidence-Mapper/models/')


##################################################
#########   catchment maps #######################
##################################################

# find catchment maps for each samplingLocation and geoLevel
  geoLevels <- c('GEOID','PUMA5CE','CRA_NAME','NEIGHBORHOOD_DISTRICT_NAME')  

  for(geo in geoLevels){
    queryIn <- list(
      SELECT   =list(COLUMN=c('samplingLocation',geo)),
      GROUP_BY =list(COLUMN=c('samplingLocation',geo)),
      SUMMARIZE=list(COLUMN='samplingLocation', IN= 'all')
    )
    db <- expandDB( selectFromDB(  queryIn ) )
    
    modelDefinition <- smoothModel(db=db, shp=shp)
    model <- modelTrainR(modelDefinition)
    summary(model$inla)
        
    saveModel(model, cloudDir = 'C:/Users/mfamulare/Dropbox (IDM)/SeattleFlu-incidenceMapR/models')
    
    if (geo =='GEOID'){
      for(k in unique(model$modeledData$samplingLocation)){
        tmp<-list(modeledData = model$modeledData[model$modeledData$samplingLocation==k,])
        ggplotSmoothMap(tmp,shp,k)
      }
    }
  }

  
#############################################
##### trying out interaction of two factors ########
#############################################

  # get samplingLocations and hasFever
  queryIn <- list(
    SELECT   =list(COLUMN=c('hasFever','samplingLocation','GEOID')),
    GROUP_BY =list(COLUMN=c('hasFever','samplingLocation','GEOID')),
    SUMMARIZE=list(COLUMN='samplingLocation', IN= "all")
  )
  db <- expandDB(selectFromDB(queryIn))
  
  modelDefinition <- smoothModel(db=db, shp=shp)
  model <- modelTrainR(modelDefinition)
  
  saveModel(model, cloudDir = 'C:/Users/mfamulare/Dropbox (IDM)/SeattleFlu-incidenceMapR/models')
  
  
  # plot
  for(k in unique(model$modeledData$samplingLocation)){
    for(n in unique(model$modeledData$hasFever)){
      tmp<-list(modeledData = model$modeledData[model$modeledData$samplingLocation==k & model$modeledData$hasFever==n,])
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
    tmp<-db$observedData %>% group_by(age) %>% summarize(n=sum(n))
    db$observedData <- db$observedData %>% select(-n) %>% left_join(tmp,by='age')

  db <- expandDB(db)

  modelDefinition <- smoothModel(db=db, shp=shp)
  model <- modelTrainR(modelDefinition)
  summary(model$inla)
  
  saveModel(model, cloudDir = 'C:/Users/mfamulare/Dropbox (IDM)/SeattleFlu-incidenceMapR/models')
    
  ggplot(model$modeledData) + geom_line(aes(x=ageBin,y=fitted.values.mean, group=pathogen)) + geom_point(aes(x=ageBin,y=positive/n, group=pathogen)) + facet_wrap("pathogen")
    
