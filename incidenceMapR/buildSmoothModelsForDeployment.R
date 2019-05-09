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
  SELECT   =list(COLUMN=c('site_type','residence_census_tract')),
  WHERE   =list(COLUMN='site_type', IN = c('kiosk')),
  GROUP_BY =list(COLUMN=c('site_type','residence_census_tract')),
  SUMMARIZE=list(COLUMN='site_type', IN= c('kiosk'))
)
db <- expandDB( selectFromDB(  queryIn ) )

modelDefinition <- smoothModel(db=db, shp=shp)
model <- modelTrainR(modelDefinition)

ggplotSmoothMap(model,shp,'kiosk')

queryIn <- list(
  SELECT   =list(COLUMN=c('site_type','residence_neighborhood_district_name')),
  WHERE   =list(COLUMN='site_type', IN = c('kiosk')),
  GROUP_BY =list(COLUMN=c('site_type','residence_neighborhood_district_name')),
  SUMMARIZE=list(COLUMN='site_type', IN= c('kiosk'))
)
db <- expandDB( selectFromDB(  queryIn ) )

modelDefinition <- smoothModel(db=db, shp=shp)
model <- modelTrainR(modelDefinition)

ggplotSmoothMap(model,shp,title= 'kiosk', shape_level = "residence_neighborhood_district_name" )

# cache model object
# this needs to handle multiple model types and point to accessible data store
# saveModel(model)



# test: real childrensHospital data
queryIn <- list(
  SELECT   =list(COLUMN=c('site_type','residence_cra_name')),
  WHERE   =list(COLUMN='site_type', IN = c('childrensHospital')),
  GROUP_BY =list(COLUMN=c('site_type','residence_cra_name')),
  SUMMARIZE=list(COLUMN='site_type', IN= c('all'))
)
db <- expandDB( selectFromDB(  queryIn, source='production', na.rm=TRUE ) )

shp<-masterSpatialDB(shape_level = 'cra_name', source = 'seattle_geojson')

modelDefinition <- smoothModel(db=db, shp=shp)
model <- modelTrainR(modelDefinition)

ggplotSmoothMap(model,shp,'childrensHospital', shape_level = 'residence_cra_name')


##################################################
#########   catchment maps #######################
##################################################

# find catchment maps for each site_type and geoLevel
  geoLevels <- c('residence_census_tract','residence_puma','residence_cra_name','residence_neighborhood_district_name')

  for(geo in geoLevels){
    queryIn <- list(
      SELECT   =list(COLUMN=c('site_type',geo)),
      GROUP_BY =list(COLUMN=c('site_type',geo)),
      SUMMARIZE=list(COLUMN='site_type', IN= 'all')
    )
    db <- expandDB( selectFromDB(  queryIn ) )
    
    shp <- masterSpatialDB(shape_level = gsub('residence_','',geo), source = 'seattle_geojson')
    
    modelDefinition <- smoothModel(db=db, shp=shp)
    model <- modelTrainR(modelDefinition)
    summary(model$inla)
        
    # saveModel(model, cloudDir = './data')
    
    # if (geo =='residence_census_tract'){
      for(k in unique(model$modeledData$site_type)){
        tmp<-list(modeledData = model$modeledData[model$modeledData$site_type==k,])
        ggplotSmoothMap(tmp,shp,title = k, shape_level = geo)
      }
    # }
  }

  
#####################
### REAL DATA #######
#####################

# find catchment maps for each site_type and geoLevel
geoLevels <- c('residence_census_tract','residence_puma','residence_cra_name','residence_neighborhood_district_name')

for(geo in geoLevels){
  queryIn <- list(
    SELECT   =list(COLUMN=c('site_type',geo)),
    GROUP_BY =list(COLUMN=c('site_type',geo)),
    SUMMARIZE=list(COLUMN='site_type', IN= 'all')
  )
  db <- expandDB( selectFromDB(  queryIn, source='production', na.rm=TRUE ) )
  
  shp <- masterSpatialDB(shape_level = gsub('residence_','',geo), source = 'seattle_geojson')
  
  modelDefinition <- smoothModel(db=db, shp=shp)
  model <- modelTrainR(modelDefinition)
  summary(model$inla)
  
  # saveModel(model, cloudDir = './data')
  
  # if (geo =='residence_census_tract'){
    for(k in unique(model$modeledData$site_type)){
      tmp<-list(modeledData = model$modeledData[model$modeledData$site_type==k,])
      ggplotSmoothMap(tmp,shp,title=k,shape_level = geo)
    }
  # }
}


#############################################
##### trying out interaction of two factors ########
#############################################

  # get site_types and has_fever
  queryIn <- list(
    SELECT   =list(COLUMN=c('has_fever','site_type','residence_census_tract')),
    GROUP_BY =list(COLUMN=c('has_fever','site_type','residence_census_tract')),
    SUMMARIZE=list(COLUMN='site_type', IN= "all")
  )
  db <- expandDB(selectFromDB(queryIn))
  
  modelDefinition <- smoothModel(db=db, shp=shp)
  model <- modelTrainR(modelDefinition)

  # plot
  for(k in unique(model$modeledData$site_type)){
    for(n in unique(model$modeledData$has_fever)){
      tmp<-list(modeledData = model$modeledData[model$modeledData$site_type==k & model$modeledData$has_fever==n,])
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
    
