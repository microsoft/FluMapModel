# buildModelsForDeployment.R
# this script (and similar others?) controls standardized database queries and model training for web deployment

library(dbViewR)
library(incidenceMapR)
library(modelServR)
library(modelTestR)
library(dplyr)
library(ggplot2)


## factors and levels to be modeled ##

#####################
### REAL DATA #######
#####################

db <- selectFromDB( list(SELECT  =c("*")), source = 'production')

pathogens <- c('all', unique(db$observedData$pathogen))
factors   <- c('site_type','sex','flu_shot')

geoLevels <- list( seattle_geojson = c('residence_puma','residence_neighborhood_district_name','residence_cra_name','residence_census_tract'),
                   wa_geojson = c('residence_puma'), # census tract impossible due to memory limits
                   king_county_geojson = c('residence_puma','residence_census_tract')
                 )

##############################
## time-independent maps #####
##############################

# catchments: number of subjects with pathogen and factor at residence location 
for (SOURCE in names(geoLevels)){
  for (PATHOGEN in pathogens){
    for (FACTOR in factors){
      for (GEO in geoLevels[[SOURCE]]){
        
        queryIn <- list(
          SELECT   =list(COLUMN=c('pathogen', FACTOR, GEO)),
          WHERE    =list(COLUMN='pathogen', IN=PATHOGEN),
          GROUP_BY =list(COLUMN=c(FACTOR,GEO)),
          SUMMARIZE=list(COLUMN=FACTOR, IN= 'all')
        )
        db <- expandDB( selectFromDB(  queryIn, source='production', na.rm=TRUE ) )
        
        shp <- masterSpatialDB(shape_level = gsub('residence_','',GEO), source = SOURCE)
        
        modelDefinition <- smoothModel(db=db, shp=shp)
        model <- modelTrainR(modelDefinition)
        print(summary(model$inla))
        
        saveModel(model)
        
        for(k in unique(model$modeledData[[FACTOR]])){
          tmp<-list(modeledData = model$modeledData[model$modeledData[[FACTOR]]==k,])
          fname <- paste('/home/rstudio/seattle_flu/data/plots/',paste(PATHOGEN,SOURCE,GEO,FACTOR,k,sep='-'),'.png',sep='')
          png(filename = fname,width = 6, height = 5, units = "in", res = 300)
          print(ggplotSmoothMap(tmp,shp,title=k,shape_level = GEO))
          dev.off()
        }
        
      }
    }
  }
}

# age-distributions by pathogen and factor
# eventually this should be multinomial models, but indepdendent binomial for now

### BROKEN BECAUSE NEED TO PROPOGATE AGE_RANGE from DATABASE THROUGH SYSTEM

for (PATHOGEN in pathogens){
  for (FACTOR in factors){
    
    queryIn <- list(
      SELECT   =list(COLUMN=c('pathogen',FACTOR,'age_range_fine_lower')),
      GROUP_BY =list(COLUMN=c('pathogen',FACTOR,'age_range_fine_lower')),
      SUMMARIZE=list(COLUMN='pathogen', IN= PATHOGEN)
    )
    # queryIn <- list(
    #   SELECT   =list(COLUMN=c(FACTOR,'age_range_fine_lower')),
    #   GROUP_BY =list(COLUMN=c(FACTOR,'age_range_fine_lower')),
    #   SUMMARIZE=list(COLUMN=FACTOR, IN= 'all')
    # )
    db<- selectFromDB(  queryIn, source='production', na.rm=TRUE  ) 

    # get all ages denominator. I'm not sure how to implement this as single query..
    tmp<-db$observedData %>% group_by_(.dots=c(FACTOR,'age_range_fine_lower')) %>% summarize(n=sum(n))
    db$observedData <- db$observedData %>% select(-n) %>% left_join(tmp)
    
    db <- expandDB(db)
    
    modelDefinition <- smoothModel(db=db, shp=shp)
    model <- modelTrainR(modelDefinition)
    print(summary(model$inla))
    
    saveModel(model)
    
    p1<-ggplot(model$modeledData) + geom_line(aes(x=age_range_fine_lower ,y=modeled_count_mode, group=pathogen)) + 
      geom_point(aes(x=age_range_fine_lower, y=positive, group=pathogen)) + 
      geom_ribbon(aes(x=age_range_fine_lower ,ymin=modeled_count_0_025quant, ymax=modeled_count_0_975quant  , group=pathogen)) + 
      facet_wrap(FACTOR) +
      ylim(c(0,2*max(model$modeledData$modeled_count_mode)))
    
    fname <- paste('/home/rstudio/seattle_flu/data/plots/',paste(PATHOGEN,FACTOR,'age_range_fine',sep='-'),'.png',sep='')
    png(filename = fname,width = 6, height = 5, units = "in", res = 300)
    print(p1)
    dev.off()
  }
}


#####################################
###### timeseries models ############
#####################################

# number of subjects with pathogen and factor at residence location 
for (SOURCE in names(geoLevels)){
  for (PATHOGEN in pathogens){
    for (GEO in geoLevels[[SOURCE]]){
      
      queryIn <- list(
        SELECT   =list(COLUMN=c('pathogen', factors, GEO,'encountered_week')),
        WHERE    =list(COLUMN='pathogen', IN=PATHOGEN),
        GROUP_BY =list(COLUMN=c('pathogen',factors,GEO,"encountered_week")),
        SUMMARIZE=list(COLUMN='pathogen', IN= PATHOGEN)
      )
      
      shp <- masterSpatialDB(shape_level = gsub('residence_','',GEO), source = SOURCE)
      
      db <- expandDB( selectFromDB(  queryIn, source='production', na.rm=TRUE ), shp=shp )
      
      db <- appendCatchmentModel(db,shp=shp, source='production', na.rm=TRUE  )
      
      modelDefinition <- latentFieldModel(db=db, shp=shp)
      model <- modelTrainR(modelDefinition)
      
      print(summary(model$inla))
      
      saveModel(model)
      fname <- paste('/home/rstudio/seattle_flu/data/plots/',paste(PATHOGEN,SOURCE,paste(factors,collapse='-'),GEO,'encountered_week',sep='-'),'.png',sep='')
      png(filename = fname,width = 6, height = 5, units = "in", res = 300)
      print(ggplot(model$latentField) + geom_line(aes_string(x='encountered_week',y="modeled_intensity_mode", color=GEO,group =GEO)) )
      dev.off()
    }
  }
}

# change offset standardization