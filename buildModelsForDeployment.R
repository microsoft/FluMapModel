# buildModelsForDeployment.R
# this script (and similar others?) controls standardized database queries and model training for web deployment

library(dbViewR)
library(incidenceMapR)
library(modelServR)
library(modelTestR)
library(dplyr)
library(ggplot2)

SRC <- 'production'
# SRC <- 'simulated_data'


#####################
### REAL DATA #######
#####################

db <- selectFromDB(queryIn= list(SELECT  =c("*")), source = SRC)

pathogens <- c('all', unique(db$observedData$pathogen))
factors   <- c('site_type','sex','flu_shot')

geoLevels <- list( seattle_geojson = c('residence_puma','residence_neighborhood_district_name','residence_cra_name','residence_census_tract'),
                   wa_geojson = c('residence_puma'), # census tract impossible due to memory limits
                   king_county_geojson = c('residen#ce_census_tract')
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
        
        shp <- masterSpatialDB(shape_level = gsub('residence_','',GEO), source = SOURCE)
        
        db <- expandDB( selectFromDB(  queryIn, source=SRC, na.rm=TRUE ), shp=shp )
        
        modelDefinition <- smoothModel(db=db, shp=shp)
        
        # training occassionaly segfaults but it does not appear to be deterministic...
        tryCatch(
          {
            model <- modelTrainR(modelDefinition)
            
            print(summary(model$inla))
            
            saveModel(model)
            
            dir.create('/home/rstudio/seattle_flu/plots/', showWarnings = FALSE)
            for(k in unique(model$modeledData[[FACTOR]])){
              tmp<-list(modeledData = model$modeledData[model$modeledData[[FACTOR]]==k,])
              fname <- paste('/home/rstudio/seattle_flu/plots/',paste(PATHOGEN,SOURCE,GEO,FACTOR,k,sep='-'),'.png',sep='')
              png(filename = fname,width = 6, height = 5, units = "in", res = 300)
              print(ggplotSmoothMap(tmp,shp,title=k,shape_level = GEO))
              dev.off()
            }
            
          }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
        )
        
      }
    }
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
        GROUP_BY =list(COLUMN=c(factors,GEO,"encountered_week")),
        SUMMARIZE=list(COLUMN='pathogen', IN= PATHOGEN)
      )
      
      shp <- masterSpatialDB(shape_level = gsub('residence_','',GEO), source = SOURCE)
      
      db <- expandDB( selectFromDB(  queryIn, source=SRC, na.rm=TRUE ), shp=shp )
      
      # training occassionaly segfaults on but it does not appear to be deterministic...
      tries <- 0
      success<-0
      while (success==0 & tries<=2){
        tries <- tries+1
        tryCatch(
          {
            
            db <- appendCatchmentModel(db,shp=shp, source=SRC, na.rm=TRUE  )
            
            modelDefinition <- latentFieldModel(db=db, shp=shp)
            model <- modelTrainR(modelDefinition)
            
            print(summary(model$inla))
            
            saveModel(model)
            
            dir.create('/home/rstudio/seattle_flu/plots/', showWarnings = FALSE)
            fname <- paste('/home/rstudio/seattle_flu/plots/',paste(PATHOGEN,SOURCE,paste(factors,collapse='-'),GEO,'encountered_week',sep='-'),'.png',sep='')
            png(filename = fname,width = 6, height = 5, units = "in", res = 300)
            print(ggplot(model$latentField) + 
                    geom_line(aes_string(x='encountered_week',y="modeled_intensity_mode", color=GEO,group =GEO)) + 
                    # geom_ribbon(aes_string(x='encountered_week',ymin="modeled_intensity_lower_95_CI", ymax="modeled_intensity_upper_95_CI", fill=GEO,group =GEO),alpha=0.1) + 
                    guides(color=FALSE) )
            dev.off()
            
            success<-1
            
          }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
        )
      }
    }
  }
}


###########################################
# age-distributions by pathogen and factor#
###########################################
# eventually this should be multinomial models, but indepdendent binomial for now
for (PATHOGEN in pathogens){
  for (FACTOR in factors){
    
    queryIn <- list(
      SELECT   =list(COLUMN=c('pathogen',FACTOR,'age_range_fine_lower')),
      WHERE    =list(COLUMN='pathogen', IN= PATHOGEN),
      GROUP_BY =list(COLUMN=c(FACTOR,'age_range_fine_lower')),
      SUMMARIZE=list(COLUMN='pathogen', IN= 'all')
    )
    
    db<- selectFromDB(  queryIn, source=SRC, na.rm=TRUE  ) 
    
    # get all ages denominator. I'm not sure how to implement this as single query..
    tmp <- selectFromDB(  queryIn[c('SELECT','GROUP_BY','SUMMARIZE')], source=SRC, na.rm=TRUE  )$observedData %>% 
      group_by_(.dots=c(FACTOR,'age_range_fine_lower')) %>% summarize(n=sum(n))
    db$observedData <- db$observedData %>% select(-n) %>% left_join(tmp)
    
    db <- expandDB(db)
    
    modelDefinition <- smoothModel(db=db, shp=shp)
    model <- modelTrainR(modelDefinition)
    print(summary(model$inla))
    
    saveModel(model)
    
    if (model$modelDefinition$family[1]=='poisson'){
      p1<-ggplot(model$modeledData) + geom_line(aes(x=age_range_fine_lower ,y=modeled_count_mode, group=pathogen)) + 
        geom_point(aes(x=age_range_fine_lower, y=positive, group=pathogen)) + 
        geom_ribbon(aes(x=age_range_fine_lower ,ymin=modeled_count_lower_95_CI, ymax=modeled_count_upper_95_CI  , group=pathogen)) + 
        facet_wrap(FACTOR) +
        ylim(c(0,2*max(model$modeledData$modeled_count_mode)))
    } else if (model$modelDefinition$family[1]=='binomial'){
      p1<-ggplot(model$modeledData) + geom_line(aes(x=age_range_fine_lower ,y=modeled_fraction_mode, group=pathogen)) + 
        geom_point(aes(x=age_range_fine_lower, y=positive/n, group=pathogen)) + 
        geom_ribbon(aes(x=age_range_fine_lower ,ymin=modeled_fraction_lower_95_CI, ymax=modeled_fraction_upper_95_CI  , group=pathogen)) + 
        facet_wrap(FACTOR) +
        ylim(c(0,1))
    }
    
    dir.create('/home/rstudio/seattle_flu/plots/', showWarnings = FALSE)
    fname <- paste('/home/rstudio/seattle_flu/plots/',paste(PATHOGEN,FACTOR,'age_range_fine',sep='-'),'.png',sep='')
    png(filename = fname,width = 6, height = 5, units = "in", res = 300)
    print(p1)
    dev.off()
  }
}

