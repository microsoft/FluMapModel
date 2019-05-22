# buildModelsForDeployment.R
# this script (and similar others?) controls standardized database queries and model training for web deployment

library(dbViewR)
library(incidenceMapR)
library(modelServR)
library(modelTestR)
library(dplyr)
library(tidyr)
library(ggplot2)

SRC <- 'production'
# SRC <- 'simulated_data'

db <- selectFromDB(queryIn= list(SELECT  =c("*")), source = SRC) 

dim(db$observedData %>% nest(-encounter))[1]

dim(db$observedData)[1]-sum(is.na(db$observedData$residence_census_tract))

db$observedData <- db$observedData[!is.na(db$observedData$residence_census_tract),]
dim(db$observedData)

tmp<-db$observedData %>% nest(-encounter)
dim(tmp)
dim(tmp)[1]-sum((tmp %>% unnest())$number_pathogens_tested==0)

pathogens <- db$observedData %>% group_by(pathogen) %>% summarize(n = n()) %>% arrange(desc(n))
pathogens$pathogen
pathogens$n
sum(pathogens$n[pathogens$pathogen != 'not_yet_tested'])
pathogens <- c('all',pathogens$pathogen[pathogens$n >= 50 | grepl('Flu',pathogens$pathogen)])

pathogens<-c('all','Flu_A_H1','Flu_A_H3')

fluVax <- db$observedData %>% group_by(flu_shot) %>% summarize(n=n())
1-fluVax$n[1]/sum(fluVax$n[c(1:2)])



##############################
## time-independent maps #####
##############################
factors   <- c('all','site_type')

geoLevels <- list( seattle_geojson = c('residence_neighborhood_district_name','residence_census_tract'),
                   king_county_geojson = c('residence_puma'),
                   wa_geojson = c('residence_puma') # census tract impossible due to memory limits
)

# catchments: number of subjects with pathogen and factor at residence location 
for (SOURCE in names(geoLevels)){
  for (PATHOGEN in pathogens){
    for (FACTOR in factors){
      for (GEO in geoLevels[[SOURCE]]){
        
        if(FACTOR=='all'){
          queryIn <- list(
            SELECT   =list(COLUMN=c('pathogen', GEO)),
            WHERE    =list(COLUMN='pathogen', IN=PATHOGEN),
            GROUP_BY =list(COLUMN=GEO),
            SUMMARIZE=list(COLUMN='pathogen', IN= 'all')
          )
        } else {
          queryIn <- list(
            SELECT   =list(COLUMN=c('pathogen', FACTOR, GEO)),
            WHERE    =list(COLUMN='pathogen', IN=PATHOGEN),
            GROUP_BY =list(COLUMN=c(FACTOR,GEO)),
            SUMMARIZE=list(COLUMN=FACTOR, IN= 'all')
          )
        }
        
        shp <- masterSpatialDB(shape_level = gsub('residence_','',GEO), source = SOURCE)
        
        db <- expandDB( selectFromDB(  queryIn, source=SRC, na.rm=TRUE ), shp=shp )
        
        modelDefinition <- smoothModel(db=db, shp=shp)
        
        # training occassionaly segfaults but it does not appear to be deterministic...
        tryCatch(
          {
            model <- modelTrainR(modelDefinition)
            
            print(summary(model$inla))
            
            saveModel(model)
            
            if(FACTOR == 'all'){
              dir.create('/home/rstudio/seattle_flu/may_22_plots/', showWarnings = FALSE)
              fname <- paste('/home/rstudio/seattle_flu/may_22_plots/',paste('inla_observed',PATHOGEN,SOURCE,GEO,sep='-'),'.png',sep='')
              png(filename = fname,width = 6, height = 5, units = "in", res = 300)
              ggplotSmoothMap(model,shp,title=FACTOR,shape_level = GEO)
              dev.off()
            }else {
              for(k in unique(model$modeledData[[FACTOR]])){
                tmp<-list(modeledData = model$modeledData[model$modeledData[[FACTOR]]==k,])
                dir.create('/home/rstudio/seattle_flu/may_22_plots/', showWarnings = FALSE)
                fname <- paste('/home/rstudio/seattle_flu/may_22_plots/',paste('inla_observed',PATHOGEN,SOURCE,GEO,FACTOR,k,sep='-'),'.png',sep='')
                png(filename = fname,width = 6, height = 5, units = "in", res = 300)
                ggplotSmoothMap(tmp,shp,title=k,shape_level = GEO)
                dev.off()
              }
            }
            
          }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
        )
        
      }
    }
  }
}

#####################################
###### timeseries latent field models ############
#####################################

factors   <- c('site_type','sex','flu_shot')

geoLevels <- list( seattle_geojson = c('residence_neighborhood_district_name')#,
                   # king_county_geojson = c('residence_puma'),
                   # wa_geojson = c('residence_puma') # census tract impossible due to memory limits
)

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
            
            dir.create('/home/rstudio/seattle_flu/may_22_plots/', showWarnings = FALSE)
            fname <- paste('/home/rstudio/seattle_flu/may_22_plots/',paste('inla_latent',PATHOGEN,SOURCE,GEO,'encountered_week',sep='-'),'.png',sep='')
            png(filename = fname,width = 8, height = 5, units = "in", res = 300)
            print(ggplot(model$latentField) + 
                    geom_line(aes_string(x='encountered_week',y="modeled_intensity_mode", color=GEO,group =GEO)) + 
                    # geom_ribbon(aes_string(x='encountered_week',ymin="modeled_intensity_lower_95_CI", ymax="modeled_intensity_upper_95_CI", fill=GEO,group =GEO),alpha=0.1) + 
                    # guides(color=FALSE) + 
                    theme(axis.text.x = element_text(angle = 90, hjust = 1)))
            dev.off()
            
            fname <- paste('/home/rstudio/seattle_flu/may_22_plots/',paste('inla_latent',PATHOGEN,SOURCE,GEO,'median_encountered_week',sep='-'),'.png',sep='')
            png(filename = fname,width = 8, height = 5, units = "in", res = 300)
            ggplotLatentMap(model,shp,title='',shape_level = GEO)
            dev.off()
            
            success<-1
            
          }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
        )
      }
    }
  }
}



#####################################
###### smooth timeseries models that match latent_field (no factors) ############
#####################################

# number of subjects with pathogen and factor at residence location 
for (SOURCE in names(geoLevels)){
  for (PATHOGEN in pathogens){
    for (GEO in geoLevels[[SOURCE]]){
      
      queryIn <- list(
        SELECT   =list(COLUMN=c('pathogen', GEO,'encountered_week')),
        WHERE    =list(COLUMN='pathogen', IN=PATHOGEN),
        GROUP_BY =list(COLUMN=c(GEO,"encountered_week")),
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
            
            modelDefinition <- smoothModel(db=db, shp=shp)
            model <- modelTrainR(modelDefinition)
            
            print(summary(model$inla))
            
            saveModel(model)
            
            dir.create('/home/rstudio/seattle_flu/may_22_plots/', showWarnings = FALSE)
            fname <- paste('/home/rstudio/seattle_flu/may_22_plots/',paste('inla_observed',PATHOGEN,SOURCE,GEO,'encountered_week',sep='-'),'.png',sep='')
            png(filename = fname,width = 8, height = 5, units = "in", res = 300)
            print(ggplot(model$modeledData) + 
                    geom_line(aes_string(x='encountered_week',y="modeled_count_mode", color=GEO,group =GEO)) + 
                    # geom_line(aes_string(x='encountered_week',y="positive", color=GEO,group =GEO),size=0.1) + 
                    # geom_ribbon(aes_string(x='encountered_week',ymin="modeled_count_lower_95_CI", ymax="modeled_count_upper_95_CI", fill=GEO,group =GEO),alpha=0.1) + 
                    # guides(color=FALSE) + 
                    theme(axis.text.x = element_text(angle = 90, hjust = 1)))
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
        geom_ribbon(aes(x=age_range_fine_lower ,ymin=modeled_count_lower_95_CI, ymax=modeled_count_upper_95_CI  , group=pathogen), alpha=0.3) + 
        facet_wrap(FACTOR) +
        ylim(c(0,2*max(model$modeledData$modeled_count_mode)))
    } else if (model$modelDefinition$family[1]=='binomial'){
      p1<-ggplot(model$modeledData) + geom_line(aes(x=age_range_fine_lower ,y=modeled_fraction_mode, group=pathogen)) + 
        geom_point(aes(x=age_range_fine_lower, y=positive/n, group=pathogen)) + 
        geom_ribbon(aes(x=age_range_fine_lower ,ymin=modeled_fraction_lower_95_CI, ymax=modeled_fraction_upper_95_CI  , group=pathogen), alpha=0.3) + 
        facet_wrap(FACTOR) +
        ylim(c(0,1))
    }
    
    dir.create('/home/rstudio/seattle_flu/may_22_plots/', showWarnings = FALSE)
    fname <- paste('/home/rstudio/seattle_flu/may_22_plots/',paste('inla_observed',PATHOGEN,FACTOR,'age_range_fine',sep='-'),'.png',sep='')
    png(filename = fname,width = 6, height = 5, units = "in", res = 300)
    print(p1)
    dev.off()
  }
}
