# buildModelsForDeployment.R
# this script (and similar others?) controls standardized database queries and model training for web deployment

library(dbViewR)
library(incidenceMapR)
library(modelServR)
library(modelVisualizeR)
library(dplyr)
library(tidyr)
library(ggplot2)

SRC <- 'production'
# SRC <- 'simulated_data'

db <- selectFromDB(queryIn= list(SELECT  =c("*")), source = SRC)

pathogens <- db$observedData %>% group_by(pathogen) %>% summarize(n = n()) %>% arrange(desc(n))
pathogens <- c('all',pathogens$pathogen[pathogens$n >= 50 | grepl('Flu',pathogens$pathogen)])

factors   <- c('site_type','sex','flu_shot')

geoLevels <- list( seattle_geojson = c('residence_puma','residence_neighborhood_district_name','residence_cra_name','residence_census_tract'),
                   wa_geojson = c('residence_puma')#, # census tract impossible due to memory limits
                   #king_county_geojson = c('residence_census_tract')
                 )


# geoLevels <- list( seattle_geojson = c('residence_neighborhood_district_name'))

#####################################
###### timeseries latent field models ############
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
            
            dir.create('/home/rstudio/seattle_flu/model_diagnostic_plots/', showWarnings = FALSE)
            fname <- paste('/home/rstudio/seattle_flu/model_diagnostic_plots/',paste('inla_latent',PATHOGEN,SOURCE,GEO,'encountered_week',sep='-'),'.png',sep='')
            png(filename = fname,width = 6, height = 5, units = "in", res = 300)
            print(ggplot(model$latentField) + 
                    geom_line(aes_string(x='encountered_week',y="modeled_intensity_mode", color=GEO,group =GEO)) + 
                    # geom_ribbon(aes_string(x='encountered_week',ymin="modeled_intensity_lower_95_CI", ymax="modeled_intensity_upper_95_CI", fill=GEO,group =GEO),alpha=0.1) + 
                    guides(color=FALSE) + 
                    theme(axis.text.x = element_text(angle = 90, hjust = 1)))
            dev.off()
            
            success<-1
            
          }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
        )
      }
    }
  }
}

