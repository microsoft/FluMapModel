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
# 
# pathogens <- db$observedData %>% group_by(pathogen) %>% summarize(n = n()) %>% arrange(desc(n))


fluPathogens <- c('Flu_A_H1','Flu_A_H3','Flu_A_pan','Flu_B_pan')

factors   <- c('site_type','age_range_coarse')

geoLevels <- list( # seattle_geojson = c('residence_neighborhood_district_name'),
                   king_county_geojson = c('residence_puma')#,'residence_census_tract')
                 )

#####################################
###### DeDx smooth model ############
#####################################

for (SOURCE in names(geoLevels)){
  for (GEO in geoLevels[[SOURCE]]){

    shp <- masterSpatialDB(shape_level = gsub('residence_','',GEO), source = SOURCE)
    
    queryIn <- list(
      SELECT   =list(COLUMN=c('pathogen',factors, GEO,'encountered_week')),
      GROUP_BY =list(COLUMN=c(factors,GEO,'encountered_week','age_range_coarse')),
      SUMMARIZE=list(COLUMN='pathogen', IN= fluPathogens)
    )
    
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
          
          dir.create('/home/rstudio/seattle_flu/model_diagnostic_plots/', showWarinngs = FALSE)
          fname <- paste('/home/rstudio/seattle_flu/model_diagnostic_plots/',paste('DeDx',SOURCE,GEO,'age_range_coarse_upper','encountered_week',sep='-'),'.png',sep='')
          png(filename = fname,width = 6, height = 5, units = "in", res = 300)
          print(
            ggplot(model$modeledData %>% filter(site_type == "hospital")) +
                  geom_line(aes_string(x='encountered_week',y="modeled_fraction_mean", color="age_range_coarse_upper",group ="age_range_coarse_upper")) +
                  facet_wrap(GEO) +
                  # geom_ribbon(aes_string(x='encountered_week',ymin="modeled_intensity_lower_95_CI", ymax="modeled_intensity_upper_95_CI", fill=GEO,group =GEO),alpha=0.1) +
                  guides(color=FALSE) +
                  theme(axis.text.x = element_text(angle = 90, hjust = 1))
            )
          dev.off()
          
          success<-1
          
        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
      )
    }
  }
}
