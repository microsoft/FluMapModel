# testModelTrainR
# script to test incidenceMapR package

library(dbViewR)
library(incidenceMapR)
library(modelVisualizeR)

shp <- masterSpatialDB()  # census-tract shapefiles

# ggplot build eventually will be replaced by function ggplotSmoothSequential
library(ggplot2)
plotSettings <- ggplot() + theme_bw() +  theme(panel.border = element_blank()) + xlab('')

###################################
##### smoothing models ############
###################################

## simulated data
queryIn <- list(
  SELECT   =list(COLUMN=c('site_type','residence_census_tract')),
  WHERE    =list(COLUMN='site_type', IN = c('kiosk')),
  GROUP_BY =list(COLUMN=c('site_type','residence_census_tract')),
  SUMMARIZE=list(COLUMN='site_type', IN= c('kiosk'))
)
db <- expandDB( selectFromDB(  queryIn ) )

#smooth model
modelDefinition <- smoothModel(db=db, shp=shp)
model <- modelTrainR(modelDefinition)
ggplotSmoothMap(model,shp)
ggplotDiagnosticPlots(model) #plot marginals and residuals


####################################################
##### fixed effects models: bing queries ############
####################################################
## simulated data
queryIn <- list(
  SELECT   =list(COLUMN=c('site_type','residence_census_tract')),
  WHERE    =list(COLUMN='site_type', IN = c('kiosk')),
  GROUP_BY =list(COLUMN=c('site_type','residence_census_tract')),
  SUMMARIZE=list(COLUMN='site_type', IN= c('kiosk'))
)
db <- expandDB( selectFromDB(  queryIn ) )

#get the number of bing queries per tract
bing_queries_pertract <- loadBingData(geography = "tract", 
                                      bing_queries_path = 'C:/Users/grhuynh/FluMapModel')
#or add bing queries as random numbers from 0 to max positives
#searchdb$observedData$num_bing_queries <- runif(nrow(searchdb$observedData), min = 0, max = max(searchdb$observedData$positive)) #add random numbers between 0 and max #of positives as the bing weights 

#or add bing queries as the same values as positive 
#searchdb$observedData$num_bing_queries <- searchdb$observedData$positive 


#join to the observed data
searchdb <- db
library(dplyr)
searchdb$observedData <- inner_join(searchdb$observedData, bing_queries_pertract, by = c("residence_census_tract" = "GEOID"))

#run the fixed effects model
searchdb$observedData$site_type <- NULL #this messes up the fixed effects model because it has only 1 level
modelDefinition <- effectsModel(db=searchdb, shp=shp)
model_fixedeffects <- modelTrainR(modelDefinition)

#plot just the fixed effects model
ggplotFixedEffects(model_fixedeffects,shp)
ggplotDiagnosticPlots(model_fixedeffects) #the residuals plot doesn't work

####################################################
##### compare smooth and fixed effects models ############
####################################################
#run all above code
ggplotSmoothEffects(model, model_fixedeffects,shp)
