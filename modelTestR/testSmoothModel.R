# testModelTrainR
# script to test incidenceMapR package

library(dbViewR)
library(incidenceMapR)
library(modelTestR)
library(dplyr)

shp <- masterSpatialDB()  # census-tract shapefiles

# ggplot build eventually will be replaced by function ggplotSmoothSequential
library(ggplot2)
plotSettings <- ggplot() + theme_bw() +  theme(panel.border = element_blank()) + xlab('')


###################################
##### smoothing models ############
###################################

# simulated data kiosk catchment map
queryIn <- list(
  SELECT   =list(COLUMN=c('site_type','residence_census_tract')),
  WHERE    =list(COLUMN='site_type', IN = c('kiosk')),
  GROUP_BY =list(COLUMN=c('site_type','residence_census_tract')),
  SUMMARIZE=list(COLUMN='site_type', IN= c('kiosk'))
)
db <- expandDB( selectFromDB(  queryIn ) )

modelDefinition <- smoothModel(db=db, shp=shp)
model <- modelTrainR(modelDefinition)
ggplotSmoothMap(model,shp)

#OR plot as fraction of census population in each tract
censusdb <- addCensusData(db = db, source = "decennial", variable = c("B01003_001", "B09001_001", "P005004"), year = 2010, credentials_path = 'C:/Users/grhuynh')
ggplotSmoothMap(model,shp, censusdb = censusdb)




## simulated data with bing query search data, run fixed effects model
queryIn <- list(
  SELECT   =list(COLUMN=c('site_type','residence_census_tract')),
  WHERE    =list(COLUMN='site_type', IN = c('kiosk')),
  GROUP_BY =list(COLUMN=c('site_type','residence_census_tract')),
  SUMMARIZE=list(COLUMN='site_type', IN= c('kiosk'))
)
db <- expandDB( selectFromDB(  queryIn ) )
modelDefinition <- smoothModel(db=db, shp=shp)
model <- modelTrainR(modelDefinition)


#ADD bing query search data and run fixed effects model
searchdb <- db
searchdb$observedData$site_type <- NULL #this messes up the fixed effects model because it has only 1 level

#add bing queries as random numbers from 0 to max positives
#searchdb$observedData$query_weight <- runif(nrow(searchdb$observedData), min = 0, max = max(searchdb$observedData$positive)) #add random numbers between 0 and max #of positives as the bing weights 

#add bing queries as the same values as positive 
searchdb$observedData$query_weight <- searchdb$observedData$positive 

#run the model
modelDefinition <- effectsModel(db=searchdb, shp=shp)
model_search <- modelTrainR(modelDefinition)

#compare values just for looking at the table
temp_search <- select(model_search$modeledData, residence_census_tract, query_weight, positive, modeled_count_mode)
temp_smooth <- select(model$modeledData, residence_census_tract, positive, modeled_count_mode)
joined_smoothsame <- left_join(temp_smooth, temp_search, by = "residence_census_tract")

ggplotSmoothEffects(model, model_search,shp)




# simulated data at_home catchment map
queryIn <- list(
  SELECT   =list(COLUMN=c('site_type','residence_census_tract')),
  WHERE    =list(COLUMN='site_type', IN = c('at_home')),
  GROUP_BY =list(COLUMN=c('site_type','residence_census_tract')),
  SUMMARIZE=list(COLUMN='site_type', IN= c('at_home'))
)
db <- expandDB( selectFromDB(  queryIn ) )

modelDefinition <- smoothModel(db=db, shp=shp)
model <- modelTrainR(modelDefinition)
ggplotSmoothMap(model,shp)



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




######################
########### age ######
######################

# simulated data h1n1pdm age fraction

queryIn <- list(
  SELECT   =list(COLUMN=c('pathogen','age')),
  MUTATE   =list(COLUMN='age', AS='age_bin'),
  GROUP_BY =list(COLUMN=c('age_bin')),
  SUMMARIZE=list(COLUMN='pathogen', IN= 'h1n1pdm')
)
db<-selectFromDB(  queryIn )
db <- expandDB( db )

modelDefinition <- smoothModel(db=db, shp=shp)
model <- modelTrainR(modelDefinition)

plotDat <- model$modeledData
p1 <- plotSettings + geom_point(data=plotDat,aes(x=age_bin,y=positive/n))
p1 <- p1 + geom_line(data=plotDat,aes(x=age_bin,y=modeled_fraction_mode)) +
  geom_ribbon(data=plotDat,aes(x=age_bin,ymin=modeled_fraction_0_025quant,ymax=modeled_fraction_0_975quant),alpha=0.3)
p1 + ggtitle('h1n1pdm fraction')


# simulated data rsva age fraction

queryIn <- list(
  SELECT   =list(COLUMN=c('pathogen','age')),
  MUTATE   =list(COLUMN='age', AS='age_bin'),
  GROUP_BY =list(COLUMN=c('age_bin')),
  SUMMARIZE=list(COLUMN='pathogen', IN= 'rsva')
)
db <- expandDB( selectFromDB(  queryIn ) )

modelDefinition <- smoothModel(db=db, shp=shp)
model <- modelTrainR(modelDefinition)

plotDat <- model$modeledData
p1 <- plotSettings + geom_point(data=plotDat,aes(x=age_bin,y=positive/n))
p1 <- p1 + geom_line(data=plotDat,aes(x=age_bin,y=modeled_fraction_mode)) +
  geom_ribbon(data=plotDat,aes(x=age_bin,ymin=modeled_fraction_0_025quant,ymax=modeled_fraction_0_975quant),alpha=0.3)
p1 + ggtitle('rsva fraction')



