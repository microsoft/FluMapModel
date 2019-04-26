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
  SELECT   =list(COLUMN=c('site_type','GEOID')),
  WHERE    =list(COLUMN='site_type', IN = c('kiosk')),
  GROUP_BY =list(COLUMN=c('site_type','GEOID')),
  SUMMARIZE=list(COLUMN='site_type', IN= c('kiosk'))
)
db <- expandDB( selectFromDB(  queryIn ) )

modelDefinition <- smoothModel(db=db, shp=shp)
model <- modelTrainR(modelDefinition)

ggplotSmoothMap(model,shp)


# simulated data at_home catchment map
queryIn <- list(
  SELECT   =list(COLUMN=c('site_type','GEOID')),
  WHERE    =list(COLUMN='site_type', IN = c('at_home')),
  GROUP_BY =list(COLUMN=c('site_type','GEOID')),
  SUMMARIZE=list(COLUMN='site_type', IN= c('at_home'))
)
db <- expandDB( selectFromDB(  queryIn ) )

modelDefinition <- smoothModel(db=db, shp=shp)
model <- modelTrainR(modelDefinition)

ggplotSmoothMap(model,shp)




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
p1 <- p1 + geom_line(data=plotDat,aes(x=age_bin,y=fitted_values_mode)) +
  geom_ribbon(data=plotDat,aes(x=age_bin,ymin=fitted_values_0_025quant,ymax=fitted_values_0_975quant),alpha=0.3)
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
p1 <- p1 + geom_line(data=plotDat,aes(x=age_bin,y=fitted_values_mode)) +
  geom_ribbon(data=plotDat,aes(x=age_bin,ymin=fitted_values_0_025quant,ymax=fitted_values_0_975quant),alpha=0.3)
p1 + ggtitle('rsva fraction')


# h3n2 PUMA5CE-time smoother
queryIn <- list(
  SELECT   =list(COLUMN=c('pathogen','encountered_date','PUMA5CE')),
  WHERE    =list(COLUMN=c('pathogen'), IN=c('h3n2')),
  MUTATE   =list(COLUMN=c('encountered_date'), AS=c('epi_week')),
  GROUP_BY =list(COLUMN=c('epi_week','PUMA5CE')),
  SUMMARIZE=list(COLUMN='pathogen', IN= c('h3n2'))
)
db <- expandDB( selectFromDB(  queryIn ) )

modelDefinition <- smoothModel(db=db, shp=shp)
model <- modelTrainR(modelDefinition)

plotDat <- model$modeledData
p1 <- plotSettings + geom_point(data=plotDat,aes(x=time_row,y=positive))
p1 <- p1 + geom_line(data=plotDat,aes(x=time_row,y=fitted_values_mode)) +
  geom_ribbon(data=plotDat,aes(x=time_row,ymin=fitted_values_0_025quant,ymax=fitted_values_0_975quant),alpha=0.3)
p1 + ggtitle('h3n2 counts') + facet_wrap("PUMA5CE")

plotDat <- plotDat %>% group_by(PUMA5CE) %>% mutate(peak = max(fitted_values_mode))

p2 <- plotSettings + geom_point(data=plotDat,aes(x=time_row,y=positive/peak, group=PUMA5CE, color=PUMA5CE))
p2 <- p2 + geom_line(data=plotDat,aes(x=time_row,y=fitted_values_mode/peak, group=PUMA5CE, color=PUMA5CE)) +
  # geom_ribbon(data=plotDat,aes(x=time_row,ymin=fitted_values_0_025quant/peak,ymax=fitted_values_0_975quant/peak, group=PUMA5CE, fill=PUMA5CE),alpha=0.3) +
  ggtitle('h3n2 peak timing')
p2


# h3n2 GEOID-time smoother
queryIn <- list(
  SELECT   =list(COLUMN=c('pathogen','encountered_date','GEOID')),
  WHERE    =list(COLUMN=c('pathogen'), IN=c('h3n2')),
  MUTATE   =list(COLUMN=c('encountered_date'), AS=c('epi_week')),
  GROUP_BY =list(COLUMN=c('epi_week','GEOID')),
  SUMMARIZE=list(COLUMN='pathogen', IN= c('h3n2'))
)
db <- expandDB( selectFromDB(  queryIn ) )

db$observedData <- db$observedData[db$observedData$epi_week %in% sort(unique(db$observedData$epi_week))[1:10],]

modelDefinition <- smoothModel(db=db, shp=shp)
model <- modelTrainR(modelDefinition)

plotDat <- model$modeledData
p1 <- plotSettings + geom_point(data=plotDat,aes(x=time_row,y=positive, group=GEOID, color=GEOID))
p1 <- p1 + geom_line(data=plotDat,aes(x=time_row,y=fitted_values_mode, group=GEOID, color=GEOID))
p1 + ggtitle('h3n2 counts') + guides(color=FALSE)

plotDat <- plotDat %>% group_by(GEOID) %>% mutate(peak = max(fitted_values_mode))

p2 <- plotSettings + geom_point(data=plotDat,aes(x=time_row,y=positive/peak, group=GEOID, color=GEOID))
p2 <- p2 + geom_line(data=plotDat,aes(x=time_row,y=fitted_values_mode/peak, group=GEOID, color=GEOID))
p2 + ggtitle('h3n2 peak timing')+ guides(color=FALSE)


# coerce peak timing into ggplotSmoothMap expected format
plotDat <- plotDat %>% group_by(GEOID) %>% summarize(fitted_values_mode = time_row[fitted_values_mode==max(fitted_values_mode)],
                                                     positive = mean(time_row[positive==max(positive)]))
tmp<- list(modeledData = plotDat)
ggplotSmoothMap(tmp,shp)


# overfitting right now....




