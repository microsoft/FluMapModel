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
  SELECT   =list(COLUMN=c('samplingLocation','GEOID')),
  WHERE    =list(COLUMN='samplingLocation', IN = c('kiosk')),
  GROUP_BY =list(COLUMN=c('samplingLocation','GEOID')),
  SUMMARIZE=list(COLUMN='samplingLocation', IN= c('kiosk'))
)
db <- expandDB( selectFromDB(  queryIn ) )

modelDefinition <- smoothModel(db=db, shp=shp)
model <- modelTrainR(modelDefinition)

ggplotSmoothMap(model,shp)


# simulated data atHome catchment map
queryIn <- list(
  SELECT   =list(COLUMN=c('samplingLocation','GEOID')),
  WHERE    =list(COLUMN='samplingLocation', IN = c('atHome')),
  GROUP_BY =list(COLUMN=c('samplingLocation','GEOID')),
  SUMMARIZE=list(COLUMN='samplingLocation', IN= c('atHome'))
)
db <- expandDB( selectFromDB(  queryIn ) )

modelDefinition <- smoothModel(db=db, shp=shp)
model <- modelTrainR(modelDefinition)

ggplotSmoothMap(model,shp)




# simulated data h1n1pdm age fraction

queryIn <- list(
  SELECT   =list(COLUMN=c('pathogen','age')),
  GROUP_BY =list(COLUMN=c('age')),
  SUMMARIZE=list(COLUMN='pathogen', IN= 'h1n1pdm')
)
db <- expandDB( selectFromDB(  queryIn ) )

modelDefinition <- smoothModel(db=db, shp=shp)
model <- modelTrainR(modelDefinition)

plotDat <- model$modeledData
p1 <- plotSettings + geom_point(data=plotDat,aes(x=ageBin,y=positive/n))
p1 <- p1 + geom_line(data=plotDat,aes(x=ageBin,y=fitted.values.mode)) +
  geom_ribbon(data=plotDat,aes(x=ageBin,ymin=fitted.values.0.025quant,ymax=fitted.values.0.975quant),alpha=0.3)
p1 + ggtitle('h1n1pdm fraction')


# simulated data rsva age fraction

queryIn <- list(
  SELECT   =list(COLUMN=c('pathogen','age')),
  GROUP_BY =list(COLUMN=c('age')),
  SUMMARIZE=list(COLUMN='pathogen', IN= 'rsva')
)
db <- expandDB( selectFromDB(  queryIn ) )

modelDefinition <- smoothModel(db=db, shp=shp)
model <- modelTrainR(modelDefinition)

plotDat <- model$modeledData
p1 <- plotSettings + geom_point(data=plotDat,aes(x=ageBin,y=positive/n))
p1 <- p1 + geom_line(data=plotDat,aes(x=ageBin,y=fitted.values.mode)) +
  geom_ribbon(data=plotDat,aes(x=ageBin,ymin=fitted.values.0.025quant,ymax=fitted.values.0.975quant),alpha=0.3)
p1 + ggtitle('rsva fraction')


# h3n2 PUMA5CE-time smoother
queryIn <- list(
  SELECT   =list(COLUMN=c('pathogen','timeInfected','PUMA5CE')),
  WHERE    =list(COLUMN=c('pathogen'), IN=c('h3n2')),
  MUTATE   =list(COLUMN=c('timeInfected'), AS=c('timeBin')),
  GROUP_BY =list(COLUMN=c('timeBin','PUMA5CE')),
  SUMMARIZE=list(COLUMN='pathogen', IN= c('h3n2'))
)
db <- expandDB( selectFromDB(  queryIn ) )

modelDefinition <- smoothModel(db=db, shp=shp)
model <- modelTrainR(modelDefinition)

plotDat <- model$modeledData
p1 <- plotSettings + geom_point(data=plotDat,aes(x=timeBin,y=positive))
p1 <- p1 + geom_line(data=plotDat,aes(x=timeBin,y=fitted.values.mode)) +
  geom_ribbon(data=plotDat,aes(x=timeBin,ymin=fitted.values.0.025quant,ymax=fitted.values.0.975quant),alpha=0.3)
p1 + ggtitle('h3n2 counts') + facet_wrap(~PUMA5CE)

plotDat <- plotDat %>% group_by(PUMA5CE) %>% mutate(peak = max(fitted.values.mode))

p2 <- plotSettings + geom_point(data=plotDat,aes(x=timeBin,y=positive/peak, group=PUMA5CE, color=PUMA5CE))
p2 <- p2 + geom_line(data=plotDat,aes(x=timeBin,y=fitted.values.mode/peak, group=PUMA5CE, color=PUMA5CE)) +
  # geom_ribbon(data=plotDat,aes(x=timeBin,ymin=fitted.values.0.025quant/peak,ymax=fitted.values.0.975quant/peak, group=PUMA5CE, fill=PUMA5CE),alpha=0.3) +
  ggtitle('h3n2 peak timing')
p2


# h3n2 GEOID-time smoother
queryIn <- list(
  SELECT   =list(COLUMN=c('pathogen','timeInfected','GEOID')),
  WHERE    =list(COLUMN=c('pathogen'), IN=c('h3n2')),
  MUTATE   =list(COLUMN=c('timeInfected'), AS=c('timeBin')),
  GROUP_BY =list(COLUMN=c('timeBin','GEOID')),
  SUMMARIZE=list(COLUMN='pathogen', IN= c('h3n2'))
)
db <- expandDB( selectFromDB(  queryIn ) )

modelDefinition <- smoothModel(db=db, shp=shp)
model <- modelTrainR(modelDefinition)

plotDat <- model$modeledData
p1 <- plotSettings + geom_point(data=plotDat,aes(x=timeBin,y=positive, group=GEOID, color=GEOID))
p1 <- p1 + geom_line(data=plotDat,aes(x=timeBin,y=fitted.values.mode, group=GEOID, color=GEOID))
p1 + ggtitle('h3n2 counts') + guides(color=FALSE)

plotDat <- plotDat %>% group_by(GEOID) %>% mutate(peak = max(fitted.values.mode))

p2 <- plotSettings + geom_point(data=plotDat,aes(x=timeBin,y=positive/peak, group=GEOID, color=GEOID))
p2 <- p2 + geom_line(data=plotDat,aes(x=timeBin,y=fitted.values.mode/peak, group=GEOID, color=GEOID))
p2 + ggtitle('h3n2 peak timing')+ guides(color=FALSE)


# coerce peak timing into ggplotSmoothMap expected format
plotDat <- plotDat %>% group_by(GEOID) %>% summarize(fitted.values.mode = timeBin[fitted.values.mode==max(fitted.values.mode)],
                                                     positive = mean(timeBin[positive==max(positive)]))
tmp<- list(modeledData = plotDat)
ggplotSmoothMap(tmp,shp)


# overfitting right now....




###################################
##### latent field models #########
###################################



###################################
##### effects of factors models ###
###################################







####################################
###### catchment models  ###########
####################################
### OLD! ###


####################################
###### disease models  ###########
####################################

# sampling offset for later
catchment <-rbind(model$modeledData,model2$modeledData)
catchment <- catchment[,c(1,2,10)]
names(catchment)[3]<-'offset'
catchment$offset<- log(catchment$offset)

# h1n1 spatial only (no time)
queryIn <- list(
  SELECT   =list(COLUMN=c('samplingLocation','pathogen','PUMA5CE','GEOID')),
  WHERE   =list(COLUMN='samplingLocation', IN = c('kiosk','hospital')),
  GROUP_BY =list(COLUMN=c('PUMA5CE','GEOID','samplingLocation')),
  SUMMARIZE=list(COLUMN='pathogen', IN= c('h1n1pdm'))
)
db <- expandDB( selectFromDB(  queryIn ) )

db$observedData <- right_join(db$observedData,catchment,by=c("samplingLocation",'GEOID'))

model <- modelTrainR(family='poisson',db=db, shp=shp)
saveModel(model)

plotDat <- right_join(model$modeledData,shp, by=c('GEOID','PUMA5CE'))
plotDat$positive[plotDat$positive==0]<-NaN
ggplotSmoothMap(plotDat,shp)


plot(model$inla$summary.random$PUMA5CERow$mean)
plot(model$inla$summary.random$GEOIDRow$mean)
plot(model$modeledData$fitted.values.mode,model$modeledData$offset)
plot(model$modeledData$offset,db$observedData$positive)
### hmmm.  the offset from catchment determines everything...
### I'm double-counting somehow.  Something about scale of offset....
### LATER: nevermind--this is dumb. If I'm not doing time dependence, the double-counting is obvious.
###    This line of reasoning requires a new model flow in incidenceMapR.


# h1n1 PUMA5CE with time
queryIn <- list(
  SELECT   =list(COLUMN=c('pathogen','timeInfected','PUMA5CE')),
  MUTATE   =list(COLUMN=c('timeInfected'), AS=c('timeBin')),
  GROUP_BY =list(COLUMN=c('timeBin','PUMA5CE')),
  SUMMARIZE=list(COLUMN='pathogen', IN= c('h1n1pdm'))
)
db <- expandDB( selectFromDB(  queryIn ) )

db$observedData$n<-db$observedData$positive
model <- modelTrainR(family='poisson',db=db, shp=shp)

plotDat <- right_join(model$modeledData,shp, by=c('PUMA5CE'))

p1 <- plotSettings + geom_point(data=plotDat,aes(x=timeBin,y=positive))
p1 <- p1 + geom_line(data=plotDat,aes(x=timeBin,y=fitted.values.mode)) +
  geom_ribbon(data=plotDat,aes(x=timeBin,ymin=fitted.values.0.025quant,ymax=fitted.values.0.975quant),alpha=0.3)
p1 + facet_wrap(~PUMA5CE) + ggtitle('PUMA5CE h1n1 fraction')

p2 <- plotSettings + geom_line(data=plotDat,aes(x=timeBin,y=fitted.values.mode,color=PUMA5CE, group=PUMA5CE))
p2



# rsva PUMA5CE with age
queryIn <- list(
  SELECT   =list(COLUMN=c('pathogen','age','PUMA5CE')),
  GROUP_BY =list(COLUMN=c('age','PUMA5CE')),
  SUMMARIZE=list(COLUMN='pathogen', IN= c('rsva'))
)
db <- expandDB( selectFromDB(  queryIn ) )

model <- modelTrainR(family='binomial',db=db, shp=shp)

plotDat <- right_join(model$modeledData,shp, by=c('PUMA5CE'))

p1 <- plotSettings + geom_point(data=plotDat,aes(x=ageBin,y=positive/n))
p1 <- p1 + geom_line(data=plotDat,aes(x=ageBin,y=fitted.values.mode)) +
  geom_ribbon(data=plotDat,aes(x=ageBin,ymin=fitted.values.0.025quant,ymax=fitted.values.0.975quant),alpha=0.3)
p1 + facet_wrap(~PUMA5CE) + ggtitle('PUMA5CE rsva fraction')

p2 <- plotSettings + geom_line(data=plotDat,aes(x=ageBin,y=fitted.values.mode,color=PUMA5CE, group=PUMA5CE))
p2


# h1n1 PUMA5CE and GEOID with time
queryIn <- list(
  SELECT   =list(COLUMN=c('pathogen','timeInfected','PUMA5CE','GEOID')),
  MUTATE   =list(COLUMN=c('timeInfected'), AS=c('timeBin')),
  GROUP_BY =list(COLUMN=c('timeBin','PUMA5CE','GEOID')),
  SUMMARIZE=list(COLUMN='pathogen', IN= c('h1n1pdm'))
)
db <- expandDB( selectFromDB(  queryIn ) )
db$observedData$n<-db$observedData$positive

model <- modelTrainR(family='poisson',db=db, shp=shp)

plotDat <- right_join(model$modeledData,shp, by=c('PUMA5CE','GEOID'))

p1 <- plotSettings + geom_point(data=plotDat,aes(x=timeBin,y=positive,group=GEOID, color=GEOID))
p1 <- p1 + geom_line(data=plotDat,aes(x=timeBin,y=fitted.values.mode,group=GEOID,color=GEOID)) +
  geom_ribbon(data=plotDat,aes(x=timeBin,ymin=fitted.values.0.025quant,ymax=fitted.values.0.975quant,group=GEOID, fill=GEOID),alpha=0.02)
p1 + facet_wrap(~PUMA5CE) + ggtitle('PUMA5CE h1n1 fraction') + guides(color=FALSE, fill=FALSE) + ylim(c(0,8))

p2 <- plotSettings + geom_line(data=plotDat,aes(x=timeBin,y=mode,color=PUMA5CE, group=GEOID)) + guides(color=FALSE)
p2
