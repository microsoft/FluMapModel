# testModelTrainR
# script to test incidenceMapR package

library(dbViewR)
library(incidenceMapR)
library(modelTestR)
library(modelServR)
library(dplyr)
library(magrittr)

library(INLA)

shp <- masterSpatialDB()  # census-tract shapefiles
neighborGraph <- constructAdjacencyNetwork(shp)

# ggplot build eventually will be replaced by function ggplotSmoothSequential
library(ggplot2)
plotSettings <- ggplot() + theme_bw() +  theme(panel.border = element_blank()) + xlab('')


##################################
### raw INLA protypes ############
##################################

# first, joint catchment maps

queryIn <- list(
  SELECT   =list(COLUMN=c('samplingLocation','GEOID')),
  GROUP_BY =list(COLUMN=c('samplingLocation','GEOID')),
  SUMMARIZE=list(COLUMN='samplingLocation', IN= 'all')
)
db <- expandDB( selectFromDB(  queryIn ) )

# multiple likelihood
uniqueCategories <- sort(unique(db$observedData$samplingLocation))
numLikelihoods <- length(uniqueCategories)

family <- rep('poisson',numLikelihoods)  # must be list of valid families

inputData<-db$observedData
inputData$GEOIDRow <- match(inputData$GEOID,unique(inputData$GEOID))


y <- matrix(NA,nrow(inputData),numLikelihoods)
r <- matrix(NA,nrow(inputData),1)
for( k in uniqueCategories){
  idx <- inputData$samplingLocation %in% k
  count <- which(uniqueCategories %in% k)
  y[idx, count] <- inputData$positive[idx]
  r[idx]<-count
}

rf <- inputData$samplingLocation

i <- inputData$GEOIDRow


hyper=list()
hyper$global <- list(prec = list( prior = "pc.prec", param = 1/10, alpha = 0.01))

# replicate field with different offset

formula <- y ~ f(i, model="besag", graph=neighborGraph, replicate=r, constr=TRUE, hyper = hyper$global) + rf - 1
data <- data.frame(y, i, r, rf)

summary(result <- inla(formula = formula, family = family, data = data,
             control.predictor=list(compute=TRUE,link=1),
             control.compute=list(config=TRUE,dic=TRUE),verbose = TRUE,
             control.inla=list(int.strategy="eb", strategy = "gaussian")))

# append data
  modeledData <- db$observedData
  nCol <- ncol(modeledData)
  modeledData[,nCol+1:ncol(result$summary.fitted.values)]<-result$summary.fitted.values
  names(modeledData)[nCol+1:ncol(result$summary.fitted.values)]<-paste('fitted.values',names(result$summary.fitted.values),sep='.')
  rownames(modeledData)<-c()

# plot
  for(k in uniqueCategories){
    model<-list(modeledData = modeledData[modeledData$samplingLocation==k,])
    ggplotSmoothMap(model,shp,k)
  }
  # this works great!

# random effects vis
  modeledData <- inputData %>% arrange(samplingLocation,GEOIDRow)
  nCol <- ncol(modeledData)
  tmp<-result$summary.random$i
  modeledData[,nCol+1:ncol(result$summary.random$i)]<-tmp
  names(modeledData)[nCol+1:ncol(result$summary.random$i)]<-paste('fitted.values',names(result$summary.random$i),sep='.')
  rownames(modeledData)<-c()
  modeledData$fitted.values.mode <- exp(modeledData$fitted.values.mode)

  # plot
  for(k in uniqueCategories){
    model<-list(modeledData = modeledData[modeledData$samplingLocation==k,])
    ggplotSmoothMap(model,shp,k)
  }




###################################
# timeseries latent field
###################################

geoLevels <- c('GEOID','PUMA5CE','CRA_NAME','NEIGHBORHOOD_DISTRICT_NAME')

  geo='PUMA5CE'

for(geo in geoLevels){

  # find catchment maps for each samplingLocation and geoLevel
  queryIn <- list(
    SELECT   =list(COLUMN=c('samplingLocation',geo)),
    GROUP_BY =list(COLUMN=c('samplingLocation',geo)),
    SUMMARIZE=list(COLUMN='samplingLocation', IN= 'all')
  )
  db <- expandDB( selectFromDB(  queryIn ) )

  catchmentModelDefinition <- smoothModel(db=db, shp=shp)
  catchmentModel <- modelTrainR(catchmentModelDefinition)
  summary(catchmentModel$inla)


  # query pathogen and time
  queryIn <- list(
    SELECT   =list(COLUMN=c('num_date','pathogen','samplingLocation',geo)),
    MUTATE   =list(COLUMN=c('num_date'), AS='timeBin'),
    GROUP_BY =list(COLUMN=c('pathogen','timeBin','samplingLocation',geo)),
    SUMMARIZE=list(COLUMN='pathogen', IN= 'all')
  )
  db <- expandDB( selectFromDB(  queryIn ) )

  # append catchment as intercept covariate
  db$observedData <- db$observedData %>% right_join(catchmentModel$modeledData %>% select(samplingLocation, geo, fitted.values.0.5quant))
  names(db$observedData)[names(db$observedData) %in% 'fitted.values.0.5quant'] <- 'catchment'
  db$observedData$catchment <- (db$observedData$catchment - mean(db$observedData$catchment))/sd(db$observedData$catchment)


  # db$observedData <- db$observedData[db$observedData$timeRow<20,]

  # build latent field model
  modelDefinition <- latentFieldModel(db=db, shp=shp)
  model <- modelTrainR(modelDefinition)

  summary(model$inla)


  saveModel(model)

  model<-returnModel(db$queryList,format='model',type='latentField')


  head(model$modeledData)

  # inputData$level <- model$modeledData %>% select(pathogen,samplingLocation) %>% interaction()
  plotSettings + geom_line(data=model$modeledData[ model$modeledData$samplingLocation=='hospital' ,],aes(x=timeBin,y=fitted.values.mode, group = PUMA5CE, color=PUMA5CE)) + facet_wrap("pathogen")



  # extract latent field
  model$inla$size.random

  lines(model$inla$summary.random$timeRow_rw2$ID %% 43,model$inla$summary.random$timeRow_IID$mode+model$inla$summary.random$timeRow_rw2$mode)

  dim(model$inla$summary.random$PUMA5CERow)
  dim(model$inla$summary.random$timeRow_rw2)
  dim(model$inla$summary.fitted.values)






  # smooth vis
  for(k in unique(model$modeledData$samplingLocation)){
    for (n in unique(model$modeledData$timeRow)){
      tmp<-list(modeledData = model$modeledData[model$modeledData$samplingLocation==k & model$modeledData$timeRow == n,])
      ggplotSmoothMap(tmp,shp,paste(k,n))
    }
  }

  # random effects vis
  modeledData <- inputData %>% arrange(samplingLocation,GEOIDRow)
  nCol <- ncol(modeledData)
  tmp<-result$summary.random$i
  modeledData[,nCol+1:ncol(result$summary.random$i)]<-tmp
  names(modeledData)[nCol+1:ncol(result$summary.random$i)]<-paste('fitted.values',names(result$summary.random$i),sep='.')
  rownames(modeledData)<-c()
  modeledData$fitted.values.mode <- exp(modeledData$fitted.values.mode)



  saveModel(model, cloudDir = 'C:/Users/mfamulare/Dropbox (IDM)/SeattleFlu-incidenceMapR/models')

  if (geo =='GEOID'){
    for(k in unique(model$modeledData$samplingLocation)){
      for (n in unique(model$modeledData$timeRow)){
        tmp<-list(modeledData = model$modeledData[model$modeledData$samplingLocation==k & model$modeledData$timeRow == n,])
        ggplotSmoothMap(tmp,shp,paste(k,n))
      }
    }
  }
}


queryIn <- list(
  SELECT   =list(COLUMN=c('num_date','pathogen','samplingLocation','GEOID')),
  MUTATE   =list(COLUMN=c('num_date'), AS='timeBin'),
  GROUP_BY =list(COLUMN=c('timeBin','samplingLocation','GEOID')),
  SUMMARIZE=list(COLUMN='pathogen', IN= 'h1n1pdm')
)
db <- expandDB( selectFromDB(  queryIn ) )
head(db$observedData,20)

# incidence <- db$observedData %>% group_by(GEOID,timeBin,timeRow) %>% summarize(n=sum(n),positive =sum(positive, na.rm=TRUE))
# incidence$positive[incidence$n==0]<-NA
# incidence

inputData<-cbind(db$observedData,db2$observedData)


# multiple likelihood
uniqueCategories <- sort(unique(db$observedData$samplingLocation))
numLikelihoods <- length(uniqueCategories) + 1

family <- rep('poisson',numLikelihoods)  # must be list of valid families

inputData<-db$observedData
inputData$GEOIDRow <- match(inputData$GEOID,unique(inputData$GEOID))


















# append data
modeledData <- db$observedData
nCol <- ncol(modeledData)
modeledData[,nCol+1:ncol(result$summary.fitted.values)]<-result$summary.fitted.values
names(modeledData)[nCol+1:ncol(result$summary.fitted.values)]<-paste('fitted.values',names(result$summary.fitted.values),sep='.')
rownames(modeledData)<-c()






# plot
for(k in uniqueCategories){
  model<-list(modeledData = modeledData[modeledData$samplingLocation==k,])
  ggplotSmoothMap(model,shp,k)
}

###################################
##### latent field models #########
###################################

