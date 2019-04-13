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

geoLevels <- c('PUMA5CE','CRA_NAME','NEIGHBORHOOD_DISTRICT_NAME','GEOID')

  # geoLevels<-c('PUMA5CE')
  # geoLevels<-c('CRA_NAME')
  # geoLevels<-c('NEIGHBORHOOD_DISTRICT_NAME')
  # geoLevels <- c('GEOID')


# get pathogen list
queryIn <- list(
  SELECT   =list(COLUMN=c('pathogen')),
  GROUP_BY =list(COLUMN=c('pathogen')),
  SUMMARIZE=list(COLUMN='pathogen', IN= 'all')
)
pathogens <- unique(selectFromDB(  queryIn )$observedData$pathogen)

for(geo in geoLevels){

  for( k in 1:length(pathogens)){
    # query pathogen, time, levels you want to play with
    queryIn <- list(
      SELECT   =list(COLUMN=c('num_date','pathogen','samplingLocation','fluShot',geo)),
      WHERE    =list(COLUMN=c('pathogen'), IN=pathogens[k]),
      MUTATE   =list(COLUMN=c('num_date'), AS='timeBin'),
      GROUP_BY =list(COLUMN=c('pathogen','timeBin','samplingLocation','fluShot',geo)),
      SUMMARIZE=list(COLUMN='pathogen', IN= pathogens[k])
    )
    db <- expandDB( selectFromDB(  queryIn ) )

    db <- appendCatchmentModel(db,shp=shp)

    # build latent field model
    modelDefinition <- latentFieldModel(db=db, shp=shp)
    model <- modelTrainR(modelDefinition)

    print(summary(model$inla))


    idx <- model$modeledData$samplingLocation=='hospital' & model$modeledData$fluShot==1
    plot(log(model$modeledData$fitted.values.mode[idx]))
    plot(model$latentField$latent.field.mode)

    plot(model$latentField$latent.field.mode,
         log(model$modeledData$fitted.values.mode[idx]))


    saveModel(model, cloudDir = 'data')

    # model<-returnModel(db$queryList,format='model',type='latentField')

    if (geo =='GEOID'){
      for(k in unique(model$modeledData$samplingLocation)){
        for (n in unique(model$modeledData$timeRow)){
          tmp<-list(modeledData = model$modeledData[model$modeledData$samplingLocation==k & model$modeledData$timeRow == n,])
          ggplotSmoothMap(tmp,shp,paste(k,n))
        }
      }
    }
  }
}

###################################
##### latent field models #########
###################################

