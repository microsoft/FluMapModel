# testModelTrainR
# script to test incidenceMapR package

library(dbViewR)
library(incidenceMapR)
library(modelTestR)
library(dplyr)

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
uniqueCategories <- unique(db$observedData$samplingLocation)
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

formula <- y ~ f(i, model="bym2", graph=neighborGraph, replicate=r, constr=TRUE, hyper = hyper$global) + rf - 1
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






# toward timeseries latent field




###################################
##### latent field models #########
###################################

