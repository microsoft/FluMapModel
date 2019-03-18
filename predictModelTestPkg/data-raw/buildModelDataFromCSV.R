buildModelDataFromCSV <- function(){
  model<-c()
  model$incidence <- read.csv("./inst/extdata/predictedInfluenzaByTract.csv")
  model$observed <- read.csv("./inst/extdata/observedInfluenzaByTract.csv")
  return(model)
}
model<-buildModelDataFromCSV()
devtools::use_data(model, internal = TRUE)
