#' saveModel: function to save models and register them in modelDB.csv
#'
#' @param model INLA object
#' @param db dbViewR object
#'
#' @import digest
#' @importFrom jsonlite toJSON
#' 
#' @export
#'
saveModel <- function(model, cloudDir = '/home/rstudio/seattle_flu/data'){

  ts <- Sys.time()
  attr(ts, "tzone")<-'UTC'
  ts<-paste0(as.character(ts),'Z')
  
  filename <- digest::digest(paste(digest::digest(model$modeledData),ts,sep=''))
  
  # all models output inla
  newRow <- data.frame(filename=filename,
                       queryJSON=as.character(jsonlite::toJSON(model$modelDefinition$queryList)),
                       type = 'inla',
                       created = ts)
  
  outfile <- xzfile(paste(cloudDir, '/', filename, '.RDS', sep = ''), 'wb', compress=9, encoding = 'utf8')
  saveRDS(model,file = outfile)
  close(outfile)
  
  # all models output smooth
  newRow <- rbind(newRow, 
                    data.frame(filename=filename,
                    queryJSON=as.character(jsonlite::toJSON(model$modelDefinition$queryList)),
                    type = 'smooth',
                    created = ts)
                  )
  write.csv(model$modeledData,paste(cloudDir,'/',filename,'.smooth.csv',sep=''),row.names = FALSE,quote = FALSE)
  
  if(modelDefinition$type == 'latent_field'){
    newRow <- rbind(newRow, 
                     data.frame(filename=filename,
                     queryJSON=as.character(jsonlite::toJSON(model$modelDefinition$queryList)),
                     type = model$modelDefinition$type,
                     created = ts)
                     )
    
    write.csv(model$latentField,paste(cloudDir,'/',filename,'.latent_field.csv',sep=''),row.names = FALSE,quote = FALSE)
  }
  

  # register in modelDB
  modelDBfilename<-paste(cloudDir,'/','modelDB.tsv',sep='')
  if(!file.exists(modelDBfilename)){
    write.table(newRow,file=modelDBfilename,sep='\t',row.names = FALSE, col.names = TRUE,quote = FALSE)
  } else {
    write.table(newRow,file=modelDBfilename,sep='\t',row.names = FALSE, col.names = FALSE,quote = FALSE, append=TRUE)
  }
  
}

