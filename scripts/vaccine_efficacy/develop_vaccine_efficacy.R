# explore vaccine efficacy models


library(dbViewR)
library(incidenceMapR)
library(dplyr)
library(ggplot2)

SRC <- 'production'
# SRC <- 'simulated_data'

db <- selectFromDB(queryIn= list(SELECT  =c("*")), source = SRC)

pathogens <- unique(db$observedData$pathogen)
fluPathogens<- pathogens[( (pathogens %in% c('h1n1pdm','h3n2','vic','yam')) | grepl('flu',pathogens,ignore.case=TRUE) )]
referencePathogens <- pathogens[!(pathogens %in% fluPathogens)]

strata <- list(site_type='all')
# strata <- list(site_type = unique(db$observedData$site_type))


###########################
### efficacy by age #######
###########################

for (PATHOGEN in fluPathogens){
  for(STRATUM in names(strata)){
    for(LEVEL in strata[[STRATUM]]){
      
      queryIn <- list(
        SELECT   =list(COLUMN=c('pathogen','flu_shot','age_range_fine_lower',STRATUM)),
        WHERE    =list(COLUMN='pathogen', IN= PATHOGEN),
        WHERE    =list(COLUMN=STRATUM, IN = LEVEL),
        GROUP_BY =list(COLUMN=c('pathogen','flu_shot','age_range_fine_lower')),
        SUMMARIZE=list(COLUMN='pathogen', IN= PATHOGEN)
      )
      
      db<- selectFromDB(  queryIn, source=SRC, na.rm=TRUE  ) 
      db <- expandDB(db)
      
      # get all non-flu pathogens denominator.
      tmpQuery <- list(
                        SELECT   =list(COLUMN=c('pathogen','flu_shot','age_range_fine_lower',STRATUM)),
                        WHERE    =list(COLUMN='pathogen', IN= c(PATHOGEN,referencePathogens)),
                        WHERE    =list(COLUMN=STRATUM, IN= LEVEL),
                        GROUP_BY =list(COLUMN=c('pathogen','flu_shot','age_range_fine_lower')),
                        SUMMARIZE=list(COLUMN='pathogen', IN= 'all')
                      )
      tmp <- selectFromDB( tmpQuery, source=SRC, na.rm=TRUE  )$observedData %>% 
        group_by_(.dots=c('flu_shot','age_range_fine_lower')) %>% summarize(n=sum(n))
      
      db$observedData <- db$observedData %>% select(-n) %>% left_join(tmp)
      
      # training occassionaly segfaults on but it does not appear to be deterministic...
      tries <- 0
      success<-0
      while (success==0 & tries<=2){
        tries <- tries+1
        tryCatch(
          {
                  
            modelDefinition <- fluVaxEfficacyModel(db=db)
            model <- modelTrainR(modelDefinition)
            print(summary(model$inla))
            
            
            dir.create('/home/rstudio/seattle_flu/may_22_plots/', showWarnings = FALSE)
            fname <- paste('/home/rstudio/seattle_flu/may_22_plots/',paste('inla_vaccine_efficacy',PATHOGEN,STRATUM,LEVEL,'age_range_fine_lower',sep='-'),'.png',sep='')
            png(filename = fname,width = 6, height = 5, units = "in", res = 300)
            
            print(
              ggplot(model$vaxEfficacyData) + geom_line(aes(x=age_range_fine_lower,y=modeled_vaccine_efficacy_mean)) +
                geom_ribbon(aes(x=age_range_fine_lower,ymin=modeled_vaccine_efficacy_lower_95_CI,ymax=modeled_vaccine_efficacy_upper_95_CI),alpha=0.3)
              )
            dev.off()
     
            success<-1
            
          }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
        )
      }
    }
  }
}



###########################
### efficacy by time #######
###########################

for (PATHOGEN in fluPathogens){
  for(STRATUM in names(strata)){
    for(LEVEL in strata[[STRATUM]]){
      
      queryIn <- list(
        SELECT   =list(COLUMN=c('pathogen','flu_shot','encountered_week',STRATUM)),
        WHERE    =list(COLUMN='pathogen', IN= PATHOGEN),
        WHERE    =list(COLUMN=STRATUM, IN = LEVEL),
        GROUP_BY =list(COLUMN=c('pathogen','flu_shot','encountered_week')),
        SUMMARIZE=list(COLUMN='pathogen', IN= PATHOGEN)
      )
      
      db<- selectFromDB(  queryIn, source=SRC, na.rm=TRUE  ) 
      db <- expandDB(db)
      
      # get all non-flu pathogens denominator.
      tmpQuery <- list(
        SELECT   =list(COLUMN=c('pathogen','flu_shot','encountered_week',STRATUM)),
        WHERE    =list(COLUMN='pathogen', IN= c(PATHOGEN,referencePathogens)),
        WHERE    =list(COLUMN=STRATUM, IN= LEVEL),
        GROUP_BY =list(COLUMN=c('pathogen','flu_shot','encountered_week')),
        SUMMARIZE=list(COLUMN='pathogen', IN= 'all')
      )
      tmp <- selectFromDB( tmpQuery, source=SRC, na.rm=TRUE  )$observedData %>% 
        group_by_(.dots=c('flu_shot','encountered_week')) %>% summarize(n=sum(n))
      
      db$observedData <- db$observedData %>% select(-n) %>% left_join(tmp) 
      
      # training occassionaly segfaults on but it does not appear to be deterministic...
      tries <- 0
      success<-0
      while (success==0 & tries<=2){
        tries <- tries+1
        tryCatch(
          {
            modelDefinition <- fluVaxEfficacyModel(db=db)
            model <- modelTrainR(modelDefinition)
            print(summary(model$inla))
            
            dir.create('/home/rstudio/seattle_flu/may_22_plots/', showWarnings = FALSE)
            fname <- paste('/home/rstudio/seattle_flu/may_22_plots/',paste('inla_vaccine_efficacy',PATHOGEN,STRATUM,LEVEL,'encountered_week',sep='-'),'.png',sep='')
            png(filename = fname,width = 5, height = 3.1, units = "in", res = 300)
            
            print(ggplot(model$vaxEfficacyData) + geom_line(aes(x=encountered_week,y=modeled_vaccine_efficacy_mean, group=pathogen)) +
                    geom_ribbon(aes(x=encountered_week,ymin=modeled_vaccine_efficacy_lower_95_CI,ymax=modeled_vaccine_efficacy_upper_95_CI, group=pathogen),alpha=0.3) +
                    theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
                  )
            dev.off()
            
            success<-1
            
          }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
        )
      }
    }
  }
}
