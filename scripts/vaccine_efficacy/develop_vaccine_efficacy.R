# explore vaccine efficacy models


library(dbViewR)
library(incidenceMapR)
library(dplyr)
library(ggplot2)

# SRC <- 'production'
SRC <- 'simulated_data'

db <- selectFromDB(queryIn= list(SELECT  =c("*")), source = SRC)

pathogens <- unique(db$observedData$pathogen)
fluPathogens<- pathogens[( (pathogens %in% c('h1n1pdm','h3n2','vic','yam')) | grepl('flu',pathogens,ignore.case=TRUE) )]
referencePathogens <- pathogens[!(pathogens %in% fluPathogens)]

strata <- list(site_type = unique(db$observedData$site_type))

# geoLevels <- list( seattle_geojson = c('residence_puma','residence_neighborhood_district_name','residence_cra_name','residence_census_tract'),
#                    wa_geojson = c('residence_puma')#, # census tract impossible due to memory limits
#                    #king_county_geojson = c('residence_census_tract')
#                  )

PATHOGEN='h1n1pdm'


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
        GROUP_BY =list(COLUMN=c('pathogen','flu_shot','age_range_fine_lower',STRATUM)),
        SUMMARIZE=list(COLUMN='pathogen', IN= PATHOGEN)
      )
      
      db<- selectFromDB(  queryIn, source=SRC, na.rm=TRUE  ) 
      db <- expandDB(db)
      
      # get all non-flu pathogens denominator.
      tmpQuery <- list(
                        SELECT   =list(COLUMN=c('pathogen','flu_shot','age_range_fine_lower',STRATUM)),
                        WHERE    =list(COLUMN='pathogen', IN= c(PATHOGEN,referencePathogens)),
                        WHERE    =list(COLUMN=STRATUM, IN= LEVEL),
                        GROUP_BY =list(COLUMN=c('pathogen','flu_shot','age_range_fine_lower',STRATUM)),
                        SUMMARIZE=list(COLUMN='pathogen', IN= 'all')
                      )
      tmp <- selectFromDB( tmpQuery, source=SRC, na.rm=TRUE  )$observedData %>% 
        group_by_(.dots=c('flu_shot','age_range_fine_lower',STRATUM)) %>% summarize(n=sum(n))
      
      db$observedData <- db$observedData %>% select(-n) %>% left_join(tmp) %>% select(-site_type)
      
      modelDefinition <- fluVaxEfficacyModel(db=db)
      model <- modelTrainR(modelDefinition)
      print(summary(model$inla))
      
      print(ggplot(model$vaxEfficacyData) + geom_line(aes(x=age_range_fine_lower,y=modeled_vaccine_efficacy_mean)) +
        geom_ribbon(aes(x=age_range_fine_lower,ymin=modeled_vaccine_efficacy_lower_95_CI,ymax=modeled_vaccine_efficacy_upper_95_CI),alpha=0.3))
      
    }
  }
}
