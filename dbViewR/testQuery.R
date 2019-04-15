# testQuery
# script to test queries

library(dbViewR)

########################################################
####         test selectFromDB       ###################
########################################################

## return all
  queryJSON <- jsonlite::toJSON(list(SELECT  =c("*")))
  db <- selectFromDB( queryJSON )

## return subset
  queryJSON <- jsonlite::toJSON(
    list(
      SELECT  =list(COLUMN=c('id','pathogen','encountered_date','sampling_location','sex','flu_shot','age','has_fever','has_cough','has_myalgia')),
      WHERE   =list(COLUMN='pathogen', IN = c('h1n1pdm', 'h3n2')),
      WHERE   =list(COLUMN='encountered_date', BETWEEN = as.Date(c('2019-01-10','2019-02-28'))),
      WHERE   =list(COLUMN='sampling_location', IN='hospital')
    )
  )
  db <- selectFromDB( queryJSON )

########################################################
####         test summarizeDB       ###################
########################################################


## return h1n1pdm summary by time and location
  queryIn <- list(
      SELECT   =list(COLUMN=c('pathogen','encountered_date','PUMA5CE','GEOID')),
      MUTATE   =list(COLUMN=c('encountered_date'), AS=c('epi_week')),
      GROUP_BY =list(COLUMN=c('epi_week','PUMA5CE','GEOID')),
      SUMMARIZE=list(COLUMN='pathogen', IN= c('h1n1pdm'))
    )
  db <- selectFromDB( queryIn )


## return has_fever summary by age and location
  queryJSON <- jsonlite::toJSON(
    list(
      SELECT   =list(COLUMN=c('has_fever','age','PUMA5CE','GEOID')),
      GROUP_BY =list(COLUMN=c('age','PUMA5CE','GEOID')),
      SUMMARIZE=list(COLUMN='has_fever', IN= c(TRUE))
    )
  )
  db <- selectFromDB( queryJSON )


  
########################################################
####         test expandDb       ###################
########################################################


## return h1n1pdm summary by time and location
queryIn <- list(
  SELECT   =list(COLUMN=c('pathogen','encountered_date','PUMA5CE','GEOID')),
  MUTATE   =list(COLUMN=c('encountered_date'), AS=c('epi_week')),
  GROUP_BY =list(COLUMN=c('epi_week','PUMA5CE','GEOID')),
  SUMMARIZE=list(COLUMN='pathogen', IN= c('h1n1pdm'))
)
db <- expandDB(selectFromDB( queryIn ))


## return has_fever summary by age and location
queryJSON <- jsonlite::toJSON(
  list(
    SELECT   =list(COLUMN=c('has_fever','age','PUMA5CE','GEOID')),
    MUTATE   =list(COLUMN='age', AS='age_bin'),
    GROUP_BY =list(COLUMN=c('age_bin','PUMA5CE','GEOID')),
    SUMMARIZE=list(COLUMN='has_fever', IN= c(TRUE))
  )
)
db <- expandDB(selectFromDB( queryJSON ))


