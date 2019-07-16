#' addCensusData: function for fetching census data and merging it with input dataset
#'
#' @param db tibble with valid column names for INLA model
#' @param geography geography of your data, default value - 'tract', for other options check here  
#' @param variables character string or vector of character strings of variable IDs.
#' @param year the year for which you are requesting data
#' @param state the state for which you are requesting data, default value - 'WA'
#' @param county the county for which you are requesting data, default value - 'King'
#' @param source source database, one of: 'acs' for five-year American Community Survey or 'decennial' for decennial Census
#' @param credentials_path path to your file with Census API key, you can get your own census api key here: https://api.census.gov/data/key_signup.html  
#' @return db tibble with flu and census data
#'
#' @import tidycensus
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @export
#' @examples
#' 
addCensusData <- function( db = dbViewR::selectFromDB(), 
                           geography = "tract", variables, year,
                           state = "WA", county = "King",
                           source = "decennial",
                           credentials_path = '/home/rstudio/seattle_flu')
  
{
  #get the census api key value from the file
  keyFile <- file(file.path(credentials_path, "census_api_key.txt"), open = 'r') 
  keyValue <- readLines(keyFile, n = 1)
  close(keyFile)
  
  Sys.setenv(CENSUS_API_KEY = keyValue)
  
  if(source == "decennial")
     {
       dataset <- "sf1"
  }
  else if(source =="acs")
  {
    dataset <- "acs5"
  }
  else {
    print('unknown source of census data!')
    return(db)
  }

  varNames <- tidycensus::load_variables(year, dataset, cache = TRUE) 
  varToQuery <- intersect(variables, varNames$name)
  if (length(variables)<length(varToQuery))
    print('some requested variables are not presented in census data')
  
  
  if(source == "decennial")
  {
    censusData <- tidycensus::get_decennial(geography = geography, state = state, county = county, 
                         variables = varToQuery, year = year)
    for (var in varToQuery)
    {
      censusVar <- censusData %>% dplyr::filter(variable == var) %>% dplyr::select(GEOID, !!quo_name(paste(var, year, sep = ".")) := value)
      db$observedData <- dplyr::left_join(db$observedData, censusVar, by = c("residence_census_tract"="GEOID"))
    }
  }
  else if(source =="acs")
  {
    censusData <- tidycensus::get_acs(geography = geography, state = state, county = county, 
                         variables = varToQuery, year = year)
    for (var in varToQuery)
    {
      censusVar <- censusData %>% dplyr::filter(variable == var) %>% dplyr::select(GEOID, !!quo_name(paste(var, year, sep = ".")) := estimate)
      db$observedData <- dplyr::left_join(db$observedData, censusVar, by = c("residence_census_tract"="GEOID"))
    }
  }
  
 return(db)
}