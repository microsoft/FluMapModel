# incidence-mapper

seattleflu/incidence-mapper performs geospatial modeling for epi and incidence data using methods based on [R-INLA](http://www.r-inla.org/) and an API service to deliver modeled data to seattleflu-viz (to come). 

R packages encapsulate key aspects of the workflow.

### offline model training
- **dbViewR** provides functions to retrieve data from the research database and geospatial data repositories and format it for downstream use
- **incidenceMapR** is the core modeling package.  It takes data from dbViewR, parses it to define models, runs the model training code (which can take a while!), formats the output for downstream use, and saves and registers trained model artifacts for future recall. 
- **modelTestR** is a few simple test functions and scripts for playing with the workflow locally. 

### webservice to deliver model results
- **modelServR** interacts with the **api_service** to find requested model data and return it to the visualization service.  (This may migrate over to the Hutch or into the python layer in the future.)


## Cognitive tasks 

Incidence-mapper exists to perform three different classes of routine tasks on enrollment and pathogen incidence data.
- **smooth and interpolate** among observed data to regularize sparse observations. This is useful for providing more accurate estimates of mean and variance at smaller spatial/temporal/age scales than provided by raw counts/fractions, but may often be redundant with raw data binning at larger scales.
- **latent field inference** to infer population-wide infectious disease processes that are driving our observations.  This is the most challenging and most powerful aspect of the modeling, and is critical for proper public health understanding.   For example, what is the space-time series of flu incidence in the whole population, given the non-representative space-and-age distributions of study participants?  In this language, the true incidence is a latent field (set of unobserved variables with some structure) that drives the observed data. 
- **quantify effects of factors** on outcomes using generalized linear regression, after adjusting for confounders (ie covariates that aren't of interest).  This is useful for individual-level understanding of things like vaccine efficacy, and for predicting individual priors for DeDx based on measured covariates. This is not a priority for April as it applies to individuals and isn't the fundamental "flu map".


## To-do on simulated data
Currently, this service pulls data from seattleflu/simulated-data.  There are remaining tasks to finish before swapping over to live data:

- hook api_service to modelServR **OR** jettison R connection and handle modelServR tasks solely in python layer. 
- **incidenceMapR** needs a ton of work
  - build model definitions for modelTrainR with
    - modelEffects
      - write function etc
  - **saveModel** improvements
    - generalize to output fitted.values, coefficients, and latent fields
    - repoint to a more accessible data store. 
    - improve description database for better lookup.

- **modelServR**
  - in coordination with incidenceMapR::saveModel, we need a better model lookup API.  Some sort of artifact storage and recall tool. 
  - it needs to know about both smoothing and latent-field models (to come)
  - what format does Antonio want back? Flat or nested json? (R prefers flat.)
  
# To-do to swap over to live data

## dbViewR

- dbViewR::selectFromDB connect to research DB at Hutch
    - this should probably just be replaced with a SQL query handle to the database that pulls specified views into R, instead of all this dplyr junk. 
- dbViewR::masterSpatialDB connect to shapefiles repo and add option to call different levels of aggregation.
    - am I happy with our geo_jsons?  Consider updated canonical set. 
- dbViewR::expandDB needs to know about permitted columns from real database and rules for table expansion.
    - get database schema from Hutch. 

## modelServR
- Define API with James, Antonio, and Thomas.
  - We intend to ship models as JSON blobs.
  - We will propose some example JSON queries to request model blobs soon. 

# To-do to improve science

- **Provide science documentation**: what are the models (in equations) and what do they do?
 
## across all R packages
- incorporate census demographic data
  - locate appropriate data source
  - add query and formatting to dbViewR
  - incorporate in incidenceModelR where appropriate
  
## incidenceMapR
- adjacency networks
  - define lowest spatial scale based on based shapes, instead of residence_census_tract (census tract) only
  - allow adjacency network models beyond default contiguous nearest-neighbor

