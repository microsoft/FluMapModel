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


# Models that demo_deployment

These model csvs obey the format that will go up on the deployed model server (subject to revision if requested by viz team).  The older models in parallel directories are obsolete, but I've kept them to avoid breaking prototypes.

## Model definition
Models are defined by:
- the pathogen of interest, either a specific one from the database (assuming it gets populated..., like `FLU_A_H1` or `h3n2`) or `all` (for all samples regardless of pathogen), or `unknown` (the default I have until the taqman data is made available).
- the strata (/facets/covariates), like `[encountered_week, residence_puma]` or `[flu_shot, residence_census_tract]`
- either the type of model (`observed` or `latent`), or, if that makes little sense on your end, the outcome you want models of, like (`count` and `fraction` (observed), or `intensity` (latent)).

## Timeseries models
All examples included in this commit are timeseries models by `encountered_week`.  Both model types need not include time in general, but all the ones for May 22 will.  To collapse over time to produce a static map, it is approximately valid to average outcomes over time. Long-term, there are more interesting, and more correct, things to do, but that's not bad and retains interesting meaning for people exploring the data.

### `Observed` models
`Observed` models are interpolations and extrapolations from the observed data.  For the purposes of visualization, these models complement raw data summaries in two ways:
1.  They use the statistical properties of the entire sample to regularize estimates for small areas and times. This goes by many names in the statistical modeling literature, but two useful ones are "partial pooling" for  "small area estimation".  The benefit of partially-pooled estimates is they reduce the influence of small sample statistics on visual outlier detection. (For example, we shouldn't really believe that places with no measurements have no incidence.)
2.  For human comprehension, they "fill in the map" to emphasize larger-scale patterns that are more likely to be real (as in, there's a lot of aggregate evidence) and down-play local variation for small samples.  This aspect will be useful for understanding where and when we gathered data, so we can rationally plan to expand next year.

The `observed` models in this repo are faceted by factors about our participants, like `site_type` (where the sample was taken), `sex` (male or female), and `flu_shot` (self-reported "have you had flu vaccine in last year?" or records, 1=yes, 0=no). It would be nice to have a selector for factors to facet by, but tell me if you want the deployed models to have fewer facets.  From point of view, the only non-negotiable facet is `site_type` as differences in the total counts and residence locations captured by our different collection modes are very important to our study partners for understanding what we collected this year.

### `Latent` field models
`Latent` field models represent an inference of the underlying force of infection in the total population over space and time, after adjusting for features of our sampling process and factors associated with our participants. The "latent field" is estimated during the estimation of the observed models above.  It represents a model of the residual variation in the data that is not explained by observed factors like `site_type`, `sex`, or `flu_shot`.  From the inferred latent field, we can produce an output I'm calling `modeled_intensity` which is an un-normalized estimate of total population incidence.  I'm not yet producing normalized incidence estimates because we (1) haven't linked to census population data yet and (2) haven't worked out what we can about "denominator data" for our sample (what fraction of people who could've participated were sick enough to partipate and willing to enroll?).  Regardless, the relative information in the `modeled_intensity`, both in time and space, can be interpreted similarly to true incidence, and through the modeled intensity, we get a picture of the dynamics of transmission. 

The most important thing the latent field model adjusts for is an estimate of the "catchment" of each `site_type`. The catchment of each site estimates how likely people at each residence location are to have participated in our study, independent of if they are sick with the pathogen of interest.  
For example, to estimate the catchment of `kiosk` collection for `h1n1pdm`, we infer a `observed` model for the expected number of participants in each residence location  *with all non-h1n1pdm infections* who partipate in kiosk sampling, aggegrated over the entire study duration (no time-dependence).  We take this as a measure of the rate at which people would interact with a kiosk, independent of being sick with `h1n1pdm`, the pathogen of interest, and averaged over the variations in space and time of the many other pathogens that also drive participation.  
The key assumptions are that (1) averaging of pathogen dynamics that are (2) independent of the specific pathogen being modeled reveals the underlying acces to kiosks and willingness to participate of people in each mapped residence location. 

Given the estimated catchment of each `site_type`, the observed number of `h1n1pdm` cases over space and time are modeled relative to the catchment.  In places where we get a lot of samples for lots of reasons, a few `h1n1pdm` samples represents a low intesity of transmission.  But in places where we get few samples other than `h1n1pdm`, we infer that flu intensity at that residence location and time is high.  The latent field model collectively estimates this intensity across the whole map, thus inferred space-time properties of the total population epidemic. 


# After May 22 demo

## Provide science documentation
What are the models (in words, equations, and code) and what do they do?

## Time-independent models
Instead of averaging over time in the client, we should model various time averages and time slices directly, corresponding to different cognitive tasks. I can generate many of these now, but I'm not expecting the viz to request them.

## Age-dependent models
Depending on the outcome of interest, age is either like a factor or like time, and so the models and viz will need to depend on the cognitive task.

## Vaccine-efficacy models
These will be an example of a different cognitive task, where the inference of interest is a parameter and not a direct transformation of data.  Line, bar, and map chart viz components will likely be similar, but context will need to be different.  This and other intervention effectiveness summaries will be really important for Year 2. 

## Incorporate census demographic data
- locate appropriate data source
- add query and formatting to dbViewR
- incorporate in incidenceModelR where appropriate

## incidenceMapR improvements
- adjacency networks
  - define lowest spatial scale based on based shapes, instead of residence_census_tract (census tract) only
  - allow adjacency network models beyond default contiguous nearest-neighbor
- many more!
