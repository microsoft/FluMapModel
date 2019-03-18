---
title: "Description of predictModelTestPkg"
author: "Mike Famulare (IDM)"
date: "2019-02-13"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{modelDescription}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---


## Overview

This vignette documents the R package "predictModelTestPkg" that provides a rudimentary space-time logistic regression model fit to very fake data. The documentation and API will get better over time.  First, I describe the API calling the trained model at our URL.  After that, I'll go into the raw data that went into the mapping model, the model, and its inputs and outputs. 

## The API

A cached model that was trained to (very) fake data described below is currently hosted at [http://40.112.165.255/flu](http://40.112.165.255/flu).    Example POST queries are shown in [example_request.md](https://github.com/InstituteforDiseaseModeling/Seattle-Flu-Incidence-Mapper/blob/master/example_request.md)

### Input

Model queries need to be sent via JSON.  Right now, there are only two valid JSON requests--one for the fitted model and one for the data.

**Request fitted model**

```
{
  "output": ["incidence"],
  "attributes": ["Census_Tract", "vax_status"],
  "basis": ["year"],
  "values": ["incidence_median", "incidence_sd"]
} 
```
This query will return a JSON object with the model estimates of the incidence of flu in ILI broken down by *Census_Tract* (11 digit GEOID) and *vax_status* (0 = not vaccinated this year, 1 == vaccinated this year), with data points as the tuple *year, incidence_median, incidence_sd*.   This is the default returned if no query is sent. 

**Request raw data**

```
{
  "output": ["observed"],
  "attributes": ["Census_Tract", "vax_status"],
  "basis": ["year"],
  "values": ["flu_count", "nsamples"]
} 
```
This query will return a JSON object with the model estimates of the oberved counts of flu positives in ILI broken down by *Census_Tract* (11 digit GEOID) and *vax_status* (0 = not vaccinated this year, 1 == vaccinated this year), with data points as the tuple *year, flu_count, nsamples*.   This is the default returned if no query is sent. 

**Query schema (embryonic)** 

(Better schema and documentation to come!)

The categories of *output* can be *observed* or *incidence*. (Obvious improvements would be *data* and *model*...). 

The *attributes* describe the requested data groupings.  Currently allowed value is only both *["Census_Tract", "vax_status"]*. (Independence and more options is coming.)

The *basis* describes the "x-axis variable."  Right now, only *year* is available (decimal year, each data point is one week apart in this version, but that is subject to change), but *age* will be another.  Those are the two obvious bases for these models, but we may want more. 

The *values* describe the output data requested.  Right now, the only valid query for *output:incidence* is *"values": ["incidence_median", "incidence_sd"]*.  And for *output:observed*, only *"values": ["flu_count", "nsamples"]*.  (Harmonized variables and independence are coming.)

Right now, there is no way to filter by census tract, date range, or other factors, but that will change soon too.  

### Output
The server will return the output as JSON containing the query and the data.

For example, if you request the *incidence* JSON above, you will get back

```
{
  "output": ["incidence"],
  "attributes": ["Census_Tract", "vax_status"],
  "basis": ["year"],
  "values": ["incidence_median", "incidence_sd"],
  "data": [
    {
      "Census_Tract": 53003960100,
      "vax_status": 0,
      "data": [
        {
          "year": 2018.8462,
          "incidence_median": 0.0031,
          "incidence_sd": 0.211
        },
        {
          "year": 2018.8654,
          "incidence_median": 0.0055,
          "incidence_sd": 0.1815
        },
        {
          "year": 2018.8846,
          "incidence_median": 0.012,
          "incidence_sd": 0.164
        },
        {
          "...": "..."
        }
      ]
    },
    {
      "Census_Tract": 53005010400,
      "vax_status": 0,
      "data": [
        {
          "year": 2018.8462,
          "incidence_median": 0.0212,
          "incidence_sd": 0.2186
        },
        {
          "...": "..."
        }
      ]
    }
  ]
} 
```

Unlike the input query schema, I think this is closer to the real format we'll have.  R is inherently row-based, and so it's natural to have each point as it's own tuple instead of an array like {"year" : [2018.8462, 2018.8654, ...]}.  If you really want arrays, I'm sure we can coerce it in the R or python outer layer, but it may add to latency. 



## King County map
Here's a map of census tracts in King County, WA.  This defines the domain of the simulated data and subseqent analysis.  Overlaid is the nearest-neighbor connectivity graph used for spatial smoothing (this version doesn't link Vashon Island to the mainland, but this will change).


















