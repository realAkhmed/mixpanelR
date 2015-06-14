<!-- README.md is generated from README.Rmd. Please edit that file -->
mixpanelR
=========

[![Travis-CI Build Status](https://travis-ci.org/akhmed1/mixpanelR.svg?branch=master)](https://travis-ci.org/akhmed1/mixpanelR)

mixpanelR provides R interface for [MixPanel data export API](https://mixpanel.com/docs/api-documentation/data-export-api). More specifically, the package provides an S4 class that can be used to issue:

-   low-level API requests via `mixpanelR::request` function
-   high-level data dump request via `mixpanelR::export` function

The following sample code exports the full data dump for the specified event into a R data.frame. In this example, all variable names and types are automatically extracted within the `export` function so data.frame `df` contains the variables with correct names and types.

``` r
library(mixpanelR)

# Create the S4 object    
api <- MixPanel(
          api_key = "<API_KEY>",
          api_secret = "<API_SECRET>"
)

# High-level function: full event data dump into data.frame 
df <-  export(api,
          event = "project:successful_donation",
          params = list(from_date = "2015-01-01",
                        to_date = "2015-03-05")
)

summary(df)

# Low-level function: specific API method requests
raw_response <- request(api,
     methods = list("events"),
     params = list(
         event = list("project:successful_donation","project:loaded"),
         unit = "hour",
         interval = 24,
         type = "general"
     )
)

data <- fromJSON(raw_response)
```

You can install the package as follows:

-   the latest development version from github with

``` r
if (packageVersion("devtools") < 1.6) {
      install.packages("devtools")
}
    
devtools::install_github("akhmed1/mixpanelR")
```

The package passes all CRAN checks and could be submitted to CRAN.
