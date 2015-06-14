<!-- README.md is generated from README.Rmd. Please edit that file -->
MixPanel Data Export API
========================

[![Travis-CI Build Status](https://travis-ci.org/akhmed1/mixpanelR.svg?branch=master)](https://travis-ci.org/akhmed1/mixpanelR)

mixpanelR is R interface for [MixPanel data export API](https://mixpanel.com/docs/api-documentation/data-export-api). The package provides an S4 class that can be used to issue a raw request as well as high-level function for export data directly into R data.frame.

The following sample code exports the data for the specified event into a R data.frame:

``` r
library(mixpanelR)
    
api <- MixPanel(
          api_key = "<API_KEY>"
          api_secret = "<API_SECRET>"
)
    
df <-  export(api,
          event = "project:successful_donation",
          params = list(from_date = "2015-01-01",
                        to_date = "2015-03-05")
)
    
summary(df)
```

You can install:

-   the latest development version from github with

``` r
if (packageVersion("devtools") < 1.6) {
      install.packages("devtools")
}
    
devtools::install_github("akhmed1/mixpanelR")
```

The package passes all CRAN checks and could be submitted to CRAN.
