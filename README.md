# googleAuthR - Google API R Client

## gargle backend

As of version `googleAuthR>=1.0.0` the OAuth2 and service JSON authentication is provided by [gargle](https://gargle.r-lib.org/index.html).  Refer to that documentation for details.

The plan is to migrate as much functionality to `gargle` from `googleAuthR`, but backward compatibility will be maintained for all packages depending on `googleAuthR` in the meantime. 

Once there is feature parity, client packages can then migrate totally to `gargle`.  At time of writing some of the major features not in `gargle` yet are:

* Shiny authentication flows
* Paging
* Caching
* Batching

If you are not using the above then you can use `gargle` directly now.  Otherwise you can still use `googleAuthR` that will use the features of `gargle` and wait for more features to be migrated.

## Overview

This library allows you to authenticate easily via local use in an OAuth2 flow; within a Shiny app; or via service accounts. 

The main two functions are `gar_auth()` and `gar_api_generator()`.

### `gar_auth`

This takes care of getting the authentication token, storing it and refreshing. 
Use it before any call to a Google library.

### `gar_api_generator`

This creates functions for you to use to interact with Google APIs.
Use it within your own function definitions, to query the Google API you want.

## Summary

Auto-build libraries for Google APIs with OAuth2 for both local and Shiny app use.

Get more details at the [googleAuthR website](https://code.markedmondson.me/googleAuthR/)

The [`googleAuthRverse`](https://googleauthrverse.slack.com/) Slack team has been setup for support for using `googleAuthR` and the libraries it helps create.  Sign up via this [Google form](https://goo.gl/forms/d541yrJoDFMrrSJp1) to get access. 

## R Google API libraries using googleAuthR

Here is a list of [available Google APIs](https://developers.google.com/apis-explorer/#p/) to make with this library.
The below libraries are all cross-compatible as they use `googleAuthR` for authentication backend e.g. can use just one OAuth2 login flow and can be used in multi-user Shiny apps. 

* [googleComputeEngineR](https://cloudyr.github.io/googleComputeEngineR/) - Google Compute Engine VMs API
* [searchConsoleR](https://code.markedmondson.me/searchConsoleR/) - Search Console API
* [bigQueryR](https://code.markedmondson.me/bigQueryR/) - BigQuery API. Part of the cloudyr project.
* [googleAnalyticsR](https://github.com/8-bit-sheep/googleAnalyticsR/) - Google Analytics API
* [googleTagManagerR](https://github.com/IronistM/googleTagManageR) - Google Tag Manager API by IronistM
* [googleID](https://github.com/MarkEdmondson1234/googleID) - Simple user info from G+ API for Shiny app authentication flows.
* [googleCloudStorageR](https://code.markedmondson.me/googleCloudStorageR/) - Google Cloud Storage API
* [RoogleVision](https://github.com/cloudyr/googleCloudVisionR) - R Package for Image Recogntion, Object Detection, and OCR using the Google's Cloud Vision API
* [googleLanguageR](https://github.com/ropensci/googleLanguageR) - Access Speech to Text, Entity analysis and translation APIs from R
* [googleCloudRunner](https://code.markedmondson.me/googleCloudRunner/) - Continuous Development and Integration with Cloud Run, Cloud Scheduler and Cloud Build

Feel free to add your own via email or a pull request if you have used googleAuthR to build something cool. 

`googleAuthR` now has an R package generator which makes R package skeletons you can use to build your own Google API R package upon.  Browse through the 154 options at this [Github repository](https://github.com/MarkEdmondson1234/autoGoogleAPI).

## Thanks to

* Jenny Bryan and her work on the [googlesheets](https://github.com/jennybc/googlesheets) package that this work derives from.
* Hadley Wickham for [httr's OAuth2](https://github.com/r-lib/httr) excellence
* RStudio team for [Shiny](https://shiny.posit.co/)
* [Johann de Boer](https://github.com/jdeboer) for some code contributions.

## Install

googleAuthR is available on CRAN

```r
install.packages("googleAuthR")
```

Check out [News](NEWS.md) to see the features of the development version.

If you want to use the development version on Github, install via:

```r
remotes::install_github("MarkEdmondson1234/googleAuthR")
```

