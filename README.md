# googleAuthR - Google API R Client

[![CRAN](http://www.r-pkg.org/badges/version/googleAuthR)](https://cran.r-project.org/package=googleAuthR)
[![Travis-CI Build Status](https://travis-ci.org/MarkEdmondson1234/googleAuthR.svg?branch=master)](https://travis-ci.org/MarkEdmondson1234/googleAuthR)
[![codecov](https://codecov.io/gh/MarkEdmondson1234/googleAuthR/branch/master/graph/badge.svg)](https://codecov.io/gh/MarkEdmondson1234/googleAuthR)

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

Get more details at the [googleAuthR website](http://code.markedmondson.me/googleAuthR/)

The [`googleAuthRverse`](https://googleauthrverse.slack.com) Slack team has been setup for support for using `googleAuthR` and the libraries it helps create.  Sign up via this [Google form](https://goo.gl/forms/d541yrJoDFMrrSJp1) to get access. 

## R Google API libraries using googleAuthR

Here is a list of [available Google APIs](https://developers.google.com/apis-explorer/#p/) to make with this library.
The below libraries are all cross-compatible as they use `googleAuthR` for authentication backend e.g. can use just one OAuth2 login flow and can be used in multi-user Shiny apps. 

* [googleComputeEngineR](https://cloudyr.github.io/googleComputeEngineR/) - Google Compute Engine VMs API
* [searchConsoleR](http://code.markedmondson.me/searchConsoleR/) - Search Console API
* [bigQueryR](http://code.markedmondson.me/bigQueryR/) - BigQuery API. Part of the cloudyr project.
* [googleAnalyticsR](http://code.markedmondson.me/googleAnalyticsR/) - Google Analytics API
* [googleTagManagerR](https://github.com/IronistM/googleTagManageR) - Google Tag Manager API by IronistM
* [googleID](https://github.com/MarkEdmondson1234/googleID) - Simple user info from G+ API for Shiny app authentication flows.
* [googleCloudStorageR](http://code.markedmondson.me/googleCloudStorageR/) - Google Cloud Storage API
* [RoogleVision](https://github.com/cloudyr/RoogleVision) - R Package for Image Recogntion, Object Detection, and OCR using the Google's Cloud Vision API
* [googleLanguageR](https://github.com/MarkEdmondson1234/googleLanguageR) - Access Speech to Text, Entity analysis and translation APIs from R

Feel free to add your own via email or a pull request if you have used googleAuthR to build something cool. 

`googleAuthR` now has an R package generator which makes R package skeletons you can use to build your own Google API R package upon.  Browse through the 154 options at this [Github repository](https://github.com/MarkEdmondson1234/autoGoogleAPI).

## Example Shiny app

An example shiny app with Google authentication is [deployed to shinyapps.io here](https://mark.shinyapps.io/googleAuthRexample/).   It uses the example app that is available in `system.file("shiny", package="googleAuthR")`

## Thanks to

* Jenny Bryan and her work on the [googlesheets](https://github.com/jennybc/googlesheets) package that this work derives from.
* Hadley Wickham for [httr's OAuth2](https://github.com/hadley/httr) excellence
* RStudio team for [Shiny](http://shiny.rstudio.com/)
* [Johann de Boer](https://github.com/jdeboer) for some code contributions.

## Install

googleAuthR is available on CRAN

```r
install.packages("googleAuthR")
```

Check out [News](NEWS.md) to see the features of the development version.

If you want to use the development version on Github, install via:

```r
devtools::install_github("MarkEdmondson1234/googleAuthR")
```

