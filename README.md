# googleAuthR - Google API Client Library for R

[![CRAN](http://www.r-pkg.org/badges/version/googleAuthR)](https://cran.r-project.org/package=googleAuthR)
[![Travis-CI Build Status](https://travis-ci.org/MarkEdmondson1234/googleAuthR.svg?branch=master)](https://travis-ci.org/MarkEdmondson1234/googleAuthR)
[![Coverage Status](https://img.shields.io/codecov/c/github/MarkEdmondson1234/googleAuthR/master.svg)](https://codecov.io/github/MarkEdmondson1234/googleAuthR?branch=master)

Auto-build libraries for Google APIs with OAuth2 for both local and Shiny app use.
This guide is also available at the [googleAuthR website](http://code.markedmondson.me/googleAuthR/)

## Table of Contents

* [Example libraries](https://github.com/MarkEdmondson1234/googleAuthR#r-google-api-libraries-using-googleauthr)
* [Thanks](https://github.com/MarkEdmondson1234/googleAuthR#thanks-to)
* [Installation](https://github.com/MarkEdmondson1234/googleAuthR#install)
* [Overview](https://github.com/MarkEdmondson1234/googleAuthR#overview)
* [Google API Console Setup](https://github.com/MarkEdmondson1234/googleAuthR#google-api-setup)
* [Building your own Google API functions](https://github.com/MarkEdmondson1234/googleAuthR#generating-your-function)
* [Batching API Calls](https://github.com/MarkEdmondson1234/googleAuthR#batching-api-requests)
* [Service Account Authentication with JSON](https://github.com/MarkEdmondson1234/googleAuthR#authentication-with-a-json-file-via-service-accounts)
* [Authentication with Shiny](https://github.com/MarkEdmondson1234/googleAuthR#authentication-with-shiny)
* [Complete Example making a goo.gl R library](https://github.com/MarkEdmondson1234/googleAuthR#example-with-googl)

## R Google API libraries using googleAuthR

Here is a list of [available Google APIs](https://developers.google.com/apis-explorer/#p/) to make with this library.
The below libraries are all cross-compatible as they use `googleAuthR` for authentication backend e.g. can use just one OAuth2 login flow and can be used in multi-user Shiny apps. 
* [searchConsoleR](http://code.markedmondson.me/searchConsoleR/) - Search Console API
* [bigQueryR](http://code.markedmondson.me/bigQueryR/) - BigQuery API. Part of the cloudyr project.
* [googleAnalyticsR](http://code.markedmondson.me/googleAnalyticsR/) - Google Analytics API
* [gtmR](https://github.com/MarkEdmondson1234/gtmR) - Google Tag Manager API (in progress)
* [googleID](https://github.com/MarkEdmondson1234/googleID) - Simple user info from G+ API for Shiny app authentication flows.
* [googleCloudStorageR](http://code.markedmondson.me/googleCloudStorageR/) - Google Cloud Storage API
* [RoogleVision](https://github.com/cloudyr/RoogleVision) - R Package for Image Recogntion, Object Detection, and OCR using the Google's Cloud Vision API

Feel free to add your own via email or a pull request if you have used googleAuthR to build something cool. 

`googleAuthR` now has an R package generator which makes R package skeletons you can use to build your own Google API R package upon.  Browse through the 154 options at this [Github repository](https://github.com/MarkEdmondson1234/autoGoogleAPI).

## Example Shiny app

An example shiny app with Google authentication is [deployed to shinyapps.io here](https://mark.shinyapps.io/googleAuthRexample/).   It uses the example app that is available in `system.file("shiny", package="googleAuthR")`
## Thanks to:
* Jenny Bryan and her work on the [googlesheets](https://github.com/jennybc/googlesheets) package that this work derives from.
* Hadley Wickham for [httr's OAuth2](https://github.com/hadley/httr) excellence
* RStudio team for [Shiny](http://shiny.rstudio.com/)
* [Johann de Boer](https://github.com/jdeboer) for some code contributions.

## Install

GoogleAuthR version 0.3.0 is now available on CRAN
```r
install.packages("googleAuthR")
```
Check out [News](NEWS.md) to see the features of the development version.
If you want to use the development version on Github, install via:
```r
## load the library or download it if necessary
if(!require(googleAuthR)){
  if(!require(devtools)){
    install.packages("devtools")
  } else {
    devtools::install_github("MarkEdmondson1234/googleAuthR")
  }
}
library(googleAuthR)
```

## Overview

This guide is available at: `vignette("googleAuthR")`
This library allows you to authenticate easily via local use in an OAuth2 flow; within a Shiny app; or via service accounts. 
The main two functions are `gar_auth()` and `gar_api_generator()`.

### `gar_auth`

This takes care of getting the authentication token, storing it and refreshing. 
Use it before any call to a Google library.

### `gar_api_generator`

This creates functions for you to use to interact with Google APIs.
Use it within your own function definitions, to query the Google API you want.

## Google API Setup

`googleAuthR` has a default project setup with APIs activated for several APIs, but it is recommended you use your own Client IDs as the login screen will be big and scary for users with so many APIs to approve.  
It is preferred to configure your functions to only use the scopes they need.  Scopes you need will be specified in the Google API documentation. 
Set scopes via the option `googleAuthR.scopes.selected`.
The below example sets scopes for Search Console, Google Analytics and Tag Manager:
```r
options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/webmasters",
                                          "https://www.googleapis.com/auth/analytics",
                                          "https://www.googleapis.com/auth/tagmanager.readonly"))
```

### Set up steps

1. Set up your project in the Google API Console to use the Google API you want.

#### For local use

2. Click 'Create a new Client ID', and choose "Installed Application".
3. Note your Client ID and secret.
4. Modify these options after `googleAuthR` has been loaded:
  + `options("googleAuthR.client_id" = "YOUR_CLIENT_ID")`
  + `options("googleAuthR.client_secret" = "YOUR_CLIENT_SECRET")`
  
#### For Shiny use

2. Click 'Create a new Client ID', and choose "Web Application".
3. Note your Client ID and secret.
4. Add the URL of where your Shiny app will run, with no port number. e.g. https://mark.shinyapps.io/searchConsoleRDemo/
5. And/Or also put in localhost or 127.0.0.1 with a port number for local testing. Remember the port number you use as you will need it later to launch the app e.g. `http://127.0.0.1:1221`
6. In your Shiny script modify these options:
  + `options("googleAuthR.webapp.client_id" = "YOUR_CLIENT_ID")`
  + `options("googleAuthR.webapp.client_secret" = "YOUR_CLIENT_SECRET")`
7. Run the app locally specifying the port number you used e.g. `shiny::runApp(port=1221)`
8. Or deploy to your Shiny Server that deploys to web port (80 or 443).

#### Activate API

1. Click on "APIs"
2. Select and activate the API you want to use.
3. Go to the documentation and find the API scope URL
4. Set option in your R script for the scope e.g. 
```r
options("googleAuthR.scopes.selected" = 
      c("https://www.googleapis.com/auth/urlshortener"))
```

## Building your own functions

If the above is successful, then you should go through the Google login flow in your browser when you run this command:
```r
googleAuthR::gar_auth()
```
If you ever need to authenticate with a new user, use:
```r
googleAuthR::gar_auth(new_user=TRUE)
```
Authentication token is cached in a hidden file called `.httr-oauth` in the working directory.

## Authentication with no browser

If for some reason you need authentication without access to a browser (for example when using Shiny Server), then you can authenticate locally and upload the `.httr-oauth` file to the folder of your script.

## Authentication with Shiny

If you want to create a Shiny app just using your data, upload the app with your own `.httr-oauth`.
If you want to make a multi-user Shiny app, where users login to their own Google account and the app works with their data, googleAuthR provides these functions to help make the Google login process as easy as possible.
As of 0.3.0 googleAuthR uses [Shiny Modules](http://shiny.rstudio.com/articles/modules.html).  This means less code and the ability to have multiple login buttons on the same app.
* `googleAuth` - creates the authentication token and login button styling
* `googleAuthUI` - creates the server side login button for users to authenticate with.
* `with_shiny()` - wraps your API functions so they can be passed the user's authentication token.
#### Shiny authentication example
This is the example [deployed to shinyapps.io here](https://mark.shinyapps.io/googleAuthRexample/)
```r
## in global.R
library(googleAuthR)
library(shiny)
options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/urlshortener")
options(googleAnalyticsR.webapp.client_id = "YOUR_PROJECT_KEY")
options(googleAnalyticsR.webapp.client_secret = "YOUR_CLIENT_SECRET")
shorten_url <- function(url){
  
  body = list(
    longUrl = url
  )
  
  f <- gar_api_generator("https://www.googleapis.com/urlshortener/v1/url",
                         "POST",
                         data_parse_function = function(x) x$id)
  
  f(the_body = body)
  
}
## server.R
source("global.R")
server <- function(input, output, session){
  
  ## Create access token and render login button
  access_token <- callModule(googleAuth, "loginButton")
  
  short_url_output <- eventReactive(input$submit, {
    ## wrap existing function with_shiny
    ## pass the reactive token in shiny_access_token
    ## pass other named arguments
    with_shiny(f = shorten_url, 
               shiny_access_token = access_token(),
               url=input$url)
    
  })
  
  output$short_url <- renderText({
    
    short_url_output()
    
  })
}
## ui.R
ui <- fluidPage(
  googleAuthUI("loginButton"),
  textInput("url", "Enter URL"),
  actionButton("submit", "Shorten URL"),
  textOutput("short_url")
)
### If the above global.R, server.R and ui.R files are in folder "test" like so:
## /home
##    |->/test/
##            /global.R
##            /ui.R
##            /server.R
##
## Port 1221 has been set in your Google Project options as the port to listen to
## as explained in authentication setup section
## run below in /home directory
shiny::runApp("./test/", launch.browser=T, port=1221)
```
 
## Authentication with a JSON file via Service Accounts

You can also authenticate single users via a server side JSON file rather than going through the online OAuth2 flow.  The end user could supply this JSON file, or you can upload your own JSON file to your applications. 
This involves downloading a secret JSON key with the authentication details.  More details are available from Google here: Using OAuth2.0 for Server to Server Applications[https://developers.google.com/identity/protocols/OAuth2ServiceAccount]
To use, go to your Project in the Google Developement Console and select JSON Key type.  Save the JSON file to your computer and supply the file location to the function
`gar_auth_service()`
  
Navigate to the JSON file from the Google Developer Console via: 
Credentials > New credentials > Service account Key > Select service account > Key type = JSON
      
An example using a service account JSON file for authentication is shown below:
```r
library(googleAuthR)
service_token <- gar_auth_service(json_file="~/location/of/the/json/secret.json")
analytics_url <- function(shortUrl, 
                          timespan = c("allTime", "month", "week","day","twoHours")){
  
  timespan <- match.arg(timespan)
  
  f <- gar_api_generator("https://www.googleapis.com/urlshortener/v1/url",
                         "GET",
                         pars_args = list(shortUrl = "shortUrl",
                                          projection = "FULL"),
                         data_parse_function = function(x) { 
                           a <- x$analytics 
                           return(a[timespan][[1]])
                         })
  
  f(pars_arguments = list(shortUrl = shortUrl))
}
analytics_url("https://goo.gl/2FcFVQbk")
```

## Authentication via RStudio Addin

From version `0.3.0` a RStudio Addin is available via the RStudio Addin menu once you load the package, or via `googleAuthR:::gar_gadget()`
It lets you set the scopes and then saves you some typing by calling the Google authentication flow for you.
![googleAuthRGadget](https://storage.googleapis.com/mark-edmondson-public-files/myObject)

## Authentication in RMarkdown via JavaScript

From version `0.4.0` there are two functions that can be called from within RMarkdown for authentication.  They use JavaScript, rather than R/Shiny to authenticate, as an RMarkdown document can not read the URL tokens.

A demo and example are available here: `https://mark.shinyapps.io/googleAuthRMarkdown/`

### Setup

The RMarkdown document YAML needs runtime shiny and to be a HTML document:

```
output: html_document
runtime: shiny
```

Locally, you have to run the RMarkdown document on the specified port configured in Google console (`1221` for the default shared project of `googleAuthR`), configured via `options(shiny.port = 1221)`

This means you shouldn’t launch the RMarkdown via the Run button in RStudio as that starts a new R session without your set options.

Instead set the options and run via `rmarkdown::run("myfile.Rmd")`

```r
options(shiny.port = 1221)
options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/plus.me")
rmarkdown::run("googleAuthRMarkdown.Rmd")
```

When publishing, you also need to add the domain to the Javascript origins in the Google API console. Use `127.0.0.1:XXX` where XXX is your chosen Shiny port for local testing.

### Example of RMarkdown authentication

Below creates a button that when clicked makes a popup for Google authentication:

```r
library(googleAuthR)

gar_auth_jsUI("auth_demo", login_text = "Click Me")

```
The authentication token is available via the server side module command:

```
auth <- callModule(gar_auth_js, "auth_demo")
```
Pass the auth token to API functions. Below example using googleID to return G+ user info.
```
# devtools::install_github("MarkEdmondson1234/googleID")
library(googleID)

user_info <- reactive({
  
  req(auth())
  
  with_shiny(get_user_info,
             shiny_access_token = auth())
  
})
```
You can now output the user data taken from the G+ API:

```
## creates an output
renderUI({
  
  req(user_info())
  
  h1("Hello ", user_info()$displayName)
  
})
```

## Auto-authentication

From version `0.4.0` auto-authentication can be performed upon a package load.

This requires the setup of environment variables either in your `.Renviron` file or via `Sys.setenv()` to point to a previously created authentication file.  This file can be either a `.httr-oauth` file created via `gar_auth()` or a Google service account JSON downloaded from the Google API console.


This file will then be used for authentication via `gar_auth_auto`.  You can call this function yourself in scripts or R sessions, but its main intention is to be called in the `.onAttach` function via `gar_attach_auth_auto`, so that you will authenticate right after you load the library via `library(yourlibrary)`

An example from `googleCloudStorageR` is shown below:

```r
.onAttach <- function(libname, pkgname){

  googleAuthR::gar_attach_auto_auth("https://www.googleapis.com/auth/devstorage.full_control",
                                    environment_var = "GCS_AUTH_FILE")
}

```

..which calls an environment variable set in `~/.Renvion`:

```
GCS_AUTH_FILE="/Users/mark/auth/my_auth_file.json"
```


## Revoking Authentication

For local use, delete the `.httr-oauth` file.
For service level accounts delete the JSON file.
For a Shiny app, a cookie is left by Google that will mean a faster login next time a user uses the app with no Authorization screen that they get the first time through.  To force this every time, activate the parameter `revoke=TRUE` within the `googleAuth` function.

# Build a Google API library 

## Generating your function

Creating your own API should then be a matter of consulting the Google API documentation, and filling in the required details.
`gar_api_generator` has these components:
* `baseURI` - all APIs have a base for every API call
* `http_header` - what type of request, most common are GET and POST
* `path_args` - some APIs need you to alter the URL folder structure when calling, e.g. `/account/{accountId}/` where `accountId` is variable.
* `pars_args` - other APIS require you to send URL parameters e.g. `?account={accountId}` where `accountId` is variable.
* `data_parse_function` - [optional] If the API call returns data, it will be available in `$content`. You can create a parsing function that transforms it in to something you can work with (for instance, a dataframe)

Example below for generating a function:

```r
  f <- gar_api_generator("https://www.googleapis.com/urlshortener/v1/url",
                         "POST",
                         data_parse_function = function(x) x$id)
```

## Using your generated function

The function generated uses `path_args` and `pars_args` to create a template, but when the function is called you will want to pass dynamic data to them.  This is done via the `path_arguments` and `pars_arguments` parameters.
`path_args` and `pars_args` and `path_arguments` and `pars_arguments` all accept named lists.
If a name in `path_args` is present in `path_arguments`, then it is substituted in.  This way you can pass dynamic parameters to the constructed function.  Likewise for `pars_args` and `pars_arguments`.

```r
## Create a function that requires a path argument /accounts/{accountId}
  f <- gar_api_generator("https://www.googleapis.com/example",
                         "POST",
                         path_args = list(accounts = "defaultAccountId")
                         data_parse_function = function(x) x$id)
                             
## When using f(), pass the path_arguments function to it 
## with the same name to modify "defaultAccountId":
  result <- f(path_arguments = list(accounts = "myAccountId"))
```

### Body data

A lot of Google APIs look for you to send data in the Body of the request.  This is done after you construct the function.
 `googleAuthR` uses `httr`'s JSON parsing via `jsonlite` to construct JSON from R lists.
 
 Construct your list, then use `jsonlite::toJSON` to check if its in the correct format as specified by the Google documentation.  This is often the hardest part using the API.
 
### Parsing data

Not all API calls return data, but if they do:
If you have no `data_parse_function` then the function returns the whole request object.  The content is available in `$content`.  You can then parse this yourself, or pass a function in to do it for you.
If you parse in a function into `data_parse_function`, it works on the response's `$content`.

Example below of the differences between having a data parsing function and not:

```r
  ## the body object that will be passed in
  body = list(
    longUrl = "http://www.google.com"
  )
  
  ## no data parsing function
  f <- gar_api_generator("https://www.googleapis.com/urlshortener/v1/url",
                         "POST")
                         
  no_parse <- f(the_body = body)
  
  ## parsed data, only taking request$content$id
  f2 <- gar_api_generator("https://www.googleapis.com/urlshortener/v1/url",
                          "POST",
                          data_parse_function = function(x) x$id)
  
  parsed <- f2(the_body = body)
  
  ## str(no_parse) has full details of API response.
  ## just looking at no_parse$content as this is what API returns
  > str(no_parse$content)
  List of 3
   $ kind   : chr "urlshortener#url"
   $ id     : chr "http://goo.gl/ZwT9pG"
   $ longUrl: chr "http://www.google.com/"
 
  ## compare to the above - equivalent to no_parse$content$id 
  > str(parsed)
   chr "http://goo.gl/mCYw2i"
                             
```
The response is turned from JSON to a dataframe if possible, via `jsonlite::fromJSON`

### Skip parsing

In some cases you may want to skip all parsing of API content, perhaps if it is not JSON or some other reason.
For these cases, you can use the option `option("googleAuthR.rawResponse" = TRUE)` to skip all tests and return the raw response.
Here is an example of this from the googleCloudStorageR library:
```r
gcs_get_object <- function(bucket, 
                           object_name){
  ## skip JSON parsing on output as we epxect a CSV
  options(googleAuthR.rawResponse = TRUE)
  
  ## do the request
  ob <- googleAuthR::gar_api_generator("https://www.googleapis.com/storage/v1/",
                                       path_args = list(b = bucket,
                                                        o = object_name),
                                       pars_args = list(alt = "media"))
  req <- ob()
  
  ## set it back to FALSE for other API calls.
  options(googleAuthR.rawResponse = FALSE)
  req
}
```

### Batching API requests

If you are doing many API calls, you can speed this up a lot by using the batch option.
This takes the API functions you have created and wraps them in the `gar_batch` function to request them all in one POST call.  You then recieve the responses in a list.
Note that this does not count as one call for API limits purposes, it just speeds up the processing.
The example below queries from two different APIs and returns them in a list: It lists websites in your Google Search Console, and shows your goo.gl link history.

```r
## from search console API
list_websites <- function() {
  
  l <- gar_api_generator("https://www.googleapis.com/webmasters/v3/sites",
                                      "GET",
                                      data_parse_function = function(x) x$siteEntry)
  l()
}
## from goo.gl API
user_history <- function(){
  f <- gar_api_generator("https://www.googleapis.com/urlshortener/v1/url/history",
                         "GET",
                         data_parse_function = function(x) x$items)
  
  f()
}
googleAuthR::gar_auth(new_user=T)
ggg <- gar_batch(list(list_websites(), user_history()))
```

#### Walking through batch requests

A common batch task is to walk through the same API call, modifying only one parameter.  An example includes walking through Google Analytics API calls by date to avoid sampling.
A function to enable this is implemented at `gar_batch_walk`, with an example below:
```r
walkData <- function(ga, ga_pars, start, end){
  dates <- as.character(
    seq(as.Date(start, format="%Y-%m-%d"),
        as.Date(end, format="%Y-%m-%d"),
        by=1))
  ga_pars$samplingLevel <- "HIGHER_PRECISION"
  anyBatchSampled <- FALSE
  samplePercent   <- 0
  
  
  ## this is applied to each batch to keep tally of meta data
  bf <- function(batch_data){
    lapply(batch_data, function(the_data) {
      if(attr(the_data, 'containsSampledData')) anyBatchSampled <<- TRUE
      samplePercent <<- samplePercent + attr(the_data, "samplePercent")
    })
    batch_data
  }
  ## the walk through batch function. 
  ## In this case both start-date and end-date are set to the date iteration
  ## if the output is parsed as a dataframe, it also includes a rbind function
  ## otherwise, it will return a list of lists
  walked_data <- googleAuthR::gar_batch_walk(ga,
                                             dates,
                                             gar_pars = ga_pars,
                                             pars_walk = c("start-date", "end-date"),
                                             batch_function = bf,
                                             data_frame_output = TRUE)
  message("Walked through all dates. Total Results: [", NROW(walked_data), "]")
  attr(walked_data, "dateRange") <- list(startDate = start, endDate = end)
  attr(walked_data, "totalResults") <- NROW(walked_data)
  attr(walked_data, "samplingLevel") <- "HIGHER_PRECISION, WALKED"
  attr(walked_data, "containsSampledData") <- anyBatchSampled
  attr(walked_data, "samplePercent") <- samplePercent / length(dates)
  walked_data
}
```

## Auto-build libraries

New in `0.4` is helper functions that use Google's [API Discovery service](https://developers.google.com/discovery/).

This is a meta-API which holds all the necessary details to build a supported Google API, which is all modern Google APIs.  At the time of writing this is 152 libraries.

These libraries aren't intended to be submitted to CRAN or used straight away, but should take away a lot of documentation and function building work so you can concentrate on tests, examples and helper functions for your users.

Get a list of the current APIs via `gar_discovery_apis_list()`

```r
all_apis <- gar_discovery_apis_list()
```

To get details of a particular API, use its name and version in the `gar_discovery_api()` function:

```r
a_api <- gar_discovery_api("urlshortener", "v1")
```

You can then pass this list to `gar_create_package()` along with a folder path to create all the files necessary for an R library.  There are arguments to set it up with RStudio project files, do a `CRAN CMD check` and upload it to Github.

```r
vision_api <- gar_discovery_api("vision", "v1")
gar_create_package(vision_api,
                   "/Users/mark/dev/R/autoGoogleAPI/",
                   rstudio = FALSE,
                   github = FALSE)

```

### Auto-build all libraries

A loop to build all the Google libraries is shown below, the results of which is available in this [Github repo](https://github.com/MarkEdmondson1234/autoGoogleAPI).

```r
library(googleAuthR)

api_df <- gar_discovery_apis_list()

api_json_list <- mapply(gar_discovery_api, api_df$name, api_df$version)

## WARNING: this takes a couple of hours
check_results <- lapply(api_json_list, 
                        gar_create_package, 
                        directory = "/Users/mark/dev/R/autoGoogleAPI",
                        github = FALSE)

```

## Example with goo.gl

Below is an example building a link shortner R package using `googleAuthR`.
It was done referring to the [documentation for Google URL shortener](https://developers.google.com/url-shortener/v1/getting_started).
Note the help docs specifies the steps outlined above. These are in general the steps for every Google API.

1. Creating a project
2. Activate API
3. Provide scope
4. Specify the base URL (in this case `https://www.googleapis.com/urlshortener/v1/url`)
5. Specify the httr request type e.g. `POST`
6. Constructing a body request
7. Giving the response format

### Example goo.gl R library
```r
library(googleAuthR)
## change the native googleAuthR scopes to the one needed.
options("googleAuthR.scopes.selected" = 
        c("https://www.googleapis.com/auth/urlshortener"))
#' Shortens a url using goo.gl
#'
#' @param url URl to shorten with goo.gl
#' 
#' @return a string of the short URL
shorten_url <- function(url){
  
  body = list(
    longUrl = url
  )
  
  f <- gar_api_generator("https://www.googleapis.com/urlshortener/v1/url",
                         "POST",
                         data_parse_function = function(x) x$id)
  
  f(the_body = body)
  
}
#' Expands a url that has used goo.gl
#'
#' @param shortUrl Url that was shortened with goo.gl
#' 
#' @return a string of the expanded URL
expand_url <- function(shortUrl){
  
  f <- gar_api_generator("https://www.googleapis.com/urlshortener/v1/url",
                         "GET",
                         pars_args = list(shortUrl = "shortUrl"),
                         data_parse_function = function(x) x)
                         
  f(pars_arguments = list(shortUrl = shortUrl))
  
}
#' Get analyitcs of a url that has used goo.gl
#'
#' @param shortUrl Url that was shortened with goo.gl
#' @param timespan The time period for the analytics data
#' 
#' @return a dataframe of the goo.gl Url analytics
analytics_url <- function(shortUrl, 
                          timespan = c("allTime", "month", "week","day","twoHours")){
  
  timespan <- match.arg(timespan)
    
  f <- gar_api_generator("https://www.googleapis.com/urlshortener/v1/url",
                         "GET",
                         pars_args = list(shortUrl = "shortUrl",
                                          projection = "FULL"),
                         data_parse_function = function(x) { 
                                    a <- x$analytics 
                                    return(a[timespan][[1]])
                                    })
  
  f(pars_arguments = list(shortUrl = shortUrl))
}
#' Get the history of the authenticated user
#' 
#' @return a dataframe of the goo.gl user's history
user_history <- function(){
  f <- gar_api_generator("https://www.googleapis.com/urlshortener/v1/url/history",
                         "GET",
                         data_parse_function = function(x) x$items)
  
  f()
}
```
To use the above functions:
```r
library(googleAuthR)
# go through authentication flow
gar_auth()
s <- shorten_url("http://markedmondson.me")
s
expand_url(s)
analytics_url(s, timespan = "month")
user_history()
```