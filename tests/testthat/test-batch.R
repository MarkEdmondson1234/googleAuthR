library(testthat)
library(googleAuthR)
options(googleAuthR.httr_oauth_cache = "httr-oauth.rds")
## from search console API
list_websites <- function() {
  
  l <- googleAuthR::gar_api_generator("https://www.googleapis.com/webmasters/v3/sites",
                                      "GET",
                                      data_parse_function = function(x) x$siteEntry)
  l()
}

## from google Analytics API
google_analytics_account_list <- function(){
  
  url <- "https://www.googleapis.com/analytics/v3/management/accountSummaries"
  acc_sum <- gar_api_generator(url,
                               "GET",
                               data_parse_function = function(x) x)
  
  acc_sum()
}

test_walk <- function(){
  
  siteURL <-  "http://sites.google.com/a/markedmondson.me/home/"
  startDate <- Sys.Date() - 93
  endDate <- Sys.Date() - 3
  dimensions <- NULL
  searchType <- c("web")
  aggregationType <- "auto"
  rowLimit <-  1000
  
  startDate <- as.character(startDate)
  endDate   <- as.character(endDate)  
  
  siteURL <- utils::URLencode(siteURL, reserved=T)
  
  body <- list(
    startDate = startDate,
    endDate = endDate,
    dimensions = as.list(dimensions),  
    searchType = searchType,
    dimensionFilterGroups = list(
      list( ## you don't want more than one of these until different groupType available
        groupType = "and", ##only one available for now
        filters = NULL
      )
    ),
    aggregationType = aggregationType,
    rowLimit = rowLimit
  )
  
  search_analytics_g <- 
    googleAuthR::gar_api_generator("https://www.googleapis.com/webmasters/v3/",
                                   "POST",
                                   path_args = list(sites = "siteURL",
                                                    searchAnalytics = "query"),
                                   data_parse_function = function(x) x
    )
      
  ## byBatch uses API batching, but this pulls out less data
  ## 0 impression keywords not included. 
  walk_vector <- seq(0, rowLimit, 5000)
  
  googleAuthR::gar_batch_walk(search_analytics_g,
                              walk_vector = walk_vector,
                              gar_paths = list(sites = siteURL),
                              body_walk = "startRow",
                              the_body = body,
                              batch_size = 3, 
                              data_frame_output = FALSE)
  
}

context("Batching")

test_that("Can authenticate JSON settings (Batch)", {
  skip_on_cran()
  filep <- Sys.getenv("GAR_AUTH_FILE")
  if(filep == "") filep <- Sys.getenv("TRAVIS_GAR_AUTH_FILE")
  
  if(filep != ""){
    
    expect_s3_class(gar_auth_service(filep), "Token2.0")
    
  } else {
    skip("No auth file found")
  }
  
})

test_that("A batch call works", {
  skip_on_cran()
  ggg <- gar_batch(list(list_websites(), google_analytics_account_list()))
  
  expect_s3_class(ggg[[1]], "data.frame")
  expect_equal(ggg[[2]]$kind, "analytics#accountSummaries")
})

test_that("A walk batch call works but with no returned data", {
  skip_on_cran()
  gg <- test_walk()
  expect_type(gg, "list")
})
