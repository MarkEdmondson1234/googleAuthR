library(testthat)
library(googleAuthR)
options(googleAuthR.httr_oauth_cache = "httr-oauth.rds")

context("Auth")

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


test_that("The auth JSON file can be found",{
  
  filep <- Sys.getenv("GAR_AUTH_FILE")
  if(filep == "") filep <- Sys.getenv("TRAVIS_GAR_AUTH_FILE")
  
  cat("\n==auth JSON file location==\n")
  cat("=dir: ", getwd(),"\n")
  cat("=file: ",filep, "\n")
  cat("=list files:", list.files(),"\n")
  cat("\n")
  
  if(filep != "") expect_true(file.exists(filep))
  
})

test_that("The auth httr file can be found",{
  
  filep <- getOption("googleAuthR.httr_oauth_cache")
  
  if(class(filep) == "character") {
    expect_true(file.exists(filep))
  } else {
    skip("No httr file found")
  }
  
})

test_that("Can authenticate .httr passed as a file", {
  
  expect_s3_class(gar_auth("httr-oauth.rds"), "Token2.0")
  
})


test_that("Can authenticate .httr looking for existing file", {
  
  expect_s3_class(gar_auth(), "Token2.0")
  
})

test_that("Can authenticate .httr passing a token", {
  
  tt <- gar_auth()
  
  expect_s3_class(gar_auth(tt), "Token2.0")
  
})


test_that("Can authenticate JSON settings", {
  
  filep <- Sys.getenv("GAR_AUTH_FILE")
  if(filep == "") filep <- Sys.getenv("TRAVIS_GAR_AUTH_FILE")
  
  if(filep != ""){
    
    expect_s3_class(gar_auth_service(filep), "Token2.0")
    
  } else {
    skip("No auth file found")
  }
  
})

test_that("Test scopes are set", {
  
  scopes <- getOption("googleAuthR.scopes.selected")
  expected_scopes <- c("https://www.googleapis.com/auth/webmasters",
                       "https://www.googleapis.com/auth/analytics",
                       "https://www.googleapis.com/auth/analytics.readonly",
                       "https://www.googleapis.com/auth/analytics.manage.users.readonly",
                       "https://www.googleapis.com/auth/tagmanager.readonly",
                       "https://www.googleapis.com/auth/urlshortener")
  
  expect_true(all(scopes %in% expected_scopes))
  
})


test_that("Can authenticate default auto settings", {
  
  default_scopes <- getOption("googleAuthR.scopes.selected")
  
  token <- gar_auto_auth(default_scopes, 
                         environment_var = "GAR_AUTH_FILE", 
                         travis_environment_var = "TRAVIS_GAR_AUTH_FILE")
  
  expect_s3_class(token, "Token2.0")
  
})

test_that("Right message when wrong token file location given", {
  
  options(googleAuthR.httr_oauth_cache = "httr-oauth.rds")
  
  expect_s3_class(gar_auth(), "Token2.0")
  
})

context("API generator")

test_that("A generated API function works", {
  
  lw <- list_websites()
  expect_s3_class(list_websites(), "data.frame")
  
})

test_that("Another generated API function works", {
  
  uu <- google_analytics_account_list()
  expect_equal(uu$kind, "analytics#accountSummaries")
  
})



context("Discovery API")

test_that("Can get discovery API list", {
  
  da <- gar_discovery_apis_list()
  
  expect_equal(da$kind[[1]], "discovery#directoryItem")
  
})

test_that("Can get discovery API schema", {
  
  da1 <- gar_discovery_api("acceleratedmobilepageurl","v1")
  
  expect_equal(da1$id, "acceleratedmobilepageurl:v1")
  
})

test_that("Can create auto package", {
  
  tmp <- tempdir()
  on.exit(unlink(tmp))
  
  da1 <- gar_discovery_api("urlshortener","v1")
  
  gar_create_package(da1, tmp, check = FALSE, github = FALSE)
  
  created_file_dir <- "googleurlshortenerv1.auto"
  
  expect_true(created_file_dir %in% list.files(tmp))
  
  created_files <- c("DESCRIPTION", 
                     "googleurlshortenerv1.auto.Rproj", 
                     "LICENSE", 
                     "man/AnalyticsSnapshot.Rd", 
                     "man/AnalyticsSummary.Rd", 
                     "man/StringCount.Rd", 
                     "man/url.get.Rd", 
                     "man/url.insert.Rd", 
                     "man/url.list.Rd", 
                     "man/Url.Rd", 
                     "man/UrlHistory.Rd", 
                     "man/urlshortener_googleAuthR.Rd", 
                     "NAMESPACE", 
                     "R/urlshortener_functions.R", 
                     "R/urlshortener_objects.R", 
                     "README.md")
  
  actual_files <- list.files(file.path(tmp, "googleurlshortenerv1.auto"), 
                             recursive = TRUE)
  
  expect_true(all(created_files %in% actual_files))
  
})

test_that("Can create vision package", {
  
  tmp <- tempdir()
  on.exit(unlink(tmp))
  
  da1 <- gar_discovery_api("vision","v1")
  
  gg <- gar_create_package(da1, tmp, check = TRUE, github = FALSE)
  
  expect_s3_class(gg, "check_results")
  
})


