library(testthat)
library(googleAuthR)

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

test_that("The auth file can be found",{
  
  filep <- Sys.getenv("GAR_AUTH_FILE")
  if(filep == "") filep <- Sys.getenv("TRAVIS_GAR_AUTH_FILE")
  

  cat(getwd())
  cat(filep, "\n")
  cat(list.files())
  cat("\n")
  
  if(filep != "") expect_true(file.exists(filep))
  
})

test_that("Can authenticate .httr-oauth settings", {
  
    
  expect_s3_class(gar_auth("httr-oauth.rds"), "Token2.0")

  
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

context("API generator")

test_that("A generated API function works", {
  
  lw <- list_websites()
  expect_s3_class(list_websites(), "data.frame")
  
})

test_that("Another generated API function works", {
  
  uu <- google_analytics_account_list()
  expect_equal(uu$kind, "analytics#accountSummaries")
  
})

context("Batching")

test_that("A batch call works", {
  
  ggg <- gar_batch(list(list_websites(), google_analytics_account_list()))
  
  expect_s3_class(ggg[[1]], "data.frame")
  expect_equal(ggg[[2]]$kind, "analytics#accountSummaries")
})