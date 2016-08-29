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

## from goo.gl API
user_history <- function(){
  
  f <- googleAuthR::gar_api_generator("https://www.googleapis.com/urlshortener/v1/url/history",
                                      "GET",
                                      data_parse_function = function(x) x)
  
  f()
}

test_that("The auth file can be found",{
  
  filep <- Sys.getenv("GAR_AUTH_FILE")
  if(filep == "") filep <- Sys.getenv("TRAVIS_GAR_AUTH_FILE")
  
  cat(filep)
  
  expect_true(file.exists(filep))
  
})

test_that("Auth file is valid",{
  
  filep <- Sys.getenv("GAR_AUTH_FILE")
  if(filep == "") filep <- Sys.getenv("TRAVIS_GAR_AUTH_FILE")
  
  token <- readRDS(Sys.getenv("TRAVIS_GAR_AUTH_FILE"))
  
  expect_s3_class(token[[1]], "Token2.0")
  
})

test_that("Can authenticate normal settings", {
  
  token <- readRDS(Sys.getenv("TRAVIS_GAR_AUTH_FILE"))
  token <- gar_auth(token[[1]])
  expect_s3_class(token, "Token2.0")
  
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
  
  uu <- user_history()
  expect_equal(uu$kind, "urlshortener#urlHistory")
  
})

context("Batching")

test_that("A batch call works", {
  
  ggg <- gar_batch(list(list_websites(), user_history()))
  
  expect_s3_class(ggg[[1]], "data.frame")
  expect_equal(ggg[[2]]$kind, "urlshortener#urlHistory")
})