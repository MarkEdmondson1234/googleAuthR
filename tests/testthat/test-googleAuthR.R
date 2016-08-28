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

ggg <- gar_batch(list(list_websites(), user_history()))

test_that("Can authenticate default auto settings", {
  
  default_scopes <- getOption("googleAuthR.scopes.selected")
  
  token <- gar_auto_auth(default_scopes)
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