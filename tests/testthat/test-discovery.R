library(testthat)
library(googleAuthR)


context("Discovery API")

test_that("Can get discovery API list", {
  skip_on_cran()
  da <- gar_discovery_apis_list()
  
  expect_equal(da$kind[[1]], "discovery#directoryItem")
  
})

test_that("Can get discovery API schema", {
  skip_on_cran()
  da1 <- gar_discovery_api("acceleratedmobilepageurl","v1")
  
  expect_equal(da1$id, "acceleratedmobilepageurl:v1")
  
})

test_that("Can create auto package", {
  skip_on_cran()
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

# test_that("Can create vision package", {
#   skip_on_cran()
#   tmp <- tempdir()
#   on.exit(unlink(tmp))
#   
#   da1 <- gar_discovery_api("vision","v1")
#   
#   gg <- gar_create_package(da1, tmp, check = TRUE, github = FALSE)
#   
#   expect_s3_class(gg, "check_results")
#   
# })

test_that("Name of package is valid",{
  skip_on_cran()
  tmp <- tempdir()
  on.exit(unlink(tmp))
  
  da1 <- gar_discovery_api("admin","datatransfer_v1")
  
  gg <- gar_create_package(da1, tmp, check = TRUE, github = FALSE)
  
  expect_s3_class(gg, "check_results")
  
})

test_that("c unrecognised escape character is avoided",{
  skip_on_cran()
  tmp <- tempdir()
  on.exit(unlink(tmp))
  
  da1 <- gar_discovery_api("bigquery","v2")
  
  gg <- gar_create_package(da1, tmp, check = TRUE, github = FALSE)
  
  expect_s3_class(gg, "check_results")
  
})

test_that("googleservicemanagement is valid",{
  skip_on_cran()
  tmp <- tempdir()
  on.exit(unlink(tmp))
  
  da1 <- gar_discovery_api("servicemanagement","v1")
  
  gg <- gar_create_package(da1, tmp, check = TRUE, github = FALSE)
  
  expect_s3_class(gg, "check_results")
  
})