# library(testthat)
# library(googleAuthR)
# library(shiny)
# library(RSelenium)
# library(callr)
# 
# context("Shiny")
# 
# 
# test_that("Can authenticate in Shiny", {  
#   skip_on_cran()
#   do_auth_flow(remDr)
#   
#   expect_true(file.exists("auth_success.rds"), info = "The Shiny Auth failed")
# })
# 
