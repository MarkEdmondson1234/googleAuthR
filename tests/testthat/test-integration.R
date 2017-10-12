library(httptest)

context("Setup")

.mockPaths("..")

## test google project ID
options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/webmasters",
                                        "https://www.googleapis.com/auth/urlshortener"),
        googleAuthR.client_id = "201908948134-rm1ij8ursrfcbkv9koc0aqver84b04r7.apps.googleusercontent.com",
        googleAuthR.client_secret = "nksRJZ5K3nm9FUWsAtBoBArz")

## this is a local httr file generated for local tests only and ignored in .gitignore
local_httr_file <- "googleAuthR_tests.httr-oauth"

local_auth <- Sys.getenv("GAR_AUTH") != ""
if(!local_auth){
  cat("\nNo authentication file detected - skipping integration tests\n")
} else {
  cat("\nPerforming API calls for integration tests\n")
}

on_travis <- Sys.getenv("CI") == "true"
if(on_travis){
  cat("\n#testing on CI - working dir: ", path.expand(getwd()), "\n")
} else {
  cat("\n#testing not on CI\n")
}

### --  the tests

context("Scopes")

test_that("Test scopes are set", {
  skip_on_cran()
  
  scopes <- getOption("googleAuthR.scopes.selected")
  expected_scopes <- c("https://www.googleapis.com/auth/webmasters",
                       "https://www.googleapis.com/auth/urlshortener")
  
  expect_true(all(scopes %in% expected_scopes))
  
})

context("Various auth types with .httr-oauth")

test_that("The auth httr-oauth file can be found",{
  skip_on_cran()
  skip_if_not(local_auth)
  
  expect_true(file.exists(local_httr_file))

})

test_that("Can authenticate .httr passed as a string", {
  skip_on_cran()
  skip_if_not(local_auth)
  
  expect_s3_class(gar_auth(local_httr_file), "Token2.0")
})

test_that("Can authenticate .httr looking for existing file", {
  skip_on_cran()
  skip_if_not(local_auth)
  
  options(googleAuthR.httr_oauth_cache = local_httr_file)
  expect_s3_class(gar_auth(), "Token2.0")
})

test_that("Can authenticate .httr passing a token", {
  skip_on_cran()
  skip_if_not(local_auth)
  
  tt <- gar_auth()
  
  expect_s3_class(gar_auth(tt), "Token2.0")
  
})

context("Auth with JSON")

test_that("The auth JSON file can be found and used",{
  skip_on_cran()
  skip_if_not(local_auth)
  
  filep <- Sys.getenv("GAR_AUTH_FILE")
  
  expect_true(file.exists(filep))
  expect_s3_class(gar_auth_service(filep), "Token2.0")
})

context("Auto authentication")

test_that("Can authenticate default auto settings", {
  skip_on_cran()
  skip_if_not(local_auth)
  
  default_scopes <- getOption("googleAuthR.scopes.selected")
  
  token <- gar_auto_auth(default_scopes, 
                         environment_var = "GAR_AUTH_FILE")
  
  expect_s3_class(token, "Token2.0")
  
})

