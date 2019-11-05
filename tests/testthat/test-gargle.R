## this is a local httr file generated for local tests only and ignored in .gitignore
local_httr_file <- "googleAuthR_tests.httr-oauth"
auth_env <- "GAR_AUTH_FILE"
gargle_email <- "GARGLE_EMAIL"
options(googleAuthR.verbose = 3)
options(gargle_quiet = TRUE)

context("gargle new features")

test_that("Have existing local email cache", {
  skip_on_cran()
  skip_if_no_env_auth(gargle_email)
  
 gos <- gargle::gargle_oauth_sitrep()
 
 expect_true(any(gos[,"email"] %in% Sys.getenv(gargle_email) ))
  
})

test_that("Auth works by specifying email", {
  skip_on_cran()
  skip_if_no_env_auth(gargle_email)
  
  token <- gar_auth(scopes = "email", email = Sys.getenv(gargle_email))
  expect_s3_class(token, "Gargle2.0")
})

test_that("Auth works using an existing refresh token", {
  skip_on_cran()

  credentials <- list(token = "abc", refresh_token = '1//a-b-c')
  tt <- gar_auth(email='a@b.c', credentials = credentials)
  
  expect_s3_class(gar_auth(tt), "Token2.0")
  expect_equal(tt$credentials, credentials)
})

context("gargle backward compatibility")

context("Various auth types with .httr-oauth")

test_that("The auth httr-oauth file can be found",{
  skip_on_cran()
  skip_if_no_env_auth(auth_env)
  
  expect_true(file.exists(local_httr_file))
  
})

test_that("Can authenticate .httr passed as a string", {
  skip_on_cran()
  skip_if_no_env_auth(auth_env)
  
  expect_s3_class(gar_auth(local_httr_file), "Token2.0")
})


test_that("Can authenticate .httr passing a token", {
  skip_on_cran()
  skip_if_no_env_auth(auth_env)
  
  tt <- gar_auth(local_httr_file)
  
  expect_s3_class(gar_auth(tt), "Token2.0")
  
})

context("Auth with JSON")

test_that("The auth JSON file can be found and used",{
  skip_on_cran()
  skip_if_no_env_auth(auth_env)
  
  filep <- Sys.getenv("GAR_AUTH_FILE")
  
  expect_true(file.exists(filep))
  expect_equal(tools::file_ext(Sys.getenv("GAR_AUTH_FILE")), "json")
  expect_s3_class(gar_auth_service(filep), "Token2.0")
})

context("Auto authentication")

test_that("Can authenticate default auto settings", {
  skip_on_cran()
  skip_if_no_env_auth(auth_env)
  
  default_scopes <- getOption("googleAuthR.scopes.selected")
  
  token <- gar_auto_auth(default_scopes, 
                         environment_var = "GAR_AUTH_FILE")
  
  expect_s3_class(token, "Token2.0")
  
})
