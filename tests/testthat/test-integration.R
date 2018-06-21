context("Integration tests setup")

## this is a local httr file generated for local tests only and ignored in .gitignore
local_httr_file <- "googleAuthR_tests.httr-oauth"
auth_env <- "GAR_AUTH_FILE"

shorten_url <- function(url){
  
  body = list(
    longUrl = url
  )
  
  f <- gar_api_generator("https://www.googleapis.com/urlshortener/v1/url",
                         "POST",
                         data_parse_function = function(x) x$id)
  
  f(the_body = body)
  
}

shorten_url_cache <- function(url){
  
  body = list(
    longUrl = url
  )
  
  f <- gar_api_generator("https://www.googleapis.com/urlshortener/v1/url",
                         "POST",
                         data_parse_function = function(x) x)
  
  f(the_body = body)
  
}


### -----  the tests

context("Scopes")

test_that("Can set scopes and client id/secret from file", {
  skip_on_cran()
  skip_if_no_env_auth(auth_env)
  
  scopes <- c("https://www.googleapis.com/auth/webmasters",
              "https://www.googleapis.com/auth/urlshortener")
  
  pid <- gar_set_client(json = Sys.getenv("GAR_CLIENT_JSON"),
                        scopes = scopes)
  
  expect_equal(getOption("googleAuthR.scopes.selected"), scopes)
  expect_true(getOption("googleAuthR.client_id") != "")
  expect_true(getOption("googleAuthR.client_secret") != "")  
  
})

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

test_that("Right message when wrong token file location given", {
  skip_on_cran()
  skip_if_no_env_auth(auth_env)
  skip_if_not(!interactive())
  
  options(googleAuthR.httr_oauth_cache = "httr-oauth.rds")
  
  expect_error(gar_auth())
  
})

context("Auth with JSON")

test_that("The auth JSON file can be found and used",{
  skip_on_cran()
  skip_if_no_env_auth(auth_env)
  
  filep <- Sys.getenv("GAR_AUTH_FILE")
  
  expect_true(file.exists(filep))
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

context("Caching")

test_that("Can do an in memory cache", {
  skip_on_cran()
  skip_if_no_env_auth(auth_env)
  
  gar_cache_setup()
  gar_auth("googleAuthR_tests.httr-oauth")
  one <- shorten_url("http://markedmondson.me")
  two <- shorten_url("http://markedmondson.me")
  
  expect_message(shorten_url("http://markedmondson.me"), "Reading cache")
  expect_equal(one, two)
  
})

test_that("Can do disk memory cache", {
  skip_on_cran()
  skip_if_no_env_auth(auth_env)
  
  gar_cache_empty()
  file_cache <- "mock"
  gar_cache_setup(mcache = memoise::cache_filesystem(file_cache))
  
  gar_auth("googleAuthR_tests.httr-oauth")
  one <- shorten_url("http://code.markedmondson.me")
  two <- shorten_url("http://code.markedmondson.me")
  expect_message(shorten_url("http://code.markedmondson.me"), "Reading cache")
  expect_equal(one, two)
  expect_true(length(list.files(file_cache)) > 0)
  
})



test_that("Can do cache and use invalidate function", {
  skip_on_cran()
  skip_if_no_env_auth(auth_env)
  
  gar_cache_empty()
  file_cache <- "mock"
  
  ## only cache if this URL
  gar_cache_setup(invalid_func = function(req){
    req$content$longUrl == "http://code.markedmondson.me/"
  })
  
  gar_auth("googleAuthR_tests.httr-oauth")
  shorten_url_cache("http://code.markedmondson.me")
  
  ## read cache
  expect_message(shorten_url("http://code.markedmondson.me"), "Reading cache")
  
  options(googleAuthR.verbose = 2)
  ## dont cache me
  expect_message(shorten_url_cache("http://blahblah.com"), "Forgetting cache")
  
  gar_cache_empty()
})

context("Previous bugs to solve")

test_that("Encoding parameters works (#100 & #101)", {
  skip_on_cran()
  skip_if_no_env_auth(auth_env)
  
  fileSearch <- function(query) {
    gar_api_generator("https://www.googleapis.com/drive/v3/files/",
                                   "GET",pars_args=list(q=query))()
  }
  
  gar_auth(Sys.getenv("GAR_TEST_DRIVE_FILE"))
  
  searchResponse <- fileSearch("mimeType != 'application/vnd.google-apps.folder'")
  
  expect_equal(searchResponse$status_code, 200)
  
  op <- options()
  options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/bigquery")
  googleAuthR::gar_auth_service(Sys.getenv("BQ_AUTH_FILE"))
  
  tables <- bigQueryR::bqr_list_tables(projectId = Sys.getenv("BQ_DEFAULT_PROJECT_ID"),
                                       datasetId = Sys.getenv("BQ_DEFAULT_DATASET"), 
                                       maxResults = 10)
  
  expect_s3_class(tables, "data.frame")
  options(op)
  
})

test_that("Can do batching with caching (#106)", {
  skip_on_cran()
  skip_if_no_env_auth(auth_env)
  
  googleAuthR::gar_auth(Sys.getenv("GAR_TEST_GA_FILE"))
  
  f <- function(req){
    if(!is.null(req$content$reports)){
      return(TRUE)
    } else {
      return(FALSE)
    }}
  
  googleAuthR::gar_cache_setup(invalid_func = f)
  
  ## get rid of a silly warning
  expect_false(suppressWarnings(googleAnalyticsR:::is.error("g")))
  
  batched_call <- googleAnalyticsR::google_analytics_3(
    id = c(115751114, 123875646),
    start = "2017-01-01", end = "2017-08-01",
    metrics = "sessions",
    dimensions = c("date","deviceCategory","medium"),
    max_results = 10, multi_account_batching = TRUE
  )
  
  expect_equal(class(batched_call), "list")
  
})


#### -- these tests are in the unit tests online

context("API generator")

test_that("A generated API function works", {
  skip_on_cran()
  skip_if_no_env_auth(auth_env)
  
  gar_auth("googleAuthR_tests.httr-oauth")
  lw <- shorten_url("http://code.markedmondson.me")
  expect_type(lw, "character")
  
})

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

