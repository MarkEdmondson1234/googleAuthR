library(httptest)

context("Unit tests setup")

## test google project ID
options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/webmasters",
                                        "https://www.googleapis.com/auth/urlshortener"),
        googleAuthR.client_id = "201908948134-rm1ij8ursrfcbkv9koc0aqver84b04r7.apps.googleusercontent.com",
        googleAuthR.client_secret = "nksRJZ5K3nm9FUWsAtBoBArz")

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

### --  the tests

context("API Mocking")
.mockPaths("..")

test_that("Record requests if online and authenticated", {
  skip_if_disconnected()
  skip_if_no_env_auth(auth_env)
  
  capture_requests(
    {
      shorten_url("http://markedmondson.me") # make sure not a cached argument!
      gar_discovery_apis_list()
      gar_discovery_api("acceleratedmobilepageurl","v1")
    })
  
})

with_mock_API({
  context("API generator - unit")
  
  test_that("A generated API function works", {
    skip_on_cran()

    lw <- shorten_url("http://markedmondson.me")
    expect_type(lw, "character")
    
  })
  
  context("Discovery API - unit")
  
  test_that("Can get discovery API list", {
    skip_on_cran()
    
    da <- gar_discovery_apis_list()
    
    expect_equal(da$kind[[1]], "discovery#directoryItem")
    
  })
  
  test_that("Can get discovery API schema - unit", {
    skip_on_cran()
    da1 <- gar_discovery_api("acceleratedmobilepageurl","v1")
    
    expect_equal(da1$id, "acceleratedmobilepageurl:v1")
    
  })


})