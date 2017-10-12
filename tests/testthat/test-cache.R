#' library(googleAuthR)
#' ## change the native googleAuthR scopes to the one needed.
#' options(googleAuthR.client_id = "201908948134-rm1ij8ursrfcbkv9koc0aqver84b04r7.apps.googleusercontent.com",
#'         googleAuthR.client_secret = "nksRJZ5K3nm9FUWsAtBoBArz",
#'         googleAuthR.scopes.selected = "https://www.googleapis.com/auth/urlshortener")
#' #' Shortens a url using goo.gl
#' #'
#' #' @param url URl to shorten with goo.gl
#' #' 
#' #' @return a string of the short URL
#' shorten_url <- function(url){
#'   
#'   body = list(
#'     longUrl = url
#'   )
#'   
#'   f <- gar_api_generator("https://www.googleapis.com/urlshortener/v1/url",
#'                          "POST",
#'                          data_parse_function = function(x) x$id)
#'   
#'   f(the_body = body)
#'   
#' }
#' 
#' 
#' gar_auth()
#' 
#' options(googleAuthR.verbose = 2)
#' 
#' ## normal API fetch
#' shorten_url("http://markedmondson.me")
#' 
#' gar_cache_setup()
#' 
#' ## first time no cache
#' shorten_url("http://markedmondson.me")
#' 
#' ## second time cached
#' shorten_url("http://markedmondson.me")
#' 
#' # cache to file system
#' gar_cache_setup(memoise::cache_filesystem("mock"))
#' 
#' ## first time no cache
#' shorten_url("http://markedmondson.me")
#' 
#' ## second time cached
#' shorten_url("http://markedmondson.me")
#' 
#' ## can restart R and run below and get same result (e.g. without auth)
#' library(googleAuthR)
#' shorten_url <- function(url){
#'   
#'   body = list(
#'     longUrl = url
#'   )
#'   
#'   f <- gar_api_generator("https://www.googleapis.com/urlshortener/v1/url",
#'                          "POST",
#'                          data_parse_function = function(x) x$id)
#'   
#'   f(the_body = body)
#'   
#' }
#' # cache to file system
#' gar_cache_setup(memoise::cache_filesystem("mock"))
#' 
#' ## from cache
#' shorten_url("http://markedmondson.me")
#' 
