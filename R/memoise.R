# cache global
.gar_cache <- new.env(parent = emptyenv())
.gar_cache$cache <- NULL  # what type of caching

# whether to invalidate when passed req
.gar_cache$invalid <- function(req){tryCatch(req$status_code == 200, error = function(x) FALSE)} 



#' These functions let you set the cache behaviour for your API calls
#' 
#' @param cache The directory to save cache to, or \code{"memory"} to save to RAM
#' @param invalid_func A function that takes API response, and returns \code{TRUE} or \code{FALSE} whether caching takes place. Default cache everything. 
#' 
#' @noRd
gar_cache_set_loc <- function(cache, invalid_func){
  myMessage("Set API cache", level = 2)
  
  .gar_cache$cache <- cache
  .gar_cache$invalid <- invalid_func
  
  TRUE
}

#' @rdname gar_cache_setup
#' @export
#' @family cache functions
gar_cache_get_loc <- function(){
  .gar_cache$cache
}

#' @rdname gar_cache_setup
#' @export
#' @family cache functions
gar_cache_empty <- function(){
  myMessage("Deleting API cache", level = 3)
  .gar_cache$cache <- NULL
  TRUE
}


#' Setup where to put cache
#' 
#' 
#' @param mcache A cache method from \link[memoise]{memoise}. 
#' @param invalid_func A function that takes API response, and returns \code{TRUE} or \code{FALSE} whether caching takes place. Default cache everything. 
#' 
#' @description 
#' 
#' To cache to a file system use \code{memoise::cache_filesystem("cache_folder")}, 
#'   suitable for unit testing and works between R sessions. 
#' 
#' The cached API calls do not need authentication to be active, but need this function to set caching first. 
#' 
#' @return \code{TRUE} if successful.
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' # demo function to cache within
#' shorten_url_cache <- function(url){
#'   body = list(longUrl = url)
#'   f <- gar_api_generator("https://www.googleapis.com/urlshortener/v1/url",
#'                       "POST",
#'                       data_parse_function = function(x) x)
#'  f(the_body = body)
#'  
#'  }
#'  
#'  ## only cache if this URL
#'  gar_cache_setup(invalid_func = function(req){
#'       req$content$longUrl == "http://code.markedmondson.me/"
#'  })
#'  
#'  # authentication
#'  gar_auth()
#'  ## caches
#'  shorten_url_cache("http://code.markedmondson.me")
#'  
#'  ## read cache
#'  shorten_url("http://code.markedmondson.me")
#'  
#'  ## ..but dont cache me
#'  shorten_url_cache("http://blahblah.com")
#' 
#' }
#' 
#' @export
#' @family cache functions
gar_cache_setup <- function(mcache=memoise::cache_memory(),
                            invalid_func = function(req){tryCatch(req$status_code == 200, error = function(x) FALSE)}){
  
  if(is.null(mcache)){
    return(gar_cache_empty())
  }
  
  gar_cache_set_loc(mcache, invalid_func = invalid_func)
  
}

#' @import memoise
#' @family cache functions
#' @noRd
memDoHttrRequest <- function(req_url,
                             shiny_access_token,
                             request_type,
                             the_body,
                             customConfig,
                             simplifyVector){

  cachedHttrRequest <- memoise(
    doHttrRequest, 
    cache = gar_cache_get_loc())

  existing_cache <- has_cache(cachedHttrRequest)(
    req_url,
    shiny_access_token=shiny_access_token,
    request_type=request_type,
    the_body=the_body,
    customConfig=customConfig,
    simplifyVector=simplifyVector
  )
  
  if(existing_cache){
    myMessage("Reading cache", level = 3)
  } else {
    myMessage("Making new cache", level = 1)
  }
  
  req <- cachedHttrRequest(
    req_url,
    shiny_access_token=shiny_access_token,
    request_type=request_type,
    the_body=the_body,
    customConfig=customConfig,
    simplifyVector=simplifyVector)
  
  ## check request against cache_function to see whether to cache result is TRUE
  cache_function <- .gar_cache$invalid

  cache_result <- tryCatch({
    cache_function(req)
  }, error = function(ex){
    warning("Error in cache function", call. = FALSE)
    FALSE
  })

  if(!cache_result){
    myMessage("Forgetting cache", level = 1)
    forget(cachedHttrRequest)
  } else {
    myMessage("Passed cache_function", level = 1)
  }
  
  req
}


#' @noRd
#' @import memoise
#' @family cache functions
memDoBatchRequest <- function(l, batch_endpoint){
  
  cachedBatchedRequest <- memoise(doBatchRequest, cache = gar_cache_get_loc())
  
  existing_cache <- has_cache(cachedBatchedRequest)(l, batch_endpoint)
  
  if(existing_cache){
    myMessage("Reading batched cache", level = 3)
  } else {
    myMessage("Making new batched cache", level = 2)
  }
  
  req <- cachedBatchedRequest(l, batch_endpoint)
  
  ## check request against cache_function to see whether to cache result is TRUE
  cache_function <- .gar_cache$invalid

  cache_result <- tryCatch({
    cache_function(req)
  }, error = function(ex){
    warning("Error in batch cache function", call. = FALSE)
    FALSE
  })
  
  if(!cache_result){
    myMessage("Forgetting cache", level = 1)
    forget(cachedBatchedRequest)
  } else {
    myMessage("Passed cache_function", level = 1)
  }
  
  req
  
  
}
