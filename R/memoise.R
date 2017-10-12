# cache global
.gar_cache <- new.env(parent = emptyenv())
.gar_cache$cache <- NULL  # what type of caching
.gar_cache$invalid <- function() {TRUE} # whether to invalidate when passed req



#' These functions let you set the cache behaviour for your API calls
#' 
#' @param cache The directory to save cache to, or \code{"memory"} to save to RAM
#' @param invalid_func A function that takes API response, and returns \code{TRUE} or \code{FALSE} whether caching takes place. Default cache everything. 
#' 
#' @noRd
gar_cache_set_loc <- function(cache, invalid_func){
  myMessage("Set API cache", level = 3)
  
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
#' @return \code{TRUE} is successful.
#' 
#' @export
#' @family cache functions
gar_cache_setup <- function(mcache=memoise::cache_memory(),
                            invalid_func = function(req){req$status_code == 200}){
  
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

  cachedHttrRequest <- memoise(doHttrRequest, cache = gar_cache_get_loc())

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
    myMessage("Making new cache", level = 3)
  }
  
  req <- cachedHttrRequest(req_url,
                           shiny_access_token=shiny_access_token,
                           request_type=request_type,
                           the_body=the_body,
                           customConfig=customConfig,
                           simplifyVector=simplifyVector)
  
  ## check request against cache_function to see whether to cache result is TRUE
  cache_function <- .gar_cache$invalid
  
  if(!cache_function(req)){
    myMessage("Forgetting cache", level = 3)
    forget(cachedHttrRequest)
  } else {
    myMessage("Passed cache_function", level = 1)
  }
  
  req
}


#' @noRd
#' @import memoise
#' @family cache functions
memDoBatchRequest <- function(l){
  
  cachedBatchedRequest <- memoise(doBatchRequest, cache = gar_cache_get_loc())
  
  existing_cache <- has_cache(cachedBatchedRequest)(l)
  
  if(existing_cache){
    myMessage("Reading batched cache", level = 3)
  } else {
    myMessage("Making new batched cache", level = 3)
  }
  
  req <- cachedBatchedRequest(l)
  
  ## check request against cache_function to see whether to cache result is TRUE
  cache_function <- .gar_cache$invalid
  
  if(!cache_function(req)){
    myMessage("Forgetting cache", level = 3)
    forget(cachedBatchedRequest)
  } else {
    myMessage("Passed cache_function", level = 1)
  }
  
  req
  
  
}
