# cache global
.gar_cache <- new.env(parent = emptyenv())
.gar_cache$cache <- NULL  # what type of caching

#' Set cache location
#' 
#' These functions let you set the cache behaviour for your API calls
#' 
#' @param cache The directory to save cache to, or \code{"memory"} to save to RAM
#' 
#' 
#' @export
gar_cache_set_loc <- function(cache){
  myMessage("Set API cache", level = 3)
  .gar_cache$cache <- cache
}

#' @rdname gar_cache_set_loc
#' @export
gar_cache_get_loc <- function(){
  cache <- .gar_cache$cache
  if(!is.null(cache)){
    myMessage("Getting API cache func", level = 1)
  }
  cache
}

#' @rdname gar_cache_set_loc
#' @export
gar_cache_empty <- function(){
  myMessage("Deleting API cache", level = 3)
  .gar_cache$cache <- NULL
}


#' Setup which package to perform mock testing upon
#' 
#' This helps make sure the right functions are mocked
#' 
#' @param mcache A cache method from \link[memoise]{memoise}. If \code{NULL} will do no caching.
#' 
#' @description 
#' 
#' To cache to a file system use \code{memoise::cache_filesystem("cache_folder")}, 
#'   suitable for unit testing and works between R sessions. 
#' 
#' The cached API calls do not need authentication to be active, but need this function to set caching first. 
#' 
#' @export
gar_cache_setup <- function(mcache=memoise::cache_memory()){
  
  if(is.null(mcache)){
    return(gar_cache_empty())
  }
  
  gar_cache_set_loc(mcache)
  
  
}

memDoHttrRequest <- function(req_url,
                             shiny_access_token,
                             request_type,
                             the_body,
                             customConfig,
                             simplifyVector){
  myMessage("Using caching", level = 3)
  
  cachedHttrRequest <- memoise::memoise(doHttrRequest, 
                                        cache = gar_cache_get_loc())
  
  existing_cache <- memoise::has_cache(cachedHttrRequest)(
    req_url,
    shiny_access_token=shiny_access_token,
    request_type=request_type,
    the_body=the_body,
    customConfig=customConfig,
    simplifyVector=simplifyVector
  )
  
  if(existing_cache){
    myMessage("Reading cache file", level = 3)
  } else {
    myMessage("Make cache file", level = 3)
  }
  
  cachedHttrRequest(req_url,
                    shiny_access_token=shiny_access_token,
                    request_type=request_type,
                    the_body=the_body,
                    customConfig=customConfig,
                    simplifyVector=simplifyVector)
}
