#' Get a list of Google API libraries
#' 
#' Doesn't require authentication
#' 
#' @seealso \href{https://developers.google.com/discovery/v1/reference/apis/list}
#' 
#' @return List of Google APIs and their resources
#' @export
gar_discovery_apis_list <- function(){
  
  url <- "https://www.googleapis.com/discovery/v1/apis"
  req <- httr::RETRY("GET", url)
  
  httr::stop_for_status(req)
  
  stuff <- httr::content(req, as = "text")
  apis <- jsonlite::fromJSON(stuff)
  
  if(!is.null(apis$kind) && apis$kind == "discovery#directoryList"){
    out <- apis$items
  } else {
    stop("Problem fetching Discovery APIs")
  }
  
  out
}

#' Get meta data details for specified Google API
#' 
#' Doesn't require authentication
#' 
#' @param api The API to fetch
#' @param version The API version to fetch
#' 
#' @seealso \href{https://developers.google.com/discovery/v1/reference/apis/getRest}
#' 
#' @return Details of the API 
#' @export
gar_discovery_api <- function(api, version){
  
  url <- sprintf("https://www.googleapis.com/discovery/v1/apis/%s/%s/rest",
                 api,
                 version)
  
  req <- httr::RETRY("GET", url)
  
  httr::stop_for_status(req)
  
  stuff <- httr::content(req, as = "text")
  api <- jsonlite::fromJSON(stuff)
  
  if(!is.null(api$kind) && api$kind == "discovery#restDescription"){
    out <- api
  } else {
    stop("Problem fetching API Description")
  }
  
  out
}

