#' Check token scopes
## check if scopes are set correctly
check_cached_scopes <- function(){
  
  httr_cache <- getOption("googleAuthR.httr_oauth_cache")
  if(!file.exists(httr_cache)){
    myMessage("No cache file found to check scopes", level = 2)
    return(NULL)
  }
  google_token <- tryCatch({
    readRDS(httr_cache)
  }, error = function(ex){
    stop("Problem reading cached token from ", normalizePath(httr_cache))
  })
  
  current_scopes <- google_token$params$scope
  option_scopes  <- getOption("googleAuthR.scopes.selected")
  
  if(!all(current_scopes %in% option_scopes)){
    warning(paste0("option(googleAuthR.scopes.selected) not same scopes as current cached token ", 
                   httr_cache, ", may cause issues with authentication.  
                     \nCurrent Token scopes: ", 
                   paste(google_token$params$scope, collapse = " "),
                   "\ngetOption(googleAuthR.scopes.selected): ",
                   paste(getOption("googleAuthR.scopes.selected"), collapse = " "),
                   collapse = " "), call. = FALSE)
    return(FALSE)
  }
  
  TRUE
  
}

#' Check that token appears to be legitimate
#'
#' This unexported function exists to catch tokens that are technically valid,
#' i.e. `inherits(token, "Token2.0")` is TRUE, but that have dysfunctional
#' credentials.
#'
#' @keywords internal
#' @family authentication functions
is_legit_token <- function(x) {
  
  if(!inherits(x, "Token2.0")) {
    myMessage("Not a Token2.0 object. Found:", class(x), level=2)
    if(!inherits(x, "list")){
      if(inherits(x[[1]], "Token2.0")){
        warning("Passed a list of Token2.0 objects, not a Token2.0 object.")
        return(FALSE)
      }
    }
    return(FALSE)
  }
  
  if("invalid_client" %in% unlist(x$credentials)) {
    # check for validity so error is found before making requests
    # shouldn't happen if id and secret don't change
    myMessage("Authorization error. Please check client_id and client_secret.", level=3)
    
    return(FALSE)
  }
  
  if("invalid_request" %in% unlist(x$credentials)) {
    # known example: if user clicks "Cancel" instead of "Accept" when OAuth2
    # flow kicks to browser
    myMessage("Authorization error. No access token obtained.", level=3)
    return(FALSE)
  }
  
  ## check if current cached token has same scopes as those set in options
  check_cached_scopes()
  
  TRUE
}

#' Check if authorization currently in force
#'
#' @return logical
#'
#' @keywords internal
#' @family authentication functions
#' @import assertthat
token_exists <- function() {
  
  token <- Authentication$public_fields$token
  
  if(is.null(token)) {
    
    myMessage("No authorization yet in this session!", level=3)
    httr_cache <- getOption("googleAuthR.httr_oauth_cache")
    assert_that(is.string(httr_cache))
    
    if(file.exists(httr_cache)) {
      myMessage(paste("NOTE: a ", httr_cache ,
                      " file exists in current working",
                      "directory.\n Run authentication function to use the",
                      "credentials cached for this session."), level=3)
    } else {
      myMessage(paste("No ", httr_cache ,
                      " file exists in current working directory.",
                      " Do library authentication steps to provide credentials."), level=3)
    }
    
    return(FALSE)
  } else {
    myMessage("Token exists.", level=2)
    return(TRUE)      
  }
}

#' Check API data token
#'
#' @param shiny_access_token auth token
#'
#' @return boolean if it works.
#'
#' @keywords internal
#' @family data fetching functions
checkTokenAPI <- function(shiny_access_token=NULL){
  
  if(any(which(grepl("with_mock_API", as.character(sys.calls()))))){
    myMessage("Skipping token checks as using with_mock_API", level = 3)
    return(TRUE)
  }
  
  if(!is.null(gar_cache_get_loc())){
    myMessage("Skipping token checks as using cache", level = 3)
    return(TRUE)
  }
  
  if(is.null(shiny_access_token)){
    ## local token
    token <- Authentication$public_fields$token
    
    if(token_exists() && is_legit_token(token)) {
      myMessage("Valid local token", level = 1)
      return(TRUE)
    } else {
      myMessage("Invalid local token", level = 1)
      return(FALSE)
    }
    
  } else {
    ## is it a valid shiny token passed?
    if(is_legit_token(shiny_access_token)){
      myMessage("Valid Shiny token", level = 1)
      return(TRUE)
    } else {
      myMessage("Invalid Shiny token", level = 1)
      return(FALSE)
    }
  }
  
}

#' Get Google API errors
#'
#' @param req a httr request
#'
#' @keywords internal
#' @importFrom httr content stop_for_status
#' @importFrom jsonlite fromJSON
checkGoogleAPIError <- function(req){
  
  ok_content_types=getOption("googleAuthR.ok_content_types")
  
  ga.json <- content(req,
                     as = "text",
                     type = "application/json",
                     encoding = "UTF-8")
  
  if(is.null(ga.json)) {
    warning("JSON parsing was NULL")
    return(FALSE)
  }
  
  if(nchar(ga.json) > 0) {
    ga.json <- fromJSON(ga.json)
  } else {
    warning("No JSON content detected", call. = FALSE)
    return(FALSE)
  }
  
  ## make all checks to headers lowercase (#78)
  names(req$headers) <- tolower(names(req$headers))
  
  if(!is.null(req$headers$`content-type`)){
    ## charset not strictly required so "application/json" 
    ##  doesn't fail "application/json; charset=UTF-8" (#78)
    if(!(any(startsWith(ok_content_types, req$headers$`content-type`)))) {
      stop(sprintf(paste("Not expecting content-type to be:\n%s"),
                   req$headers[["content-type"]]), call. = FALSE)
      
    }
  } else {
    warning("No content-type returned.")
    return(FALSE)
  }
  
  ## get error message from API
  if (!is.null(ga.json$error$message)) {
    gar_token_info(2)
    stop("API returned: ", paste(ga.json$error$message), call. = FALSE)
  }
  
  stop_for_status(req)
  
  TRUE
}
