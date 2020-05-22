#' Check a token vs options
#' 
#' Useful for debugging authentication issues
#' 
#' @param token A token to check, default current live session token
#' 
#' @return \code{FALSE} if the options and current token do not match, \code{TRUE} if they do.
#' 
#' @details 
#' 
#' Will compare the passed token's settings and compare to set options.  
#'   If these differ, then reauthentication may be needed.
#' 
#' @export
gar_check_existing_token <- function(token = .auth$cred){
  
  cache_path <- scopes <- client_id <- client_secret <- FALSE
  
  if(is.null(token)){
    myMessage("No local token found in session", level = 2)
    return(FALSE)
  }
  
  cache_path <- is.different(token$cache_path, "googleAuthR.httr_oauth_cache")
  scopes     <- is.different(token$params$scope, "googleAuthR.scopes.selected")

  if(!is.null(token$app)){
    client_id     <- is.different(token$app$key, "googleAuthR.client_id")
    client_secret <- is.different(token$app$secret, "googleAuthR.client_secret")
    
  } else {
    myMessage("No client_id in token, authentication from JSON key file", level = 2)
  }
  
  ## FALSE if any are different, TRUE if they are not
  !any(cache_path, scopes, client_id, client_secret)
}

is.different <- function(token_element, option_name){
  if(!all(token_element %in% getOption(option_name))){
    myMessage(sprintf("Token %s != getOption('%s') \n#>Token: %s \n#>Option: %s\n", 
                    deparse(substitute(token_element)), 
                    option_name, 
                    paste(token_element, collapse = " "), 
                    paste(getOption(option_name), collapse = " ")
                    ), level = 2
            )
    
    return(TRUE)
  }
  FALSE
}

#' Check that token appears to be legitimate
#'
#' Catch tokens that are technically valid,
#' i.e. `inherits(token, "Token2.0")` is TRUE, but that have dysfunctional
#' credentials.
#'
#' @keywords internal
#' @family authentication functions
#' @noRd
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
    myMessage("Invalid client authorization error. Check client_id and client_secret.", level=3)
    
    return(FALSE)
  }
  
  if("invalid_request" %in% unlist(x$credentials)) {
    # known example: if user clicks "Cancel" instead of "Accept" when OAuth2
    # flow kicks to browser
    myMessage("Invalid request authorization error. Check request format.", level=3)
    return(FALSE)
  }
  
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
  
  token <- .auth$cred
  
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
  
  if(any(which(grepl("with_mock_API", 
                     as.character(sys.calls()), ignore.case = FALSE)))){
    myMessage("Skipping token checks as using with_mock_API", level = 1)
    return(TRUE)
  }
  
  if(!is.null(gar_cache_get_loc())){
    myMessage("Skipping token checks as using cache", level = 1)
    return(TRUE)
  }
  
  if(is.null(shiny_access_token)){
    ## local token
    token <- .auth$cred

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
    if(!(any(startsWith(tolower(ok_content_types), 
                        tolower(req$headers$`content-type`))))) {
      stop(sprintf(paste("Not expecting content-type to be:\n%s"),
                   req$headers[["content-type"]]), call. = FALSE)
      
    }
  } else {
    warning("No content-type returned.")
    return(FALSE)
  }
  
  ## get error message from API
  if (!is.null(ga.json$error$message)) {
    
    if(all(!is.null(ga.json$error$errors), 
           getOption("googleAuthR.verbose") < 3)){
      error_message <- paste(
        paste("#", ga.json$error$errors, collapse = "\n")
      )
    } else {
      error_message <- paste(ga.json$error$message)
    }
    
    stop("API returned: ", error_message, call. = FALSE)
  }
  
  stop_for_status(req)
  
  TRUE
}
