#' R6 environment to store authentication credentials
#' 
#' Used to keep persistent state.
#' @export
Authentication <- R6::R6Class(
  "Authentication",
  public = list(
    websites = "initial",
    token = NULL,
    shiny = FALSE,
    app_url = NULL
  ),
  lock_objects = F,
  parent_env = emptyenv()
)

#' Authorize \code{googleAuthR}
#'
#' Authorize \code{googleAuthR} to access your Google user data. You will be
#' directed to a web browser, asked to sign in to your Google account, and to
#' grant \code{googleAuthR} access to user data for Google Search Console. 
#' These user credentials are cached in a file named
#' \code{.httr-oauth} in the current working directory, from where they can be
#' automatically refreshed, as necessary.
#'
#'
#' These arguments are controlled via options, which, 
#' if undefined at the time \code{googleAuthR} is loaded, are
#' defined like so:
#'
#' \describe{
#'   \item{key}{Set to option \code{googleAuthR.client_id}, which defaults to
#'   a client ID that ships with the package}
#'   \item{secret}{Set to option \code{googleAuthR.client_secret}, which
#'   defaults to a client secret that ships with the package}
#'   \item{cache}{Set to option \code{googleAuthR.httr_oauth_cache}, which
#'   defaults to TRUE}
#'   \item{scopes}{Set to option \code{googleAuthR.scopes.selected}, which
#'   defaults to demo scopes.}
#' }
#'
#' To override these defaults in persistent way, predefine one or more of
#' them with lines like this in a \code{.Rprofile} file:
#' \preformatted{
#' options(googleAuthR.client_id = "FOO",
#'         googleAuthR.client_secret = "BAR",
#'         googleAuthR.httr_oauth_cache = FALSE)
#' }
#' See \code{\link[base]{Startup}} for possible locations for this file and the
#' implications thereof.
#'
#' More detail is available from
#' \href{https://developers.google.com/identity/protocols/OAuth2}{Using OAuth
#' 2.0 to Access Google APIs}. This function executes the "installed
#' application" flow.
#'
#' @param token an actual token object or the path to a valid token stored as an
#'   \code{.rds} file
#' @param new_user logical, defaults to \code{FALSE}. Set to \code{TRUE} if you
#'   want to wipe the slate clean and re-authenticate with the same or different
#'   Google account. This deletes the \code{.httr-oauth} file in current working
#'   directory.
#'   
#'
#' @return an OAuth token object, specifically a
#'   \code{\link[=Token-class]{Token2.0}}, invisibly
#'
#' @export
#' @family authentication functions
gar_auth <- function(token = NULL,
                     new_user = FALSE) {
  
  if(new_user) {
    Authentication$set("public", "token", NULL, overwrite=TRUE)
    Authentication$set("public", "websites", data.frame(siteURL="None", permissionLevel="N/A"), overwrite=TRUE)
    if(file.exists(".httr-oauth")){
      myMessage("Removing old credentials ...", level=2)
      file.remove(".httr-oauth")     
    }
    
  }
  
  if(is.null(token)) {
    
    endpoint <- httr::oauth_endpoints("google")
    
    app <- httr::oauth_app("google", 
                           key = getOption("googleAuthR.client_id"), 
                           secret = getOption("googleAuthR.client_secret"))
    
    google_token <- httr::oauth2.0_token(endpoint = endpoint, 
                                         app = app,
                                         scope = getOption("googleAuthR.scopes.selected"), 
                                         cache = getOption("googleAuthR.httr_oauth_cache"))   

    
    
    stopifnot(is_legit_token(google_token))
    
    Authentication$set("public", "token", google_token, overwrite=TRUE)
    
  } else {
    
    if(is_legit_token(token)) {
      google_token <- token
    } else {
      google_token <- try(suppressWarnings(readRDS(token)), silent = TRUE)
      if(is.error(google_token)) {
        myMessage(sprintf("Cannot read token from alleged .rds file:\n%s",
                          token), level=3)
        return(invisible(NULL))
      } else if(!is_legit_token(google_token)) {
          
        myMessage(sprintf("File does not contain a proper token:\n%s", token), level=3)
        return(invisible(NULL))
      }
    }
    
    Authentication$set("public", "token", google_token, overwrite=TRUE)
    
  }
  
  return(invisible(Authentication$public_fields$token)) 
  
}

#' Retrieve Google token from environment and configs for httr
#'
#' Get token if it's previously stored, else prompt user to get one.
#' @param shiny_return_token In a shiny session, this is passed instead.
#' @return a httr configured option for token
#' For shiny the token is passed from reactive session
#'
#' @keywords internal
#' @family authentication functions
get_google_token <- function(shiny_return_token=NULL) {
  
  if(is.null(shiny_return_token)){
    token <- Authentication$public_fields$token
    
    if(is.null(token) || !is_legit_token(token)) {
      gar_auth()
    }
    
    
  } else { #shiny session
    
    token <- shiny_return_token
    
  }
  
  httr::config(token = token)
  
}



#' Check if authorization currently in force
#'
#' @return logical
#'
#' @keywords internal
#' @family authentication functions
token_exists <- function() {
  
  token <- Authentication$public_fields$token
  
  if(is.null(token)) {
    
    myMessage("No authorization yet in this session!", level=3)
    
    if(file.exists(".httr-oauth")) {
      myMessage(paste("NOTE: a .httr-oauth file exists in current working",
                    "directory.\n Run gar_auth() to use the",
                    "credentials cached in .httr-oauth for this session."), level=3)
    } else {
      myMessage(paste("No .httr-oauth file exists in current working directory.",
                    "Run scr_auth() to provide credentials."), level=3)
    }
    
    myMessage("Token doesn't exist", level=3)
    FALSE
  } else {
    myMessage("Token exists.", level=2)
    TRUE      
  }
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
    myMessage("Not a Token2.0 object. Found:", class(x), level=3)
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
  
  TRUE
}

#' JSON service account authentication
#' 
#' @description As well as OAuth2 authentication, 
#'   you can authenticate without user interaction via Service accounts.  
#'   This involves downloading a secret JSON key with the authentication
#'   details.
#'   
#'   To use, go to your Project in 
#'     the https://console.developers.google.com/apis/credentials/serviceaccountkey
#'     
#'     and select JSON Key type.  Save the file 
#'   to your computer and call it via supplying 
#'   the file path to the \code{json_file} parameter.
#'   
#'   Navigate to it via: 
#'     Google Dev Console > Credentials > New credentials > Service account Key > 
#'        Select service account > Key type = JSON
#' 
#' @param json_file the JSON file downloaded from Google Developer Console
#' @param scope Scope of the JSON file auth if needed
#' 
#' @seealso https://developers.google.com/identity/protocols/OAuth2ServiceAccount
#' 
#' @return (Invisible) Sets authentication token
#' 
#' @seealso 
#' https://developers.google.com/identity/protocols/OAuth2ServiceAccount
#' 
#' @export
#' @family authentication functions
gar_auth_service <- function(json_file, scope = getOption("googleAuthR.scopes.selected")){
  
  
  endpoint <- httr::oauth_endpoints("google")
  
  secrets  <- jsonlite::fromJSON(json_file)
  scope <- paste(scope, collapse=" ")
  
  google_token <- httr::oauth_service_token(endpoint, secrets, scope)
  
  Authentication$set("public", "token", google_token, overwrite=TRUE)
  
  myMessage("Returning service token", level=1)
  
  return(invisible(Authentication$public_fields$token))
  
}

