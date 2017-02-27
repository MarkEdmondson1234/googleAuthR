#' R6 environment to store authentication credentials
#' 
#' Used to keep persistent state.
#' @export
Authentication <- R6::R6Class(
  "Authentication",
  public = list(
    token = NULL,
    method = NULL
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
  
  httr_file <- getOption("googleAuthR.httr_oauth_cache")
  if(inherits(httr_file, "logical")){
    httr_file <- ".httr-oauth"
  }
  
  if(new_user) {
    Authentication$set("public", "token", NULL, overwrite=TRUE)
    if(file.exists(httr_file)){
      myMessage("Removing old credentials ... ", httr_file, level=2)
      file.remove(httr_file)     
    }
    
  }
  
  if(is.null(token)) {
    
    ## check if current cached token has same scopes as those set in options
    check_cached_scopes()
    
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
    Authentication$set("public", "method", "new_token", overwrite=TRUE)
    
  } else {
    ## supplied a file path or Token object
    if(is_legit_token(token)) {
      ## a token object, just return it back
      google_token <- token
      Authentication$set("public", "method", "passed_token", overwrite=TRUE)
    } else {
      ## a file path to a token
      options("googleAuthR.httr_oauth_cache" = token)
      google_token <- read_cache_token(token_path = token)
      google_token$cache_path <- token
      Authentication$set("public", "method", "filepath", overwrite=TRUE)
    }
    
    Authentication$set("public", "token", google_token, overwrite=TRUE)
    
  }
  gar_token_info()
  return(invisible(Authentication$public_fields$token)) 
  
}

#' Get current token summary
#' 
#' Get details on the current active auth token to help debug issues
#' 
#' @export
gar_token_info <- function(){
  token <- Authentication$public_fields$token
  method <- Authentication$public_fields$method
  
  myMessage("Token cache file: ", token$cache_path, level = 3)
  myMessage("Scopes: ", paste(token$params$scope, collapse = " "), level = 2)
  myMessage("Hash: ", token$hash(), level = 2)
  
  if(!is.null(token$app$key)){
    myMessage("App key: ", token$app$key, level = 2)
  }

  myMessage("Method: ", method, level = 2)
  
  ## service
  if(!is.null(token$secrets)){
    myMessage("Type: ", token$secrets$type, level = 2)
    myMessage("ProjectID: ", token$secrets$project_id, level = 2)
    myMessage("Client email: ", token$secrets$client_email, level = 2)
    myMessage("ClientID: ", token$secrets$client_id, level = 2)
  }
}

## httr cache files such as .httr-oauth can hold multiple tokens for different scopes.
## this selects the token that covers the scope given
read_cache_token <- function(token_path = getOption("googleAuthR.httr_oauth_cache")){
  if(inherits(token_path, "logical")){
    token_path <- ".httr-oauth"
  }
  myMessage("Reading token from file path", level = 2)
  google_token <- try(suppressWarnings(readRDS(token_path)), silent = TRUE)
  if(is.error(google_token)) {
    stop(sprintf("Cannot read token from alleged .rds file:\n%s",
                 token_path))
  }
  
  if(inherits(google_token, "list")){
    myMessage("Found a list, return token in first element.")
    google_token <- google_token[[1]]
  }
  
  google_token
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
    Authentication$set("public", "method", "shiny", overwrite=TRUE)
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
  
  httr_cache <- getOption("googleAuthR.httr_oauth_cache")
  if(class(httr_cache) == "logical"){
    httr_cache <- ".httr-oauth"
  }
  
  if(is.null(token)) {
    
    myMessage("No authorization yet in this session!", level=3)
    
    if(file.exists(httr_cache)) {
      myMessage(paste("NOTE: a ", httr_cache ,
                      " file exists in current working",
                      "directory.\n Run authentication function to use the",
                      "credentials cached for this session."), level=3)
    } else {
      myMessage(paste("No ", httr_cache ,
                      " file exists in current working directory.",
                      " Run gar_auth() to provide credentials."), level=3)
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
    myMessage("Not a Token2.0 object. Found:", class(x), level=2)
    if(!inherits(x, "list")){
      if(inherits(x[[1]], "Token2.0")){
        myMessage("Its a list of Token2.0 objects though")
        return(TRUE)
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
  
  stopifnot(file.exists(json_file))
  
  endpoint <- httr::oauth_endpoints("google")
  
  secrets  <- jsonlite::fromJSON(json_file)
  scope <- paste(scope, collapse=" ")
  
  if(is.null(secrets$private_key)){
    stop("$private_key not found in JSON - have you downloaded the correct JSON file? 
         (Service Account Keys, not service account client)")
  }
  
  google_token <- httr::oauth_service_token(endpoint, secrets, scope)
  
  Authentication$set("public", "token", google_token, overwrite=TRUE)
  Authentication$set("public", "method", "service_json", overwrite=TRUE)
  myMessage("Returning service token", level=1)
  
  return(invisible(Authentication$public_fields$token))
  
}

#' Authenticate on Google Compute Engine
#' 
#' This takes the metadata auth token in a Google Compute Engine instance as authentication source
#' 
#' @param service_account Specify a different service account from the \code{default}
#' @inheritParams gar_shiny_getToken
#' 
#' @details 
#' 
#' \code{service_account} is \code{default} or the service account email 
#' e.g. \code{"service-account-key-json@projectname.iam.gserviceaccount.com"}
#' 
#' Google Compute Engine instances come with their own authentication tokens.  
#' 
#' It has no refresh token so you need to call for a fresh token after approx. one hour. 
#' The metadata token will refresh itself when it has about 60 seconds left.
#' 
#' You can only use for scopes specified when creating the instance.
#' 
#' If you want to use them make sure their service account email is added to accounts you want to get data from.
#' 
#' If this function is called on a non-Google Compute Engine instance it will return \code{NULL}
#' 
#' @return A token 
#' @export
#' @family authentication functions
gar_gce_auth <- function(service_account = "default",
                         client.id     = getOption("googleAuthR.webapp.client_id"),
                         client.secret = getOption("googleAuthR.webapp.client_secret")){
  
  call_url <- sprintf("http://metadata.google.internal/computeMetadata/v1/instance/service-accounts/%s/token",
                      service_account)
  
  ## will be an error if not on GCE
  req <- try(httr::GET(call_url, httr::add_headers("Metadata-Flavor" = "Google")), silent = TRUE)
  
  if(is.error(req)){
    myMessage("Not detected as being on Google Compute Engine", level = 2)
    return(NULL)
  }

  token <- httr::content(req, type = "application/json")
  
  gar_app <- httr::oauth_app("google", key = client.id, secret = client.secret)
  
  scope_list <- getOption("googleAuthR.scope")
  
  token_formatted <-
    httr::Token2.0$new(app = gar_app,
                       endpoint = httr::oauth_endpoints("google"),
                       credentials = list(access_token = token$access_token,
                                          token_type = token$token_type,
                                          expires_in = token$expires_in,
                                          refresh_token = NULL),
                       params = list(scope = scope_list, type = NULL,
                                     use_oob = FALSE, as_header = TRUE),
                       cache_path = getOption("googleAuthR.httr_oauth_cache"))
  
  myMessage("Authenticated on Google Compute Engine", level = 2)
  
  ## for other google auth on a server (such as Google Analytics) need to manually do tokens via OOB
  options(httr_oob_default = TRUE)
  Authentication$set("public", "method", "gce_auth", overwrite=TRUE)
  
  ## puts it in environment
  gar_auth(token_formatted)

}


#' Check token scopes
## check if scopes are set correctly
check_cached_scopes <- function(){
  
  httr_cache <- getOption("googleAuthR.httr_oauth_cache")
  if(class(httr_cache) == "logical"){
    httr_cache <- ".httr-oauth"
  }
  google_token <- try(suppressWarnings(readRDS(httr_cache)[[1]]), silent = TRUE)

  out <- TRUE
  if(!is.error(google_token)){
    if(!all(google_token$params$scope %in%
            getOption("googleAuthR.scopes.selected"))){
      warning(paste0("option(googleAuthR.scopes.selected) not same scopes as current cached token ", httr_cache, ", will need reauthentication.  
                     \nToken scopes: ", 
                     paste(google_token$params$scope, collapse = " "),
                     "\ngetOption(googleAuthR.scopes.selected): ",
                     paste(getOption("googleAuthR.scopes.selected"), collapse = " "),
                     collapse = " "), call. = FALSE)
      out <- FALSE
    }
  }
  
  out
}
