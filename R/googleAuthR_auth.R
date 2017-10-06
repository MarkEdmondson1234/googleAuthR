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
#' @import assertthat
gar_auth <- function(token = NULL,
                     new_user = FALSE) {
  
  ## Up to 0.4.0 this coud be TRUE, FALSE or a character file location.  Now only file location allowed. 
  httr_file <- getOption("googleAuthR.httr_oauth_cache")
  
  if(is.flag(httr_file)){
    stop("option('googleAuthR.httr_oauth_cache') must be set to valid cache file location, 
         not TRUE or FALSE - (example: '.httr-oauth')", 
         call. = FALSE)
  }
  
  ## we only support writing auth cache files
  assert_that(is.string(httr_file),
              is.flag(new_user))
  
  if(new_user) {
    rm_old_user_cache(httr_file)
  }
  
  if(is.null(token)) {     ## supplied no token
    
    google_token <- make_new_token()
    
  } else if(is_legit_token(token)){     ## supplied a Token object
    
    Authentication$set("public", "method", "passed_token", overwrite=TRUE)
    
    ## just return it back
    google_token <- token

  } else if(is.string(token)){ ## a filepath
    
    google_token <- read_cache_token(token_path = token)

  } else {
    stop("Unrecognised token object - class ", class(token), call. = FALSE)
  }
  
  ## set the global session token
  Authentication$set("public", "token", google_token, overwrite=TRUE)
  
  ## output info on token saved
  gar_token_info()
  
  ## return from the R6 object just to check it went ok, but this is google_token above
  return(invisible(google_token)) 
  
}

#' @noRd
rm_old_user_cache <- function(httr_file){
  Authentication$set("public", "token", NULL, overwrite=TRUE)
  if(file.exists(httr_file)){
    myMessage("Removing old cached credentials from: ", normalizePath(httr_file), level = 3)
    file.remove(httr_file)     
  }
}

#' @noRd
#' @importFrom httr oauth_endpoints oauth_app oauth2.0_token
make_new_token <- function(){

  
  endpoint <- oauth_endpoints("google")
  
  key    <- getOption("googleAuthR.client_id")
  secret <- getOption("googleAuthR.client_secret")
  scope  <- getOption("googleAuthR.scopes.selected")
  cache  <- getOption("googleAuthR.httr_oauth_cache")
  
  if(key == ""){
    stop("option('googleAuthR.client_id') has not been set", call. = FALSE)
  }
  
  if(secret == ""){
    stop("option('googleAuthR.client_secret') has not been set", call. = FALSE)
  }
  
  if(scope == ""){
    stop("option('googleAuthR.scopes.selected') has not been set", call. = FALSE)
  }
  
  if(cache == ""){
    stop("option('googleAuthR.httr_oauth_cache') has not been set", call. = FALSE)
  }
  
  app <- oauth_app("google", 
                   key = key, 
                   secret = secret)
  
  google_token <- oauth2.0_token(endpoint = endpoint, 
                                 app = app,
                                 scope = scope, 
                                 cache = cache)   
  

  
  stopifnot(is_legit_token(google_token))
  
  ## set globals
  Authentication$set("public", "token", google_token, overwrite=TRUE)
  Authentication$set("public", "method", "new_token", overwrite=TRUE)
  
  google_token
}

#' Get current token summary
#' 
#' Get details on the current active auth token to help debug issues
#' 
#' @param detail_level How much info to show
#' 
#' @export
gar_token_info <- function(detail_level = getOption("googleAuthR.verbose", default = 3)){
  token <- Authentication$public_fields$token
  method <- Authentication$public_fields$method
  
  if(is.null(token)){
    myMessage("No token found", level = 3)
    return(NULL)
  }
  if(detail_level >= 3){
    myMessage("Token cache file: ", token$cache_path, level = 3)

    ## service
    if(!is.null(token$secrets)){
      myMessage("Type: ", token$secrets$type, level = 3)
      myMessage("ProjectID: ", token$secrets$project_id, level = 3)
      myMessage("Client email: ", token$secrets$client_email, level = 3)
      myMessage("ClientID: ", token$secrets$client_id, level = 3)
    }
    
  } else if(detail_level == 2){
    myMessage("Hash: ", token$hash(), level = 2)
    myMessage("Scopes: ", paste(token$params$scope, collapse = " "), level = 2)
    if(!is.null(token$app$key)){
      myMessage("App key: ", token$app$key, level = 2)
    }
    
    myMessage("Method: ", method, level = 2)
    
  } else if(detail_level == 1){
    NULL
  }


}

#' Reads a token form a filepath
#' 
#' Also sets the option of token cache name to the supplied filepath "googleAuthR.httr_oauth_cache"
#' 
#' httr cache files such as .httr-oauth can hold multiple tokens for different scopes, 
#'   this only returns the first one and raises a warning if there are multiple in the rds file
#' @noRd
#' @import assertthat
read_cache_token <- function(token_path){
  
  assert_that(is.readable(token_path))
  
  myMessage("Reading token from file path", level = 2)
  
  google_token <- tryCatch({suppressWarnings(readRDS(token_path))},
                           error = function(ex){
                             warning(sprintf("Cannot read token from alleged .rds file:\n%s",
                                             token_path))
                             stop(ex)
                           })
  
  if(is.list(google_token)){
    #warning("Multiple httr-tokens in cache ",token_path, ", only returning first found token")
    google_token <- google_token[[1]]
  } else if(is.token2.0(google_token)){
    myMessage("Read token successfully from file", level = 2)
  } else {
    stop("Unknown object read from ", token_path, " of class ", class(google_token))
  }
  
  options("googleAuthR.httr_oauth_cache" = token_path)
  google_token$cache_path <- token_path
  Authentication$set("public", "method", "filepath", overwrite=TRUE)
  
  google_token
}

is.token2.0 <- function(x){
  inherits(x, "Token2.0")
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
  
  if(any(which(grepl("with_mock_API", as.character(sys.calls()))))){
    myMessage("Skipping token checks as using with_mock_API", level = 3)
    return(NULL)
  }
  
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
