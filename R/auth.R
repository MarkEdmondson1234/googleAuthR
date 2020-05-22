#' Authorize \code{googleAuthR}
#' 
#' Wrapper of \link[gargle]{token_fetch}
#'
#' @param token an actual token object or the path to a valid token stored as an
#'   \code{.rds} file
#' @param email An existing gargle cached email to authenticate with or TRUE to authenticate with the only email available.
#' @param scopes Scope of the request
#' @param app app as specified by \link{gar_auth_configure}
#' @param package The name of the package authenticating
#' @param cache Where to store authentication tokens
#' @param use_oob Whther to use OOB browserless authetication
#'
#' @return an OAuth token object, specifically a
#'   \code{\link[=Token-class]{Token2.0}}, invisibly
#'   
#' @examples 
#' 
#' \dontrun{
#' 
#' # sets GCP project to auth through
#' gar_auth_configure(path="path/to/gcp-client.json")
#' 
#' # starts auth process with defaults
#' gar_auth()
#' 
#' # switching between auth scopes
#' # first time new scope manual auth, then auto if supplied email   
#' gar_auth(email = "your@email.com", 
#'          scopes = "https://www.googleapis.com/auth/drive")
#'          
#' # ... query Google Drive functions ...
#' 
#' gar_auth(email = "your@email.com", 
#'          scopes = "https://www.googleapis.com/auth/bigquery")
#'          
#' # ..query BigQuery functions ...
#' 
#' }
#'
#' @export
#' @family authentication functions
#' @importFrom gargle token_fetch oauth_app_from_json
#' @importFrom httr oauth_app
#' @import assertthat
gar_auth <- function(token = NULL,
                     email = NULL,
                     scopes = getOption("googleAuthR.scopes.selected"),
                     app = gar_oauth_app(),
                     cache = gargle::gargle_oauth_cache(),
                     use_oob = gargle::gargle_oob_default(),
                     package = "googleAuthR") {
  
  # file locations to read existing httr tokens (legacy compatibility)
  if(is.string(token) && is.readable(token)){
    token <- read_cache_token(token)
    #`scopes` cannot be specified when user brings their own OAuth token; 
    #`scopes` are already implicit in the token
    scopes <- NULL 
  }
  
  # set scopes to options for backward compatibility  
  if(!is.null(scopes) && scopes != ""){
    options(googleAuthR.scopes.selected = scopes)
  }
  
  # to aid non-interactive scripts
  if(is.null(token)
     && is.null(email) 
     && !interactive() 
     && is.null(getOption("gargle_oauth_email"))){
    stop("Non-interactive session and no authentication email selected.
         \nSetup JSON service email auth or specify email in gar_auth(email='me@preauthenticated.com')", 
         call.=FALSE)
  }
  
  if(is.null(app)){
    # sets return value for gar_oauth_app()
    make_app()
  }
  
  token <- token_fetch(
    email = email,
    token = token,
    scopes = scopes,
    app = gar_oauth_app(),
    package = package,
    cache = cache,
    use_oob = use_oob
  )
  
  if(!is.token2.0(token)){
    stop("Could not authenticate via any gargle cred function", call. = FALSE)
  }
  
  .auth$set_cred(token)
  .auth$set_auth_active(TRUE)
  
  invisible(token)
  
}

# only called if new gar_auth_configure() not used
make_app <- function(){
  
  # legacy old environment argument to json path
  if(Sys.getenv("GAR_CLIENT_JSON") != ""){
    myMessage("Setting client.id from GAR_CLIENT_JSON", level = 3)
    return(gar_auth_configure(path = Sys.getenv("GAR_CLIENT_JSON")))
  }
  # TODO: doesn't cover GAR_WEB_CLIENT_JSON in Shiny functions?
  
  # legacy set via gar_set_client() or options() directly
  if(!all(getOption("googleAuthR.client_id") != "",
          getOption("googleAuthR.client_secret") != "")){
    stop("No oauth app could be created.  
         Set via gar_auth_configure()", call. = FALSE) 
  }

  myMessage("Setting client.id from options(googleAuthR.client_id)", level = 3)
  app <- oauth_app(
      appname = paste0("googleAuthR.client_id=", 
                       substring(getOption("googleAuthR.client_id"),1,13)),
      key = getOption("googleAuthR.client_id"),
      secret = getOption("googleAuthR.client_secret")
  )
  
  gar_auth_configure(app = app)

}



#' Get current token summary
#' 
#' Get details on the current active auth token to help debug issues
#' 
#' @param detail_level How much info to show
#' 
#' @export
#' @importFrom gargle gargle_oauth_sitrep
gar_token_info <- function(detail_level = getOption("googleAuthR.verbose", 
                                                    default = 3)){
  token  <- .auth$cred

  if(is.null(token)){
    message("No token found")
    return(NULL)
  }
  
  gargle_oauth_sitrep()
  
  if(detail_level >= 3){
    message("Authentication from cache file: ", token$cache_path)

    ## service
    if(!is.null(token$secrets)){
      message("Type: ", token$secrets$type)
      message("ProjectID: ", token$secrets$project_id)
      message("Client email: ", token$secrets$client_email)
      message("ClientID: ", token$secrets$client_id)
    }
    
  }
  
  if(detail_level <= 2){
    message("Scopes: ", paste(token$params$scope, collapse = " "))
    if(!is.null(token$app$key)){
      message("App key: ", token$app$key)
    }
    
  }
  
  if(detail_level == 1){
    message("Hash: ", token$hash())
  }


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
#' @importFrom httr config
get_google_token <- function(shiny_return_token=NULL) {
  
  if(any(which(grepl("with_mock_API", as.character(sys.calls()))))){
    myMessage("Skipping token checks as using with_mock_API", level = 3)
    return(NULL)
  }
  
  # shiny auth provides its own token
  if(!is.null(shiny_return_token)){
    return(config(token = shiny_return_token))
  }
  
  gar_token()
  
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
#' @importFrom jsonlite fromJSON
#' @importFrom gargle credentials_service_account
gar_auth_service <- function(json_file, 
                             scope = getOption("googleAuthR.scopes.selected")){

  secrets  <- fromJSON(json_file)
  scope <- paste(scope, collapse=" ")
  
  if(is.null(secrets$private_key)){
    stop("$private_key not found in JSON - have you downloaded the correct JSON file? 
         (Service Account Keys, not service account client)")
  }
  
  token <- credentials_service_account(
    scopes = scope,
    path = json_file
  )
  
  if(!is.tokenservice(token)){
    stop("Invalid TokenServiceAccount", call. = FALSE)
  }
  
  .auth$set_cred(token)
  .auth$set_auth_active(TRUE)
  
  invisible(token)
  
}

is.tokenservice <- function(x){
  inherits(x, "TokenServiceAccount")
}

