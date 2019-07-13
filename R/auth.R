#' Environment to store authentication credentials
#' 
#' Used to keep persistent state.
#' @noRd
.auth <- gargle::init_AuthState(
  package = "googleAuthR",
  auth_active = TRUE,
  app = NULL,
  api_key = NULL,
  cred = NULL
)


# REMOVE?
#' R6 environment to store authentication credentials
#' Environment to store authentication credentials
#' 
#' Used to keep persistent state.
#' @export
Authentication <- R6::R6Class(
  "Authentication",
  public = list(
    token = NULL,
    method = NULL
  ),
  lock_objects = FALSE,
  parent_env = emptyenv()
)

#' Authorize \code{googleAuthR}
#' 
#' Wrapper of \link[gargle]{token_fetch}
#'
#' @param token an actual token object or the path to a valid token stored as an
#'   \code{.rds} file
#' @param email An existing gargle cached email to authenticate with or TRUE to authenticate with the only email available.
#' @param scopes Scope of the request
#' @param new_user Not used   
#' @param package The name of the package authenticating
#'
#' @return an OAuth token object, specifically a
#'   \code{\link[=Token-class]{Token2.0}}, invisibly
#'
#' @export
#' @family authentication functions
#' @importFrom gargle token_fetch oauth_app_from_json
#' @importFrom httr oauth_app
#' @import assertthat
gar_auth <- function(token = NULL,
                     email = NULL,
                     scopes = getOption("googleAuthR.scopes.selected"),
                     new_user = NULL,
                     package = "googleAuthR") {
  
  # to aid non-interactive scripts
  if(is.null(email) 
     && !interactive() 
     && is.null(getOption("gargle_oauth_email"))){
    options(gargle_oauth_email=TRUE)
  }
  
  if(!is.null(new_user)){
    warning("Argument 'new_user' is not used anymore, remove it from ", 
            paste(sys.call(-1), collapse = " "))
  }
  
  if(!is.null(token)){
    Authentication$set("public", "method", "passed_token", overwrite=TRUE)
  }
  
  # file locations to read existing httr tokens
  if(is.string(token) && is.readable(token)){
    token <- read_cache_token(token)
    Authentication$set("public", "method", "filepath", overwrite=TRUE)
  }
  
  token <- token_fetch(
    email = email,
    token = token,
    scopes = scopes,
    app = make_app(),
    package = package
  )
  
  ## set the global session token
  Authentication$set("public", "token", token, overwrite=TRUE)
  
  invisible(token)
  
}

make_app <- function(){
  # set GAR_WEB_CLIENT_JSON in Shiny functions?
  if(Sys.getenv("GAR_CLIENT_JSON") != ""){
    app <- oauth_app_from_json(Sys.getenv("GAR_CLIENT_JSON"),
                               appname = "google")
  } else if(
    all(getOption("googleAuthR.client_id") != "",
        getOption("googleAuthR.client_secret") != "")){
    app <- oauth_app(
      "google",
      key = getOption("googleAuthR.client_id"),
      secret = getOption("googleAuthR.client_secret")
    )
  } else {
    stop("No oauth app could be created.  
         Set via GAR_CLIENT_JSON environment argument 
         or via gar_set_client()", call. = FALSE)
  }
  
  app
}

#' Reads a token from a filepath
#' 
#' Also sets the option of token cache name to the supplied filepath 
#'   "googleAuthR.httr_oauth_cache"
#' 
#' httr cache files such as .httr-oauth can hold multiple tokens for different scopes, 
#'   this only returns the first one and raises a warning if there are multiple 
#'   in the rds file
#' @noRd
#' @import assertthat
read_cache_token <- function(token_path){
  
  assert_that(is.readable(token_path))
  
  myMessage("Reading token from file path", level = 2)
  
  google_token <- tryCatch({readRDS(token_path)},
                           error = function(ex){
                             stop(sprintf("Cannot read token from alleged .rds file:\n%s ",
                                          token_path), 
                                  ex$message, 
                                  call. = FALSE)
                           })
  
  if(is.list(google_token)){
    myMessage("Multiple httr-tokens in cache ",
              token_path, ", only returning first found token", level = 2)
    google_token <- google_token[[1]]
  } else if(is.token2.0(google_token)){
    myMessage("Read token successfully from file", level = 2)
  } else {
    stop("Unknown object read from ", token_path, " of class ", class(google_token))
  }
  
  ## for existing tokens, set the options to what is in the token
  google_token <- overwrite_options(google_token, token_path = token_path)
  

  ## set the global session token
  Authentication$set("public", "token", google_token, overwrite=TRUE)
  
  google_token
}

overwrite_options <- function(google_token, token_path){
  options("googleAuthR.httr_oauth_cache" = token_path)
  google_token$cache_path <- token_path
  
  if(is.different(google_token$params$scope, "googleAuthR.scopes.selected")){
    myMessage("Overwriting googleAuthR.scopes.selected from ", getOption("googleAuthR.scopes.selected"),
              "to ", paste(google_token$params$scope, collapse = " "), level = 2)
    options("googleAuthR.scopes.selected" = google_token$params$scope)
  }
  
  if(is.null(google_token$app)){
    myMessage("No client_id in token, authentication from JSON key file", level = 2)
    return(google_token)
  }
  
  if(is.different(google_token$app$key, "googleAuthR.client_id")){
    myMessage("Overwriting googleAuthR.client_id from", getOption("googleAuthR.client_id"),
              "to ", google_token$app$key, level = 2)
    options("googleAuthR.client_id" = google_token$app$key)
  }
  
  if(is.different(google_token$app$secret, "googleAuthR.client_secret")){
    myMessage("Overwriting googleAuthR.client_secret to ", google_token$app$secret, level = 2)
    options("googleAuthR.client_secret" = google_token$app$secret)
  }
  
  google_token
  
}


#' Get current token summary
#' 
#' Get details on the current active auth token to help debug issues
#' 
#' @param detail_level How much info to show
#' 
#' @export
#' @importFrom gargle gargle_oauth_sitrep
gar_token_info <- function(detail_level = getOption("googleAuthR.verbose", default = 3)){
  token  <- Authentication$public_fields$token
  method <- Authentication$public_fields$method
 
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
    
    message("Method: ", method)
    
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
  
  if(is.null(shiny_return_token)){
    token <- Authentication$public_fields$token
    
    if(is.null(token) || !is_legit_token(token)) {
      token <- gar_auth()
    }
    
    
  } else { #shiny session
    Authentication$set("public", "method", "shiny", overwrite=TRUE)
    token <- shiny_return_token
    
  }
  
  config(token = token)
  
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
#' @importFrom httr oauth_endpoints oauth_service_token
#' @importFrom jsonlite fromJSON
#' @importFrom gargle credentials_service_account
gar_auth_service <- function(json_file, 
                             scope = getOption("googleAuthR.scopes.selected")){
  
  stopifnot(file.exists(json_file))
  
  secrets  <- fromJSON(json_file)
  scope <- paste(scope, collapse=" ")
  
  if(is.null(secrets$private_key)){
    stop("$private_key not found in JSON - have you downloaded the correct JSON file? 
         (Service Account Keys, not service account client)")
  }
  
  google_token <- credentials_service_account(
    scopes = scope,
    path = json_file
  )
  
  Authentication$set("public", "token", google_token, overwrite=TRUE)
  Authentication$set("public", "method", "service_json", overwrite=TRUE)
  
  return(invisible(Authentication$public_fields$token))
  
}


