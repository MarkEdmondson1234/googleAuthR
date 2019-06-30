#' Authenticate via gcloud's application-default login
#' 
#' This allows you to take gcloud's application-default login token and turns it into one that can be used by R
#' 
#' @param access_token The token generated via \code{gcloud auth application-default login && gcloud auth application-default print-access-token}
#' @param scopes The scope you created the access_token with
#' @param cache_file Where to save the cache file of the token
#' 
#' @details 
#' 
#'  When authenticating on Google Cloud Platform services, if you are using services that take the cloud scopes you can use \link{gar_gce_auth} to generate authentication.
#'  
#'  However, for other services that require a user login (such as Google Analytics API), you need a method of authentication where you can use your own email login.  You have two options - create a token offline and upload it to the instance, or \code{gcloud} allows you to generate your own token online via \code{gcloud auth application-default login && gcloud auth application-default print-access-token}
#'  
#'  This function will then take the returned access token and put it within R so it can be used as normal with \code{googleAuthR} functions.
#'  
#' @examples 
#' 
#' \dontrun{
#' 
#' ## in the terminal, issue this gcloud command specifying the scopes to authenticate with
#' gcloud auth application-default login \
#'   --scopes=https://www.googleapis.com/auth/analytics.readonly
#' 
#' ## access the URL, login and create a verification code, paste in console.
#' 
#' ## view then copy-paste the access token, to be passed into the R function
#' gcloud auth application-default print-access-token
#' 
#' ## In R:
#' gar_gce_auth_default(<token-copy-pasted>, 
#'      scopes = 'https://www.googleapis.com/auth/analytics.readonly',
#'      cache_file = 'my_ga.auth')
#'                      
#' # use token to authenticate as you would normally with library
#' }
#' 
#' 
#' @export
#' @importFrom httr Token2.0 oauth_app oauth_endpoints
#' @importFrom jsonlite fromJSON
#' @seealso \href{https://cloud.google.com/sdk/gcloud/reference/auth/application-default/print-access-token}{gcloud reference}
gar_gce_auth_default <- function(access_token, 
                                 scopes,
                                 cache_file = "gcloud.auth"){
  
  json_creds <- fromJSON('~/.config/gcloud/application_default_credentials.json')
  
  token_formatted <-
    Token2.0$new(app = oauth_app("google", 
                                 key = json_creds$client_id, 
                                 secret = json_creds$client_secret),
                       endpoint = oauth_endpoints("google"),
                       credentials = list(access_token = access_token,
                                          token_type = json_creds$type,
                                          expires_in = NULL,
                                          refresh_token = NULL),
                       params = list(scope = scopes, type = NULL,
                                     use_oob = FALSE, as_header = TRUE),
                       cache_path = FALSE)
  
  saveRDS(token_formatted, cache_file)
  
  myMessage("Authenticated. Token saved to ", cache_file, level = 3)
  
  token_formatted
  
}



#' Authenticate on Google Compute Engine
#' 
#' This takes the metadata auth token in a Google Compute Engine instance as authentication source
#' 
#' @param service_account Specify a different service account from the \code{default}
#' @param client.id The Google Project API console's client Id
#' @param client.secret The Google Project API console's client secret
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
#' @seealso \link{gar_gce_auth_email}
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

#' Get the service email via GCE metadata
#' 
#' @param service_account Specify a different service account from the \code{default}
#' 
#' Useful if you don't know the default email and need it for other uses
#' 
#' @return the email address character string
#' @seealso \link{gar_gce_auth}
#' @export
gar_gce_auth_email <- function(service_account = "default"){
  
  call_url <- sprintf("http://metadata.google.internal/computeMetadata/v1/instance/service-accounts/", 
                      service_account)
  req <- try(httr::GET(call_url, httr::add_headers(`Metadata-Flavor` = "Google")), 
             silent = TRUE)
  if (is.error(req)) {
    myMessage("Not detected as being on Google Compute Engine", 
              level = 2)
    return(NULL)
  }
  
  httr::content(req, as = "text", encoding = "UTF-8")

}