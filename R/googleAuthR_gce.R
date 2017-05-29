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
  
  email_content <- httr::content(req, as = "text", encoding = "UTF-8")
  strsplit(email_content, "\n")[[1]][2]
}