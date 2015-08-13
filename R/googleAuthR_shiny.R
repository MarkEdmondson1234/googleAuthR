#' Creates a random character code
#' 
#' @param seed random seed.
#' @param num number of characters the code should be.
#' 
#' @return a string of random digits and letters.
#' @family shiny auth functions
#' @keywords internal
createCode <- function(seed=NULL, num=20){
  if (!is.null(seed)) set.seed(seed)
  
  paste0(sample(c(1:9, LETTERS, letters), num, replace = T), collapse='')
}

#' Returns the authentication parameter "code" in redirected URLs
#' 
#' Checks the URL of the Shiny app to get the state and code URL parameters.
#' 
#' @param session A shiny session object
#' @param securityCode A random string to check the auth comes form the same origin.
#' 
#' @return The Google auth token in the code URL parameter.
#' @family shiny auth functions
#' @keywords internal
#' @export
authReturnCode <- function(session, 
                           securityCode=getOption("googleAuthR.securitycode")){
  
  pars <- shiny::parseQueryString(session$clientData$url_search)
  
  if(!is.null(pars$state)){
    if(pars$state != securityCode){
      warning("securityCode check failed in Authentication! Code:", 
              pars$state, 
              " Expected:", 
              securityCode)
      return(NULL)
    } 
  }
  
  if(!is.null(pars$code)){
    return(pars$code)
  } else {
    NULL
  }
}





#' Returns the Google authentication URL
#' 
#' The URL a user authenticates the Shiny app on.
#' 
#' @param redirect.uri App URL, from \code{gar_shiny_getUrl}
#' @param state A random string used to check auth is from same origin.
#' @param client.id From the Google API console.
#' @param client.secret From the Google API console.
#' @param scope What Google API service to get authentication for.
#' 
#' @return The URL for authentication.
#' 
#' @family shiny auth functions
#' @export
gar_shiny_getAuthUrl <- 
  function(redirect.uri = gar_shiny_getUrl(session),
           state = getOption("googleAuthR.securitycode"),
           client.id     = getOption("googleAuthR.webapp.client_id"),
           client.secret = getOption("googleAuthR.webapp.client_secret"),
           scope         = getOption("googleAuthR.scope")) {
    
    ## httr friendly version
    
    url <- httr::modify_url(
      httr::oauth_endpoints("google")$authorize,
      query = list(response_type = "code",
                   client_id = client.id,
                   redirect_uri = redirect.uri,
                   scope = scope,
                   state = state,
                   access_type = "online",
                   approval_prompt = "auto"))
    message("Auth Token URL: ", url)
    url
  }


#' Get the Shiny Apps URL.
#' 
#' Needed for the redirect URL in Google Auth flow
#' 
#' @param session The shiny session object.
#' 
#' @return The URL of the Shiny App its called from.
#' 
#' @family shiny auth functions
#' @export
gar_shiny_getUrl <- function(session){
  
  if(!is.null(session)){
    pathname <- session$clientData$url_pathname
    ## hack for shinyapps.io
    if(session$clientData$url_hostname == "internal.shinyapps.io"){
      split_hostname <- strsplit(pathname, "/")[[1]]
      hostname <-  paste(split_hostname[2],"shinyapps.io", sep=".")
      pathname <- paste0("/",split_hostname[3],"/")
      
    } else {
      hostname <- session$clientData$url_hostname
    }
    
    url <- paste0(session$clientData$url_protocol,
                  "//",
                  hostname,
                  ifelse(hostname == "127.0.0.1",
                         ":",
                         pathname),
                  session$clientData$url_port)
    message("Shiny URL detected as: ", url)
    url
  } else {
    NULL
  }
  
  
}

#' Does the Shiny authentication flow
#' 
#' Used in a shiny session instead of \code{scr_auth}
#' 
#' @param return_code the code in the url from \code{authReturnCode}
#' @param session A Shiny session object passed from shinyServer().
#' 
#' @return A Google OAuth2 token
#' @family shiny auth functions
#' @export
# gar_shiny_getUrl
get_google_token_shiny <- 
  function(return_code, session=NULL){
    
    if(!is.null(session)){
      app_url <- gar_shiny_getUrl(session)
      Authentication$set("public", "app_url", app_url, overwrite=TRUE)
    } else {
      app_url <- Authentication$public_fields$app_url
    }
    
    gar_shiny_getToken(return_code, app_url)
    
  }



#' Returns the authentication Token.
#' 
#' Once a user browses to ShinyGetTokenURL and is redirected back with request
#' gar_shiny_getToken takes that code and returns a token needed for Google APIs
#' Uses the same client.id and client.secret as ShinyGetTokenURL.
#' 
#' @param code The code returned from a successful Google authentication.
#' @param redirect.uri Where a user will go after authentication.
#' @param client.id From the Google API console.
#' @param client.secret From the Google API console.
#' 
#' @return A list including the token needed for Google API requests.
#' 
#' @keywords internal
#' @family shiny auth functions

gar_shiny_getToken <- function(code,
                            redirect.uri  = gar_shiny_getUrl(session),
                            client.id     = getOption("googleAuthR.webapp.client_id"),
                            client.secret = getOption("googleAuthR.webapp.client_secret")){
  
  scr_app <- httr::oauth_app("google", key = client.id, secret = client.secret)
  
  scope_list <- getOption("googleAuthR.scope")
  
  req <-
    httr::POST("https://accounts.google.com/o/oauth2/token",
               body = list(code = code,
                           client_id = client.id,
                           client_secret = client.secret,
                           redirect_uri = redirect.uri,
                           grant_type = "authorization_code"))
  
  stopifnot(identical(httr::headers(req)$`content-type`,
                      "application/json; charset=utf-8"))
  # content of req will contain access_token, token_type, expires_in
  token <- httr::content(req, type = "application/json")
  
  # Create a Token2.0 object consistent with the token obtained from scr_auth()
  token_formatted <-
    httr::Token2.0$new(app = scr_app,
                       endpoint = httr::oauth_endpoints("google"),
                       credentials = list(access_token = token$access_token,
                                          token_type = token$token_type,
                                          expires_in = token$expires_in,
                                          refresh_token = token$refresh_token),
                       params = list(scope = scope_list, type = NULL,
                                     use_oob = FALSE, as_header = TRUE),
                       cache_path = getOption("googleAuthR.httr_oauth_cache"))
  
  token_formatted
}

