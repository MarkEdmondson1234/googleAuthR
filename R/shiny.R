#' Creates a random character code
#' 
#' @param seed random seed.
#' @param num number of characters the code should be.
#' 
#' @return a string of random digits and letters.
#' @family shiny auth functions
#' @keywords internal
#' @noRd
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
#' @noRd
authReturnCode <- function(session, 
                           securityCode=getOption("googleAuthR.securitycode")){
  check_package_loaded("shiny")
  pars <- shiny::parseQueryString(session$clientData$url_search)
  
  # NULL if it isn't there
  has_auth_code(pars, securityCode = securityCode)
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
#' @keywords internal
#' @importFrom httr modify_url oauth_endpoints
#' @noRd
gar_shiny_getAuthUrl <- 
  function(redirect.uri,
           state = getOption("googleAuthR.securitycode"),
           client.id     = getOption("googleAuthR.webapp.client_id"),
           client.secret = getOption("googleAuthR.webapp.client_secret"),
           scope         = getOption("googleAuthR.scopes.selected"),
           access_type   = c("online","offline"),
           approval_prompt = c("auto","force")) {
    
    access_type <- match.arg(access_type)
    approval_prompt <- match.arg(approval_prompt)

    scopeEnc <- paste(scope, sep='', collapse=' ')
    
    ## httr friendly version
    url <- modify_url(
      oauth_endpoints("google")$authorize,
      query = list(response_type = "code",
                   client_id = client.id,
                   redirect_uri = redirect.uri,
                   scope = scopeEnc,
                   state = state,
                   access_type = access_type,
                   approval_prompt = approval_prompt))
    myMessage("Auth Token URL: ", url, level=2)
    url
}


#' Get the Shiny Apps URL.
#' 
#' Needed for the redirect URL in Google Auth flow
#' 
#' @param session The shiny session object.
#' 
#' @return The URL of the Shiny App its called from.
#' @family shiny auth functions
#' @noRd
gar_shiny_getUrl <- function(session){
  
  if(!is.null(session)){
    pathname <- shiny::isolate(session$clientData$url_pathname)
    hostname <- shiny::isolate(session$clientData$url_hostname)
    port <- shiny::isolate(session$clientData$url_port)
    
    if(hostname == "127.0.0.1"){
      hostname <- "localhost"
    }
    
    url <- paste0(shiny::isolate(session$clientData$url_protocol),
                  "//",
                  hostname,
                  if(port != "") paste0(":", port),
                  if(pathname != "/") pathname) 
    
    myMessage("Shiny URL detected as: ", url, level=1)
    url
  } else {
    NULL
  }
  
  
}

#' Returns the authentication Token.
#' 
#' Once a user browses to ShinyGetTokenURL and is redirected back with request
#' gar_shiny_getToken takes that code and returns a token needed for Google APIs
#' Uses the same client.id and client.secret as ShinyGetTokenURL.
#' 
#' @param code The code returned from a successful Google authentication.
#' @param redirect.uri Where a user will go after authentication, 
#'     from \code{gar_shiny_getUrl(session)}
#' @param client.id From the Google API console.
#' @param client.secret From the Google API console.
#' @keywords internal
#' @return A list including the token needed for Google API requests.
#' @family shiny auth functions
#' @importFrom httr oauth_app POST headers content Token2.0 oauth_endpoints
#' @noRd
gar_shiny_getToken <- function(code,
                               redirect.uri,
                               client.id     = getOption("googleAuthR.webapp.client_id"),
                               client.secret = getOption("googleAuthR.webapp.client_secret")){
  
  gar_app <- oauth_app("google", key = client.id, secret = client.secret)
  
  scope_list <- getOption("googleAuthR.scopes.selected")
  
  req <-
    POST("https://accounts.google.com/o/oauth2/token",
               body = list(code = code,
                           client_id = client.id,
                           client_secret = client.secret,
                           redirect_uri = redirect.uri,
                           grant_type = "authorization_code"))
  
  stopifnot(identical(headers(req)$`content-type`,
                      "application/json; charset=utf-8"))
  # content of req will contain access_token, token_type, expires_in
  token <- content(req, type = "application/json")
  if(!is.null(token$error)){
    stop("Authentication error: ", token$error, token$error_description, call. = FALSE)
  }
  # Create a Token2.0 object consistent with the token obtained from gar_auth()
  Token2.0$new(app = gar_app,
               endpoint = oauth_endpoints("google"),
               credentials = list(access_token = token$access_token,
                                  token_type = token$token_type,
                                  expires_in = token$expires_in,
                                  refresh_token = token$refresh_token),
               params = list(scope = scope_list, type = NULL,
                             use_oob = FALSE, as_header = TRUE),
               cache_path = FALSE)
  
}




#' Turn a googleAuthR data fetch function into a Shiny compatible one
#' 
#' @param f A function generated by \code{googleAuth_fetch_generator}.
#' @param shiny_access_token A reactive object that resolves to a token.
#' @param ... Other arguments passed to f.
#' @return the function f with an extra parameter, shiny_access_token=NULL.
#' @family shiny auth functions
#' @export
#' 
#' @examples
#' \dontrun{
#' ## in global.R
#' 
#' ## create the API call function, example with goo.gl URL shortner
#' library(googleAuthR)
#' options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/urlshortener"))
#' 
#' shorten_url <- function(url){
#' 
#'   body = list(
#'     longUrl = url
#'  )
#'  
#'  f <- gar_api_generator("https://www.googleapis.com/urlshortener/v1/url",
#'                         "POST",
#'                         data_parse_function = function(x) x$id)
#'                         
#'  f(the_body = body)
#'  
#'  }
#' 
#' 
#' ## in server.R
#' library(shiny)
#' library(googleAuthR)
#' source('global.R')
#' 
#' shinyServer(function(input, output, session)){
#'   
#'   ## Get auth code from return URL
#'   access_token  <- reactiveAccessToken(session)
#' 
#'   ## Make a loginButton to display using loginOutput
#'   output$loginButton <- renderLogin(session, access_token())
#'
#'   short_url_output <- eventReactive(input$submit, {
#'   ## wrap existing function with_shiny
#'   ## pass the reactive token in shiny_access_token
#'   ## pass other named arguments
#'     short_url <- with_shiny(f = shorten_url, 
#'                            shiny_access_token = access_token(),
#'                            url=input$url)
#'                            
#'    })
#'    
#'    output$short_url <- renderText({
#'    
#'      short_url_output()
#'      
#'    })
#'  }
#' 
#' ## in ui.R
#' library(shiny)
#' library(googleAuthR)
#' 
#' shinyUI(
#'   fluidPage(
#'     loginOutput("loginButton"),
#'     textInput("url", "Enter URL"),
#'     actionButton("submit", "Shorten URL"),
#'     textOutput("short_url")
#'     ))
#' }
with_shiny <- function(f, shiny_access_token=NULL, ...){
  if(is.null(shiny_access_token)) 
    stop("Need to provide the reactive access token in shiny_access_token argument. 
         e.g. shiny_access_token=access_token()")

  formals(f) <- c(formals(f), list(shiny_access_token=shiny_access_token))

  f(...)
}
