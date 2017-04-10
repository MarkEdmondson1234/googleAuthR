#' Shiny JavaScript Google Authorisation [UI Module]
#' 
#' A Javascript Google authorisation flow for Shiny apps.
#'
#' Shiny Module for use with \link{gar_auth_js}
#' 
#' @param id Shiny id
#' @param login_class CSS class of login button
#' @param logout_class CSS class ofr logout button
#' @param login_text Text to show on login button
#' @param logout_text Text to show on logout button
#'
#' @return Shiny UI
#' @export
gar_auth_jsUI <- function(id, 
                          login_class = "btn btn-primary",
                          logout_class = "btn btn-danger",
                          login_text = "Log In",
                          logout_text = "Log Out"){

  ns <- shiny::NS(id)

  shiny::tagList(

    shiny::tags$script(src='https://apis.google.com/js/auth.js'),
    shiny::tags$button(id = ns("login"), onclick="auth();", login_text, class = login_class),
    shiny::tags$button(id = ns("logout"), onclick="out();", logout_text, class = logout_class),
    shiny::tags$script(type="text/javascript", shiny::HTML(paste0("
      var authorizeButton = document.getElementById('",ns("login"),"');
      var signoutButton = document.getElementById('",ns("logout"),"');
      signoutButton.style.display = 'none';
      function auth() {
        var config = {
          'client_id': '",getOption("googleAuthR.webapp.client_id"),"',
          'scope': '", paste(getOption("googleAuthR.scopes.selected"), collapse = " "),"',
          'approval_prompt':'force'
        };
        gapi.auth.authorize(config, function() {
          token = gapi.auth.getToken();
          console.log('login complete');
          Shiny.onInputChange('",ns("js_auth_access_token"),"', token.access_token);
          Shiny.onInputChange('",ns("js_auth_token_type"),"', token.token_type);
          Shiny.onInputChange('",ns("js_auth_expires_in"),"', token.expires_in);
          authorizeButton.style.display = 'none';
          signoutButton.style.display = 'block';
        });
       }
       function out(){
          gapi.auth.signOut();
          location.reload()
       }
       "
    ) )
    )
  )

}

#' Shiny JavaScript Google Authorisation [Server Module]
#'
#' Shiny Module for use with \link{gar_auth_jsUI}
#'
#' Call via \code{shiny::callModule(gar_auth_js, "your_id")}
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#'
#' @return A httr reactive OAuth2.0 token
#' @import shiny
#' @export
gar_auth_js <- function(input, output, session){

    js_token <- shiny::reactive({
      shiny::validate(
        shiny::need(input$js_auth_access_token, "Authenticate")
      )
      
      list(access_token = input$js_auth_access_token,
           token_type = input$js_auth_token_type,
           expires_in = input$js_auth_expires_in
      )
      
    })
    
    ## Create access token
    access_token <- shiny::reactive({
      
      shiny::req(js_token())
      
      gar_js_getToken(js_token())
      
    })
    
    return(access_token)

}

#' Create a httr token from a js token
#' @keywords internal
gar_js_getToken <- function(token,
                            client.id     = getOption("googleAuthR.webapp.client_id"),
                            client.secret = getOption("googleAuthR.webapp.client_secret")){
  
  gar_app <- httr::oauth_app("google", key = client.id, secret = client.secret)
  
  scope_list <- getOption("googleAuthR.scope")
  
  # Create a Token2.0 object consistent with the token obtained from gar_auth()
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
  
  token_formatted
}

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
authReturnCode <- function(session, 
                           securityCode=getOption("googleAuthR.securitycode")){
  
  pars <- shiny::parseQueryString(session$clientData$url_search)
  
  if(!is.null(pars$state)){
    if(pars$state != securityCode){
      warning("securityCode check failed in Authentication! Code:", 
              pars$state, 
              " Expected:", 
              securityCode)
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
#' @keywords internal
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
    url <- httr::modify_url(
      httr::oauth_endpoints("google")$authorize,
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
gar_shiny_getUrl <- function(session){
  
  if(!is.null(session)){
    pathname <- session$clientData$url_pathname
    hostname <- session$clientData$url_hostname
    port <- session$clientData$url_port
    
    url <- paste0(session$clientData$url_protocol,
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
gar_shiny_getToken <- function(code,
                               redirect.uri,
                               client.id     = getOption("googleAuthR.webapp.client_id"),
                               client.secret = getOption("googleAuthR.webapp.client_secret")){
  
  gar_app <- httr::oauth_app("google", key = client.id, secret = client.secret)
  
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
  
  # Create a Token2.0 object consistent with the token obtained from gar_auth()
  token_formatted <-
    httr::Token2.0$new(app = gar_app,
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

#' Create a reactive Google OAuth2 token
#' 
#' Use within a Shiny server.R session to create the access token passed 
#'   to all Google API functions using \code{with_shiny}
#' 
#' @param session A Shiny session object.
#' 
#' @return A reactive Google auth token
#' @family shiny auth functions
#' @export
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
reactiveAccessToken <- function(session){
  message("reactiveAccessToken is deprecated as of googleAuthR 0.3.0. Use googleAuth() and googleAuthUI() instead.")
  shiny::reactive({
    ## gets all the parameters in the URL. The auth code should be one of them.
    
    if(!is.null(authReturnCode(session))){
      ## extract the authorization token
      app_url <- gar_shiny_getUrl(session)    
      access_token <- gar_shiny_getToken(authReturnCode(session), app_url)
      
      Authentication$set("public", "app_url", app_url, overwrite=TRUE)
      Authentication$set("public", "shiny", TRUE, overwrite=TRUE)
      
      access_token
      
    } else {
      NULL
    }
  })
}

#' Login/logout Shiny output
#' 
#' USe within a ui.R to render the login button generated by \code{renderLogin}
#' 
#' @param output_name Name of what output object was assigned in \code{renderLogin}
#' 
#' @return A login/logout button in a Shiny app
#' 
#' @export
#' @family shiny auth functions
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
loginOutput <- function(output_name){
  message("loginOutput is deprecated as of googleAuthR 0.3.0. Use googleAuth() and googleAuthUI() instead.")
  shiny::uiOutput(output_name)
}

#' Render a Google API Authentication Login/logout button
#' 
#' Use within a Shiny server.R to assign to an output for ui.R.
#' The login button carries an ActionLink with value "signed_in" 
#'   but as Shiny reloads on pushing it can't be used for detection of login state.  
#'   Use \code{!is.null(access_token())} instead.
#' 
#' @param session A Shiny session object
#' @param access_token A token generated by \code{reactiveAccessToken}
#' @param login_text What the login text will read on the button
#' @param logout_text What the logout text will read on the button
#' @param login_class The Bootstrap class for the login link
#' @param logout_class The Bootstrap class for the logout link
#' @param access_type Online or offline access for the authentication URL. 
#' @param revoke If TRUE a user on logout will need to re-authenticate.
#' @param approval_prompt Whether to show the consent screen on authentication.
#' @return An object to assign to output e.g. output$login
#' 
#' @seealso \link{revokeEventObserver}
#' 
#' @export
#' @family shiny auth functions
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
#'   ## revoke=TRUE means upon logout a user will need to reauthenticate
#'   output$loginButton <- renderLogin(session, access_token(), revoke=TRUE)
#'   
#'   ## Needed if revoke=TRUE above
#'   revokeEventObserver(access_token())
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
renderLogin <- function(session, 
                        access_token,
                        login_text="Login via Google",
                        logout_text="Logout",
                        login_class="btn btn-primary",
                        logout_class="btn btn-default",
                        access_type = c("online","offline"),
                        approval_prompt = c("auto","force"),
                        revoke = FALSE){
  message("renderLogin is deprecated as of googleAuthR 0.3.0. Use googleAuth() and googleAuthUI() instead.")
  access_type <- match.arg(access_type)
  approval_prompt <- match.arg(approval_prompt)
  
  shiny::renderUI({
    if(is.null(shiny::isolate(access_token))) {
      shiny::actionLink("signed_in",
                 shiny::a(login_text, 
                          href = gar_shiny_getAuthUrl(gar_shiny_getUrl(session), 
                                                      access_type = access_type,
                                                      approval_prompt = approval_prompt), 
                   class=login_class, 
                   role="button"))
    } else {
      if(revoke){
        
        logout_button <- shiny::actionButton("revoke", "Revoke Access", 
                                             href = gar_shiny_getUrl(session), 
                                             class=logout_class,
                                             role="button")

      } else {
        logout_button <- shiny::a(logout_text, 
                                  href = gar_shiny_getUrl(session), 
                                  class=logout_class, 
                                  role="button")
      }
      
      logout_button
      
    }
  })
}

#' A Login button (Shiny Module)
#' 
#' UI part of shiny module, use with \link{googleAuth}
#' 
#' @param id shiny id
#' 
#' @return A shiny UI for logging in
#' 
#' @family shiny module functions
#' @export
googleAuthUI <- function(id){
  
  ns <- shiny::NS(id)
  
  shiny::uiOutput(ns("googleAuthUi"))
}
  
  
#' Server side google auth (Shiny Module)
#' 
#' Server part of shiny module, use with \link{googleAuthUI}
#' 
#' Call via \code{shiny::callModule(googleAuth, "your_ui_name", login_text = "Login")}
#' 
#' 
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param login_text What the login text will read on the button
#' @param logout_text What the logout text will read on the button
#' @param login_class The CSS class for the login link
#' @param logout_class The CSS class for the logout link
#' @param access_type Online or offline access for the authentication URL
#' @param approval_prompt Whether to show the consent screen on authentication
#' @param revoke If TRUE a user on logout will need to re-authenticate
#' 
#' @return A reactive authentication token
#' 
#' @examples 
#' 
#' \dontrun{
#' options("googleAuthR.scopes.selected" = 
#'   c("https://www.googleapis.com/auth/urlshortener"))
#' 
#' shorten_url <- function(url){
#'   body = list(
#'     longUrl = url
#'  )
#'  
#'  f <- 
#'    gar_api_generator("https://www.googleapis.com/urlshortener/v1/url",
#'                      "POST",
#'                      data_parse_function = function(x) x$id)
#'                         
#'  f(the_body = body)
#'  
#'  }
#' 
#' server <- function(input, output, session){
#' 
#'   ## Create access token and render login button
#'   access_token <- callModule(googleAuth, 
#'                              "loginButton",
#'                              login_text = "Login1")
#' 
#'   short_url_output <- eventReactive(input$submit, {
#'     ## wrap existing function with_shiny
#'     ## pass the reactive token in shiny_access_token
#'     ## pass other named arguments
#'     with_shiny(f = shorten_url, 
#'                shiny_access_token = access_token(),
#'                url=input$url)
#'   })
#' 
#'   output$short_url <- renderText({
#' 
#'     short_url_output()
#' 
#'   })
#' 
#' }
#' 
#' ## ui
#' ui <- fluidPage(
#'   googleAuthUI("loginButton"),
#'   textInput("url", "Enter URL"),
#'   actionButton("submit", "Shorten URL"),
#'   textOutput("short_url")
#' )
#' 
#' shinyApp(ui = ui, server = server)
#' }
#' 
#' @family shiny module functions
#' @export
googleAuth <- function(input, output, session, 
                       login_text="Login via Google",
                       logout_text="Logout",
                       login_class="btn btn-primary",
                       logout_class="btn btn-default",
                       access_type = c("online","offline"),
                       approval_prompt = c("auto","force"),
                       revoke = FALSE){
  
  access_type <- match.arg(access_type)
  approval_prompt <- match.arg(approval_prompt)
  ns <- session$ns
  
  accessToken <- shiny::reactive({
    
      ## gets all the parameters in the URL. The auth code should be one of them.
      if(!is.null(authReturnCode(session))){
        ## extract the authorization token
        app_url <- gar_shiny_getUrl(session)    
        access_token <- gar_shiny_getToken(authReturnCode(session), app_url)
        
        Authentication$set("public", "app_url", app_url, overwrite=TRUE)
        Authentication$set("public", "shiny", TRUE, overwrite=TRUE)
        
        access_token
        
      } else {
        NULL
      }
    })
  
  output$googleAuthUi <- shiny::renderUI({
    

    
    if(is.null(shiny::isolate(accessToken()))) {
      shiny::actionLink(ns("signed_in"),
                        shiny::a(login_text, 
                                 href = gar_shiny_getAuthUrl(gar_shiny_getUrl(session), 
                                                             access_type = access_type,
                                                             approval_prompt = approval_prompt), 
                                 class=login_class, 
                                 role="button"))
    } else {
      if(revoke){
        
        logout_button <- shiny::actionButton(ns("revoke"), "Revoke Access", 
                                             href = gar_shiny_getUrl(session), 
                                             class=logout_class,
                                             role="button")
        
      } else {
        logout_button <- shiny::a(logout_text, 
                                  href = gar_shiny_getUrl(session), 
                                  class=logout_class, 
                                  role="button")
      }
      
      logout_button
      
    }
  })

  shiny::observeEvent(input[[ns("revoke")]], {
      
    ## GETS the revoke URL for this user's access_token
    httr::GET(httr::modify_url("https://accounts.google.com/o/oauth2/revoke",
                               query = 
                                 list(token = 
                                        shiny::isolate(access_token)$credentials$access_token)))
    myMessage("Revoked access", level=2)
  })

  return(accessToken)

}

#' Listens for a user revoking authentication
#' 
#' @description If the parameter \code{revoke} is set to TRUE for \link{renderLogin}
#'   then this observer is also required in the Shiny server to do the revoking.
#' 
#' @param access_token A token generated by \code{reactiveAccessToken}.
#' @param input the input object from a shinyServer function.
#' 
#' @seealso \link{renderLogin}
#' 
#' @export
#' @family shiny auth functions
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
#'   ## revoke=TRUE means upon logout a user will need to reauthenticate
#'   output$loginButton <- renderLogin(session, access_token(), revoke=TRUE)
#'   
#'   ## Needed if revoke=TRUE above
#'   revokeEventObserver(access_token())
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
revokeEventObserver <- function(access_token, input){
  message("revokeEventObserver is deprecated as of googleAuthR 0.3.0. Use googleAuth() and googleAuthUI() instead.")  
  shiny::observeEvent(input$revoke, {

        ## GETS the revoke URL for this user's access_token
        httr::GET(httr::modify_url("https://accounts.google.com/o/oauth2/revoke",
                                   query = 
                                   list(token = 
                                        shiny::isolate(access_token)$credentials$access_token)))
        myMessage("Revoked access", level=2)
  })

}

#' Turn a googleAuthR data fetch function into a Shiny compatible one
#' 
#' @param f A function generated by \code{googleAuth_fetch_generator}.
#' @param shiny_access_token A token generated within a \code{gar_shiny_getToken}.
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
    stop("Need to provide the reactive access token in shiny_access_token argument. e.g. shiny_access_token=access_token()")

  formals(f) <- c(formals(f), list(shiny_access_token=shiny_access_token))

  f(...)
}
