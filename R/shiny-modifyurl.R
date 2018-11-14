# from https://gist.github.com/MarkEdmondson1234/5321d4c61168a644505509b24a54e443

has_auth_code <- function(pars, securityCode=getOption("googleAuthR.securitycode")){
  # params is a list object containing the parsed URL parameters. Return code if
  # based on these parameters, it looks like auth codes are present that we can
  # use to get an access token. If not, it means we need to go through the OAuth
  # flow and returns NULL
  
  if(!is.null(pars$state)){
    if(pars$state != securityCode){
      warning("securityCode check failed in Authentication! Code:", 
              pars$state, 
              " Expected:", 
              securityCode)
      return(NULL)
    } 
  }
  
  # NULL if it isn't there
  pars$code
  
}

make_authorization_url <- function(req, 
                                   state = getOption("googleAuthR.securitycode"),
                                   client.id     = getOption("googleAuthR.webapp.client_id"),
                                   client.secret = getOption("googleAuthR.webapp.client_secret"),
                                   scope         = getOption("googleAuthR.scopes.selected"),
                                   access_type = c("online","offline"),
                                   approval_prompt = c("auto","force")) {
  access_type <- match.arg(access_type)
  approval_prompt <- match.arg(approval_prompt)
  # TODO: Implement for real
  # 
  # The req object is a Rook request. This is just an environment object that 
  # gives you access to the request URL, HTTP headers, etc. The documentation 
  # for this object is here:
  # https://github.com/jeffreyhorner/Rook#the-environment
  #
  # Implement this function by returning the URL that we should redirect the
  # user to in order to 
  
  if(req$SERVER_NAME == "127.0.0.1"){
    host <- "localhost"
  } else {
    host <- req$SERVER_NAME
  }
  
  url_redirect <- paste0(req$rook.url_scheme,"://",host,":",req$SERVER_PORT)

  if(req$PATH_INFO != "/"){
    url_redirect <- paste0(url_redirect, req$PATH_INFO)
  }
  gar_shiny_getAuthUrl(url_redirect,
                       state = state,
                       client.id     = client.id,
                       client.secret = client.secret,
                       scope         = scope,
                       access_type   = access_type,
                       approval_prompt = approval_prompt)
  
}

# A little-known feature of Shiny is that the UI can be a function, not just 
# objects. You can use this to dynamically render the UI based on the request. 
# We're going to pass this uiFunc, not ui, to shinyApp(). If you're using 
# ui.R/server.R style files, that's fine too--just make this function the last 
# expression in your ui.R file.

#' googleAuth_ui
#' 
#' A function that will turn your ui object into one that will look for authentication
#' 
#' @param req A Rook request
#' @param ui A Shiny ui object
#' 
#' @details 
#' 
#' Put this at the bottom of your ui.R or pass into shinyApp()
#' 
#' @export
googleAuth_ui <- function(ui){
  # make the ui available globally
  assertthat::assert_that(is.list(ui))
  Authentication$set("public", "ui", ui, overwrite=TRUE)
  # output the function
  make_googleAuth_ui
}


make_googleAuth_ui <- function(req){
  if(is.null(has_auth_code(shiny::parseQueryString(req$QUERY_STRING)))){
    authorization_url <- make_authorization_url(req)
    # This is silently redirecting the user to oauth. If you prefer, this could
    # be a pretty login page with a button-link.
    return(shiny::tags$script(shiny::HTML(sprintf("location.replace(\"%s\");", authorization_url))))
  } else {
    ui <- Authentication$public_fields$ui
    ui
  }
}


#' googleAuth_server
#' 
#' If using googleAuth_ui, put this at the top of your server.R function
#' 
#' @param session Shiny session argument
#' @export
googleAuth_server_token <- function(session){
  params <- shiny::parseQueryString(shiny::isolate(session$clientData$url_search))
  
  if(is.null(has_auth_code(params))) {
    return()
  }
  myMessage("Fetching Token", level = 3)
  token <- gar_shiny_getToken(params$code)
  # gar_auth(token)
  token
}


