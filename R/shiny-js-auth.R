#' Shiny JavaScript Google Authorisation [UI Module]
#' 
#' A Javascript Google authorisation flow for Shiny apps.
#'
#' Shiny Module for use with \link{googleAuth_js}
#' 
#' @param id Shiny id
#' @param login_class CSS class of login button
#' @param logout_class CSS class of logout button
#' @param login_text Text to show on login button
#' @param logout_text Text to show on logout button
#' @param prompt The type of login screen 
#' @param scopes Set the scopes, minimum needs is "email"
#'
#' @return Shiny UI
#' @import assertthat
#' @export
googleAuth_jsUI <- function(id, 
                          login_class = "btn btn-primary",
                          logout_class = "btn btn-danger",
                          login_text = "Log In",
                          logout_text = "Log Out",
                          prompt = c("consent", "select_account", "both", "none"),
                          scopes = getOption("googleAuthR.scopes.selected", "email")){
  check_package_loaded("shiny")
  prompt <- match.arg(prompt)
  
  assert_that(
    is.string(login_class),
    is.string(logout_class),
    is.string(login_text),
    is.string(logout_text),
    !is.null(scopes)
  )
  
  if(prompt == "both"){
    prompt <- "consent select_account"
  }
  
  approval_prompt_line <- paste0(",\n 'prompt':'",prompt,"'")
  
  ## No @import to avoid making shiny and miniUI an import

  ns <- shiny::NS(id)
  shiny::tagList(
    
    shiny::tags$script(src='https://apis.google.com/js/auth.js'),
    shiny::tags$button(id = ns("login"), onclick="auth();", login_text, class = login_class),
    shiny::tags$button(id = ns("logout"), onclick="out();", logout_text, class = logout_class),
    load_js_template("js/js-auth.js",        
                     ns("login"), 
                     ns("logout"),
                     getOption("googleAuthR.webapp.client_id"),
                     paste(scopes, collapse = " "),
                     approval_prompt_line,
                     ns("js_auth_access_token"),
                     ns("js_auth_token_type"),
                     ns("js_auth_expires_in"))
  )
  
}


#' Shiny JavaScript Google Authorisation [Server Module]
#'
#' Shiny Module for use with \link{googleAuth_jsUI}
#'
#' Call via \code{shiny::callModule(googleAuth_js, "your_id")}
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param message The message to show when not authenticated
#'
#' @return A httr reactive OAuth2.0 token
#' @export
googleAuth_js <- function(input, output, session, message = "Authenticate with your Google account"){
  check_package_loaded("shiny")
  js_token <- shiny::reactive({
    shiny::validate(
      shiny::need(input$js_auth_access_token, message)
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
#' @noRd
#' @importFrom httr oauth_app Token2.0 oauth_endpoints
gar_js_getToken <- function(token,
                            client.id     = getOption("googleAuthR.webapp.client_id"),
                            client.secret = getOption("googleAuthR.webapp.client_secret")){
  check_package_loaded("shiny")
  gar_app <- oauth_app("google", key = client.id, secret = client.secret)
  
  scope_list <- getOption("googleAuthR.scopes.selected")
  
  # Create a Token2.0 object consistent with the token obtained from gar_auth()
  token_formatted <-
    Token2.0$new(app = gar_app,
                 endpoint = oauth_endpoints("google"),
                 credentials = list(access_token = token$access_token,
                                    token_type = token$token_type,
                                    expires_in = token$expires_in,
                                    refresh_token = NULL),
                 params = list(scope = scope_list, type = NULL,
                               use_oob = FALSE, as_header = TRUE),
                 cache_path = FALSE)
  
  token_formatted
}