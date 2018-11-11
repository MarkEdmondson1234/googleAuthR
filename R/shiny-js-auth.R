#' Shiny JavaScript Google Authorisation [UI Module]
#' 
#' A Javascript Google authorisation flow for Shiny apps.
#'
#' Shiny Module for use with \link{gar_auth_js}
#' 
#' @param id Shiny id
#' @param login_class CSS class of login button
#' @param logout_class CSS class of logout button
#' @param login_text Text to show on login button
#' @param logout_text Text to show on logout button
#' @param approval_prompt_force Whether to force a login each time
#'
#' @return Shiny UI
#' @import assertthat
#' @export
gar_auth_jsUI <- function(id, 
                          login_class = "btn btn-primary",
                          logout_class = "btn btn-danger",
                          login_text = "Log In",
                          logout_text = "Log Out",
                          approval_prompt_force = TRUE){
  
  assert_that(
    is.string(login_class),
    is.string(logout_class),
    is.string(login_text),
    is.string(logout_text),
    is.flag(approval_prompt_force)
  )
  
  if(approval_prompt_force){
    approval_prompt_line <- ",\n          'approval_prompt':'force'"
  } else {
    approval_prompt_line <-NULL
  }
  
  ## No @import to avoid making shiny and miniUI an import
  check_package_loaded("shiny")
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
                                                                  'scope': '", paste(getOption("googleAuthR.scopes.selected"), collapse = " "),"'",
                                                                  approval_prompt_line,"
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
#' @export
gar_auth_js <- function(input, output, session){
  check_package_loaded("shiny")
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
#' @noRd
#' @importFrom httr oauth_app Token2.0 oauth_endpoints
gar_js_getToken <- function(token,
                            client.id     = getOption("googleAuthR.webapp.client_id"),
                            client.secret = getOption("googleAuthR.webapp.client_secret")){
  check_package_loaded("shiny")
  gar_app <- oauth_app("google", key = client.id, secret = client.secret)
  
  scope_list <- getOption("googleAuthR.scope")
  
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