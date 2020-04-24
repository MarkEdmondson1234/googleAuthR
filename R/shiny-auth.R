#' Shiny Google Authorisation [UI Module]
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
  check_package_loaded("shiny")
  ns <- shiny::NS(id)
  
  shiny::uiOutput(ns("googleAuthUi"))
}


#' Shiny Google Authorisation [Server Module]
#' 
#' Server part of shiny module, use with \link{googleAuthUI}
#' 
#' Call via \code{shiny::callModule(googleAuth, "your_ui_name", login_text = "Login")}
#' 
#' In some platforms the URL you are authenticating from will not match the Docker container the script is running in (e.g. shinyapps.io or a kubernetes cluster) - in that case you can manually set it via `options(googleAuthR.redirect = http://your-shiny-url`).  In other circumstances the Shiny app should be able to detect this itself. 
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
#' @param prompt What type of consent screen on authentication
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
                       prompt = c("consent", "select_account", "both", "none"),
                       revoke = FALSE){
  check_package_loaded("shiny")
  
  access_type     <- match.arg(access_type)
  prompt <- match.arg(prompt)
  ns              <- session$ns
  
  accessToken <- shiny::reactive({
    
    ## gets all the parameters in the URL. The auth code should be one of them.
    if(!is.null(authReturnCode(session))){
      ## extract the authorization token
      app_url <- gar_shiny_getUrl(session)    
      access_token <- gar_shiny_getToken(authReturnCode(session), app_url)
      
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
                                                             prompt = prompt), 
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