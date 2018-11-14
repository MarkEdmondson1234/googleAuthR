# from https://gist.github.com/MarkEdmondson1234/5321d4c61168a644505509b24a54e443

has_auth_code <- function(pars, securityCode=getOption("googleAuthR.securitycode")){
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

#' Create a Google login before your Shiny UI launches
#' 
#' A function that will turn your ui object into one that will look for Google 
#'   authentication before loading the main app. Use together with \link{gar_shiny_auth}
#' 
#' @param ui A Shiny ui object
#' 
#' @details 
#' 
#' Put this at the bottom of your ui.R or pass into \link[shiny]{shinyApp} wrapping your created ui.
#' 
#' @export
#' @examples 
#' 
#' \dontrun{
#' library(shiny)
#' library(googleAuthR)
#' gar_set_client()
#' 
#' fileSearch <- function(query) {
#'   googleAuthR::gar_api_generator("https://www.googleapis.com/drive/v3/files/",
#'                                 "GET",
#'                                 pars_args=list(q=query),
#'                                 data_parse_function = function(x) x$files)()
#' }
#' 
#' ## ui.R
#' ui <- fluidPage(title = "googleAuthR Shiny Demo",
#'                 textInput("query", 
#'                 label = "Google Drive query", 
#'                 value = "mimeType != 'application/vnd.google-apps.folder'"),
#'                 tableOutput("gdrive")
#'                 )
#'                 
#' ## server.R
#' server <- function(input, output, session){
#' 
#' # this is not reactive, no need as you only reach here authenticated
#' gar_shiny_auth(session)
#' 
#' output$gdrive <- renderTable({
#'   req(input$query)
#'   
#'   # no need for with_shiny()
#'   fileSearch(input$query)
#'   
#'   })
#'   }
#' 
#' # gar_shiny_ui() needs to wrap the ui you have created above.
#' shinyApp(gar_shiny_ui(ui), server)
#' }
#' @family pre-load shiny authentication
gar_shiny_ui <- function(ui){
  check_package_loaded("shiny")
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
    Authentication$public_fields$ui
  }
}


#' Create Authentication within Shiny's server.R
#' 
#' If using  \link{gar_shiny_ui}, put this at the top of your server.R function
#' 
#' @param session Shiny session argument
#' 
#' @description 
#' 
#' This can be used at the top of the server function for authentication when you have used
#'   \link{gar_shiny_ui} to create a login page for your ui function.
#' 
#' @export
#' @examples 
#' 
#' \dontrun{
#' library(shiny)
#' library(googleAuthR)
#' gar_set_client()
#' 
#' fileSearch <- function(query) {
#'   googleAuthR::gar_api_generator("https://www.googleapis.com/drive/v3/files/",
#'                                 "GET",
#'                                 pars_args=list(q=query),
#'                                 data_parse_function = function(x) x$files)()
#' }
#' 
#' ## ui.R
#' ui <- fluidPage(title = "googleAuthR Shiny Demo",
#'                 textInput("query", 
#'                 label = "Google Drive query", 
#'                 value = "mimeType != 'application/vnd.google-apps.folder'"),
#'                 tableOutput("gdrive")
#'                 )
#'                 
#' ## server.R
#' server <- function(input, output, session){
#' 
#' # this is not reactive, no need as you only reach here authenticated
#' gar_shiny_auth(session)
#' 
#' output$gdrive <- renderTable({
#'   req(input$query)
#'   
#'   # no need for with_shiny()
#'   fileSearch(input$query)
#'   
#'   })
#'   }
#' 
#' # gar_shiny_ui() needs to wrap the ui you have created above.
#' shinyApp(gar_shiny_ui(ui), server)
#' }
#' @family pre-load shiny authentication
gar_shiny_auth <- function(session){
  check_package_loaded("shiny")
  params <- shiny::parseQueryString(shiny::isolate(session$clientData$url_search))
  
  if(is.null(has_auth_code(params))) {
    return()
  }
  
  url_redirect <- gar_shiny_getUrl(session)
  
  token <- gar_shiny_getToken(params$code, 
                              redirect.uri = url_redirect)
  gar_auth(token)
}


