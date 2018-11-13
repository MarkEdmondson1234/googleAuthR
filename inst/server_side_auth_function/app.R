library(shiny)
library(googleAuthR)
options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/urlshortener")
options(googleAuthR.webapp.client_id = "1080525199262-qecndq7frddi66vr35brgckc1md5rgcl.apps.googleusercontent.com")
options(googleAuthR.webapp.client_secret = "3nVkQuvrooNO8t2OId4Vtha4")

shorten_url <- function(url){
  
  body = list(
    longUrl = url
  )
  
  f <- gar_api_generator("https://www.googleapis.com/urlshortener/v1/url",
                         "POST",
                         data_parse_function = function(x) x$id)
  
  f(the_body = body)
  
}

## ui.R
ui <- fluidPage(title = "googleAuthR Shiny Demo",

  textInput("url", "Enter URL"),
  actionButton("submit", "Shorten URL"),
  textOutput("short_url")
)

## server.R
server <- function(input, output, session){
  
  # create a non-reactive access_token as we should never get past this is not authenticated
  access_token <- googleAuth_server_token(session)
  
  short_url_output <- eventReactive(input$submit, {
    ## wrap existing function with_shiny
    ## pass the reactive token in shiny_access_token
    ## pass other named arguments
    with_shiny(f = shorten_url, 
               shiny_access_token = access_token,
               url=input$url)
    
  })
  
  output$short_url <- renderText({
    
    short_url_output()
    
  })
}

shinyApp(googleAuth_ui, server)