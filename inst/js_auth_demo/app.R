library(shiny)
library(googleAuthR)

gar_set_client()

## ui.R
ui <- fluidPage(
  googleAuth_jsUI("js_token"),
  textInput("url", "Enter URL"),
  actionButton("submit", "Shorten URL"),
  textOutput("short_url")
)

shorten_url <- function(url){
  
  body = list(
    longUrl = url
  )
  
  f <- gar_api_generator("https://www.googleapis.com/urlshortener/v1/url",
                         "POST",
                         data_parse_function = function(x) x$id)
  
  f(the_body = body)
  
}

## server.R
server <- function(input, output, session){
  
  access_token <- callModule(googleAuth_js, "js_token")
  
  short_url_output <- eventReactive(input$submit, {
    ## wrap existing function with_shiny
    ## pass the reactive token in shiny_access_token
    ## pass other named arguments
    with_shiny(f = shorten_url, 
               shiny_access_token = access_token(),
               url=input$url)
    
  })
  
  output$short_url <- renderText({
    
    short_url_output()
    
  })
}

shinyApp(ui, server)
