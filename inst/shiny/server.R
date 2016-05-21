library(shiny)
library(googleAuthR)
options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/urlshortener")

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
  
  ## Create access token and render login button
  access_token <- callModule(googleAuth, "loginButton", approval_prompt = "force")
  
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