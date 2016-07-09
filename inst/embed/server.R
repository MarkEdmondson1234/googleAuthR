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


## server.R
server <- function(input, output, session){
  
  js_token <- reactive({
    
    list(access_token = input$js_auth_access_token,
                token_type = input$js_auth_token_type,
                expires_in = input$js_auth_expires_in
    )
    
  })
  
  ## Create access token and render login button
  # access_token <- callModule(googleAuth, "loginButton", approval_prompt = "force")
  access_token <- reactive({
    
    req(js_token())
    
    token <- js_token()
    
    gar_js_getToken(token)
    
  })
  
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