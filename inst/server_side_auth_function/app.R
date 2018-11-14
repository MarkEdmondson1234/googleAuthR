library(shiny)
library(googleAuthR)
options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/drive")
options(googleAuthR.webapp.client_id = "1080525199262-qecndq7frddi66vr35brgckc1md5rgcl.apps.googleusercontent.com")
options(googleAuthR.webapp.client_secret = "3nVkQuvrooNO8t2OId4Vtha4")

fileSearch <- function(query) {
  googleAuthR::gar_api_generator("https://www.googleapis.com/drive/v3/files/",
                                 "GET",
                                 pars_args=list(q=query),
                                 data_parse_function = function(x) x$files)()
}

## ui.R
ui <- fluidPage(title = "googleAuthR Shiny Demo",
                textInput("query", 
                          label = "Google Drive query", 
                          value = "mimeType != 'application/vnd.google-apps.folder'"),
                tableOutput("gdrive")
)

## server.R
server <- function(input, output, session){
  
  # create a non-reactive access_token as we should never get past this is not authenticated
  access_token <- googleAuth_server_token(session)
  
  output$gdrive <- renderTable({
    req(token())
    req(input$query)
    
    with_shiny(fileSearch, shiny_access_token = access_token, query = input$query)
    
  })
}

shinyApp(googleAuth_ui(ui), server)