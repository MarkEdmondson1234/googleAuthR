library(shiny)
library(googleAuthR)
gar_set_client(scopes = "https://www.googleapis.com/auth/drive")

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
  
  # create a non-reactive access_token as we should never get past this if not authenticated
  gar_shiny_auth(session)
  
  output$gdrive <- renderTable({
    req(input$query)
    
    # no need for with_shiny()
    fileSearch(input$query)
    
  })
}

shinyApp(gar_shiny_ui(ui, login_ui = gar_shiny_login_ui), server)