library(shiny)
library(googleAuthR)

gar_set_client(scopes = "https://www.googleapis.com/auth/drive")

fileSearch <- function(query) {
  googleAuthR::gar_api_generator("https://www.googleapis.com/drive/v3/files/",
                                 "GET",
                                 pars_args=list(q=query),
                                 data_parse_function = function(x) x$files)()
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  googleAuthUI("loginButton"),
  textInput("query", 
            label = "Google Drive query", 
            value = "mimeType != 'application/vnd.google-apps.folder'"),
  tableOutput("gdrive")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  token <- callModule(googleAuth, 
                      "loginButton",
                      login_text = "Login")
  
  output$gdrive <- renderTable({
    req(token())
    req(input$query)
    
    with_shiny(fileSearch, shiny_access_token = token(), query = input$query)
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

