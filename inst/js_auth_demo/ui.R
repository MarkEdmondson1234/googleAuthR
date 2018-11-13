library(shiny)
library(googleAuthR)

gar_set_client()

## ui.R
ui <- fluidPage(
  gar_auth_jsUI("js_token"),
  textInput("url", "Enter URL"),
  actionButton("submit", "Shorten URL"),
  textOutput("short_url")
)
