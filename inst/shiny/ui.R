library(shiny)
library(googleAuthR)

## ui.R
ui <- fluidPage(title = "googleAuthR Shiny Demo",
  googleAuthUI("loginButton"),
  textInput("url", "Enter URL"),
  actionButton("submit", "Shorten URL"),
  textOutput("short_url")
)