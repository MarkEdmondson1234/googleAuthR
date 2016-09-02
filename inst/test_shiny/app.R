library(shiny)
library(googleAuthR)

shinyApp(
  ui = fluidPage(title = "googleAuthR Test Login",
                 googleAuthUI("loginButton")
  ),
  server = function(input, output, session){
    if(file.exists("auth_success.rds")){
      unlink("auth_success.rds")
    }
    ## Create access token and render login button
    access_token <- callModule(googleAuth, "loginButton", approval_prompt = "force")
    
    observe({
      req(access_token())
      
      cat("\n## Shiny auth sucessful")
      saveRDS(TRUE, "auth_success.rds")
      stopApp(TRUE)
    })
    
  }
)