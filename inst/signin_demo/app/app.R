library(shiny)
library(googleAuthR)

options(googleAuthR.webapp.client_id = "1080525199262-qecndq7frddi66vr35brgckc1md5rgcl.apps.googleusercontent.com")

ui <- fluidPage(
    
    titlePanel("Sample Google Sign-In"),
    
    sidebarLayout(
      sidebarPanel(
        googleSignInUI("demo")
      ),
      
      mainPanel(
        with(tags, dl(dt("Name"), dd(textOutput("g_name")),
                      dt("Email"), dd(textOutput("g_email")),
                      dt("Image"), dd(uiOutput("g_image")) ))
      )
    )
  )

server <- function(input, output, session) {
  
  sign_ins <- shiny::callModule(googleSignIn, "demo")
  
  output$g_name = renderText({ sign_ins()$name })
  output$g_email = renderText({ sign_ins()$email })
  output$g_image = renderUI({ img(src=sign_ins()$image) })
  
  observe({
    req(sign_ins()$name)
    
    print("This works")
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)