#' Gadget for easy authentication
#' @noRd
#' @import shiny
#' @import miniUI
gar_gadget <- function(){
  check_package_loaded("shiny")
  check_package_loaded("miniUI")
  ui <- miniPage(
    gadgetTitleBar("googleAuthR Authentication", right = NULL),
    miniContentPanel(
      selectInput("api", label = "Select API to prefill scopes", choices = NULL),
      actionButton("get_scope", label = "Prefill scope"),
      uiOutput("button"),
      uiOutput("scope_selector")
    )
  )
  
  server <- function(input, output, session) {
    
    returning <- reactive(authReturnCode(session))
    
    output$button <- renderUI({
      
      if(is.null(returning())){
        googleAuthUI("gadget")
      } else {
        helpText("Your R session is now authenticated, and you can close this window.")
      }

    })
    
    output$scope_selector <- shiny::renderUI({
      
      if(is.null(returning())){
        out <- shiny::tagList(
          shiny::selectInput("scopes_selected", 
                             label = "API scopes for authentication",
                             choices = getOption("googleAuthR.scopes.selected"),
                             selected = getOption("googleAuthR.scopes.selected"),
                             multiple = TRUE,
                             width = "100%"),
          shiny::textInput("add_scopes","New scopes", width = "100%"),
          shiny::actionButton("do_add_scopes","Add scopes", icon = shiny::icon("plus")),
          shiny::helpText("Google API scopes are listed ", 
                          shiny::a(href="https://developers.google.com/identity/protocols/googlescopes", 
                                   "here", 
                                   target="_blank")),
          shiny::strong("googleAuthR.client_id"),
          shiny::helpText(getOption("googleAuthR.client_id")),
          shiny::strong("googleAuthR.webapp.client_id"),
          shiny::helpText(getOption("googleAuthR.webapp.client_id")),
          shiny::strong("shiny.port"),
          shiny::helpText(getOption("shiny.port")),
          shiny::hr(),
          shiny::helpText("Ensure above settings match your", shiny::a(href="https://console.developers.google.com/apis/credentials", "Google console API credentials"), "for successful authentication."),
          shiny::helpText("googleAuthR defaults for example purposes only.")
        )
        } else {
          NULL
        }
      
    })
    
    ##update token()
    shiny::observe({
      
      if(is.null(returning())){
        options("googleAuthR.scopes.selected" = input$scopes_selected)
      }

      token <- shiny::callModule(googleAuth, 
                                 "gadget", 
                                 login_text = "Login",
                                 approval_prompt="force")
      
      token <- token()
      
      if(!is.null(token)){
        googleAuthR::gar_auth(token)
        message("Authentication complete for: ", 
                paste(getOption("googleAuthR.scopes.selected"), 
                      sep=",", 
                      collapse =" ")
                )
        message("You can safely close the browser window.")
        shiny::stopApp()
      }

      
    })
    
    #update input$scopes_selected
    shiny::observeEvent(input$do_add_scopes, {
      shiny::validate(
        shiny::need(input$add_scopes,"add_scopes")
      )
      
      if(is.null(returning())){
        options("googleAuthR.scopes.selected" = input$scopes_selected)
        
        now_scopes <- getOption("googleAuthR.scopes.selected")
        new_scopes <- unlist(strsplit(input$add_scopes, " "))
        
        scopes <- c(new_scopes, now_scopes)
        
      } else {
        scopes <- NULL
      }
      
      shiny::updateSelectInput(session,
                               "scopes_selected",
                               choices = scopes,
                               selected = scopes)
    })
    
    #update API list
    observe({
      
      apis <- gar_discovery_apis_list()
      
      choices <- apis$id
      names(choices) <- paste(apis$name, apis$version)
      
      updateSelectInput(session,
                        "api",
                        choices = choices)
      
    })
    
  }
  
  viewer <- shiny::dialogViewer("googleAuthR")
  shiny::runGadget(ui, server, viewer = viewer)
}