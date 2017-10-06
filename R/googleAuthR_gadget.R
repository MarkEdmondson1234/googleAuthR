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
      uiOutput("button"),
      selectInput("api", label = "Select API to prefill scopes", choices = NULL),
      uiOutput("scope_selector"),
      div()
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
    
    output$scope_selector <- renderUI({
      
      if(is.null(returning())){
        out <- tagList(
          selectInput("scopes_selected", 
                             label = "Current API scopes",
                             choices = getOption("googleAuthR.scopes.selected"),
                             selected = getOption("googleAuthR.scopes.selected"),
                             multiple = TRUE,
                             width = "100%"),
          selectInput("add_scopes",label = "Add new scopes", width = "100%", choices = NULL, multiple = TRUE),
          actionButton("do_add_scopes","Add scopes", icon = icon("plus")),
          helpText("Google API scopes are listed ", 
                          a(href="https://developers.google.com/identity/protocols/googlescopes", 
                                   "here", 
                                   target="_blank")),
          strong("googleAuthR.client_id"),
          helpText(getOption("googleAuthR.client_id")),
          strong("googleAuthR.webapp.client_id"),
          helpText(getOption("googleAuthR.webapp.client_id")),
          hr(),
          helpText("Ensure above settings match your", a(href="https://console.developers.google.com/apis/credentials", "Google console API credentials"), "for successful authentication.")
        )
        } else {
          NULL
        }
      
    })
    
    ##update token()
    observe({
      
      if(is.null(returning())){
        options("googleAuthR.scopes.selected" = input$scopes_selected)
      }

      token <- callModule(googleAuth, 
                                 "gadget", 
                                 login_text = "Login",
                                 approval_prompt="force")
      
      token <- token()
      
      if(!is.null(token)){
        gar_auth(token)
        message("Authentication complete for: ", 
                paste(getOption("googleAuthR.scopes.selected"), 
                      sep=",", 
                      collapse =" ")
                )
        message("You can safely close the browser window.")
        stopApp()
      }

      
    })
    
    #update input$scopes_selected
    observeEvent(input$do_add_scopes, {
      validate(
        need(input$add_scopes,"add_scopes")
      )
      
      if(is.null(returning())){
        options("googleAuthR.scopes.selected" = input$scopes_selected)
        
        now_scopes <- getOption("googleAuthR.scopes.selected")
        new_scopes <- unlist(strsplit(input$add_scopes, " "))
        
        scopes <- c(new_scopes, now_scopes)
        
      } else {
        scopes <- NULL
      }
      
      updateSelectInput(session,
                        "scopes_selected",
                        choices = scopes,
                        selected = scopes)
      
      updateSelectInput(session,
                        "add_scopes",
                        choices = c(""))
    })
    
    apis <- reactive({
      gar_discovery_apis_list()
    })
    
    #update API list
    observe({
      req(apis())
      
      apis <- apis()
      
      choices <- apis$id
      names(choices) <- paste(apis$name, apis$version)
      
      updateSelectInput(session,
                        "api",
                        choices = choices)
      
    })
    
    observe({
      req(apis())
      req(input$api)
      
      id_obj <- strsplit(input$api, ":")[[1]]
      
      api_detail <- gar_discovery_api(id_obj[1], version = id_obj[2])
      
      scopes <- names(api_detail$auth$oauth2$scopes)
      
      updateSelectInput(session, "add_scopes", choices = scopes, selected = scopes)
      
    })
    
    
    
  }
  
  viewer <- dialogViewer("googleAuthR")
  runGadget(ui, server, viewer = viewer)
}