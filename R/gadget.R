#' Gadget for easy authentication
#' @noRd
gar_gadget <- function(){
  ## No @import to avoid making shiny and miniUI an import
  check_package_loaded("shiny")
  check_package_loaded("miniUI")
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("googleAuthR Authentication", right = NULL),
    miniUI::miniContentPanel(
      shiny::uiOutput("button"),
      shiny::selectInput("api", label = "Select API to prefill scopes", choices = NULL),
      shiny::uiOutput("scope_selector"),
      shiny::div()
    )
  )
  
  server <- function(input, output, session) {
    
    returning <- shiny::reactive(authReturnCode(session))
    
    output$button <- shiny::renderUI({
      
      if(is.null(returning())){
        googleAuthUI("gadget")
      } else {
        shiny::helpText("Your R session is now authenticated, and you can close this window.")
      }

    })
    
    output$scope_selector <- shiny::renderUI({
      
      if(is.null(returning())){
        out <- shiny::tagList(
          shiny::selectInput("scopes_selected", 
                             label = "Current API scopes",
                             choices = getOption("googleAuthR.scopes.selected"),
                             selected = getOption("googleAuthR.scopes.selected"),
                             multiple = TRUE,
                             width = "100%"),
          shiny::selectInput("add_scopes",label = "Add new scopes", width = "100%", choices = NULL, multiple = TRUE),
          shiny::actionButton("do_add_scopes","Add scopes", icon = shiny::icon("plus")),
          shiny::helpText("Google API scopes are listed ", 
                          shiny::a(href="https://developers.google.com/identity/protocols/googlescopes", 
                                   "here", 
                                   target="_blank")),
          shiny::strong("googleAuthR.client_id"),
          shiny::helpText(getOption("googleAuthR.client_id")),
          shiny::strong("googleAuthR.webapp.client_id"),
          shiny::helpText(getOption("googleAuthR.webapp.client_id")),
          shiny::hr(),
          shiny::helpText("Ensure above settings match your", 
                          shiny::a(target = "_blank", 
                                   href="https://console.developers.google.com/apis/credentials", 
                                   "Google console API credentials"), 
                          "for successful authentication.")
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
        gar_auth(token)
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
      
      shiny::updateSelectInput(session,
                        "add_scopes",
                        choices = c(""))
    })
    
    apis <- shiny::reactive({
      gar_discovery_apis_list()
    })
    
    #update API list
    shiny::observe({
      shiny::req(apis())
      
      apis <- apis()
      
      choices <- apis$id
      names(choices) <- paste(apis$name, apis$version)
      
      shiny::updateSelectInput(session,
                        "api",
                        choices = choices)
      
    })
    
    shiny::observe({
      shiny::req(apis())
      shiny::req(input$api)
      
      id_obj <- strsplit(input$api, ":")[[1]]
      
      api_detail <- gar_discovery_api(id_obj[1], version = id_obj[2])
      
      scopes <- names(api_detail$auth$oauth2$scopes)
      
      shiny::updateSelectInput(session, "add_scopes", choices = scopes, selected = scopes)
      
    })
    
    
    
  }
  
  viewer <- shiny::dialogViewer("googleAuthR")
  shiny::runGadget(ui, server, viewer = viewer)
}