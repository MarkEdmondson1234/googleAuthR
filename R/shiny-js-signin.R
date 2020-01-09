#' Google SignIn [UI Module]
#'
#' Shiny Module for use with \link{googleSignIn}. 
#'   If you just want a login to a Shiny app, without API tokens.
#' 
#' @param id Shiny id.
#' @param logout_name Character. Custom name of the logout button.
#' @param logout_class Character. Bootstrap class name for buttons, e.g. "btn-info", "btn-dark".
#' 
#' @author Based on original code by David Kulp
#' 
#' @seealso \url{https://github.com/dkulp2/Google-Sign-In}
#'
#' @return Shiny UI
#' @export
googleSignInUI <- function(id, logout_name = "Sign Out", logout_class = "btn-danger"){

  ns <- shiny::NS(id)
  
  if(getOption("googleAuthR.webapp.client_id") == ""){
    stop("You need to set options('googleAuthR.webapp.client_id' = 'your.client.id') - see website: https://code.markedmondson.me/googleAuthR/articles/google-authentication-types.html#googlesignin-module-example", call. = FALSE)
  }
  
  shiny::tagList(
    shiny::tags$head(
      shiny::tags$meta(name="google-signin-scope", content="profile email"),
      shiny::tags$meta(name="google-signin-client_id", content=getOption("googleAuthR.webapp.client_id")),
      shiny::HTML('<script src="https://apis.google.com/js/platform.js?onload=init"></script>')
    ),
    shiny::div(id=ns("signin"), class="g-signin2", "data-onsuccess"="onSignIn"),
    shiny::tags$button(id = ns("signout"), logout_name, onclick = "signOut();", class = logout_class),
    load_js_template("js/signin-top.js",
                     ns("signin"), ns("signout") ,ns("g_id"), ns("g_name"), ns("g_image"), ns("g_email")),
    load_js_template("js/signin-bottom.js",
                     ns("g_id"), ns("g_name"), ns("g_image"), ns("g_email"), ns("signed_in"))
  )
}



#' Google SignIn [Server Module]
#'
#' Shiny Module for use with \link{googleSignInUI}.  
#'   Use when you don't need to call APIs, but would like a login to Shiny.
#'
#' Call via \code{shiny::callModule(googleSignIn, "your_id")}.
#'
#' @param input shiny input (must contain \code{g_id}, \code{g_name},
#'   \code{g_email}, \code{g_image}, \code{g_signed_in})
#' @param output shiny output (passed by shiny but not used)
#' @param session shiny session
#'
#' @author Based on original code by David Kulp
#' @return A reactive list with values \code{$id}, \code{$name}, \code{$email},
#'   \code{$image} and \code{$signed_in}.
#' @export
googleSignIn <- function(input, output, session){
  check_package_loaded("shiny")

  # ns <- session$ns

  shiny::reactive({
    shiny::req(input$g_id)

    list(
      id = input$g_id,
      name = input$g_name,
      email = input$g_email,
      image = input$g_image,
      signed_in = input$signed_in
    )
    
  })
  
}