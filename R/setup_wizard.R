#' Setup wizard - introduction helper
#' 
#' Salutation and initial menu
#' 
#' @param choices A character vector of the choices passed to \link{menu}
#' @param package_name The package the setup menu is for
#' 
#' @return The number option from the menu
#' 
#' @export
#' @importFrom cli cli_alert_info cli_rule
#' @importFrom utils menu
#' @family setup functions
gar_setup_menu <- function(choices,
                           package_name = "googleAuthR"){
  
  title <- sprintf("==Welcome to %s v%s setup==",
                   package_name, packageVersion(package_name))
  
  cli_alert_info(title)
  
  if(!interactive()){
    stop("Can only be used in an interactive R session", call. = FALSE)
  }
  
  ready <- menu(title = "This wizard will scan your system for setup options and help you with any that are missing. \nHit 0 or ESC to cancel.",
                choices = choices)
  
  if(ready == 0){
    return(invisible(""))
  }
  cli_rule()
  
  ready
}

#' Setup wizard help - the functions that will execute on different menu options
#' 
#' @param menu_option The menu option chosen from \link{menu} or \link{gar_setup_menu}
#' @param trigger What option will trigger the \code{do_function}
#' @param do_function The function in the same order as the menu options
#' @param stop Whether to stop and exit if the function comes back FALSE
#' @param ... arguments passed to \code{do_function}
#' 
#' @details 
#' 
#' The functions should come back with TRUE or FALSE depending on if the setting was successful.
#' 
#' @return FALSE if setting was not set, TRUE if it was
#' 
#' @export
#' @examples 
#' 
#' \dontrun{
#' 
#' op <- gar_setup_menu(c("Check all settings",
#'                        "Configure authentication",
#'                        "Configure env arg 1",
#'                        "Configure env arg 2",
#'                        "Configure something else"
#'                        ),
#'                        package_name = "googleAuthR")
#'                        
#' choice <- gar_setup_check_session()
#' 
#' custom_env_check_f <- function(choice){
#'   r <- readline("project-id:")
#'   gar_setup_env_check("ARG2", 
#'                       set_to = r, 
#'                       edit_option = choice == 1, #allow editing of env arg
#'                       session_user = choice)
#'   TRUE
#' }
#' 
#' gar_setup_menu_do(op, c(1,2), my_setup_auth_f, stop = TRUE)
#' gar_setup_menu_do(op, c(1,3), gar_setup_env_check, 
#'                   env_arg = "ARG1", set_to = "BLAH", 
#'                   edit_option = choice == 1, #allow editing of env arg
#'                   session_user = choice)
#' gar_setup_menu_do(op, c(1,4), custom_env_check_f)
#' gar_setup_menu_do(op, c(1,4), my_setup_something_f)
#' 
#' }
#' @family setup functions
gar_setup_menu_do <- function(menu_option,
                              trigger,
                              do_function,
                              stop = FALSE,
                              ...){
  
  assert_that(is.scalar(menu_option),
              is.numeric(trigger),
              is.function(do_function),
              is.flag(stop))

  myMessage("Checking do_function ", 
            as.character(substitute(do_function)), level = 2)
  if(!menu_option %in% trigger){
    return(FALSE)
  }
  
  setting <- do_function(...)

  assert_that(is.flag(setting))
  if(isFALSE(setting) && stop){
    stop("Setting is required", call. = FALSE)
  }
  
  TRUE
  
  
}


#' Setup wizard help - check if environment argument is set
#' 
#' @return TRUE once changes made
#' @param env_arg The environment argument to check
#' @param set_to NULL or a string to set in .Renviron
#' @param edit_option Pass edit_option = FALSE to edit an existing environment arg
#' @param session_user 1=user, 2=project scope of .Renviron
#' 
#' @details 
#' 
#' Pass edit_option = FALSE to edit an existing environment arg, TRUE will 
#'   check if it exists, and will pass if its present.
#' @import assertthat
#' @importFrom cli cli_alert_info cli_alert_success
#' @importFrom utils menu
#' @export
#' @family setup functions
gar_setup_env_check <- function(env_arg,
                                set_to,
                                edit_option = FALSE,
                                session_user){
  arg <- Sys.getenv(env_arg)
  
  if(arg == ""){
    cli_alert_danger("No environment argument detected: {env_arg}")
  } else if(edit_option) {
    cli_alert_success("Found: {env_arg}={arg}")
    yes_edit <- menu(title = "Do you want to edit this setting?",
                     choices = c("Yes", "No, leave it as it is"))
    if(yes_edit == 2){
      return(TRUE)
    }
    cli_alert_info("Editing environment argument: {env_arg}={arg}")
  } else {
    cli_alert_success("Found: {env_arg}={arg}")
    return(TRUE)
  }

  if(!is.null(set_to)){
    assert_that(is.string(set_to))
    gar_setup_edit_renviron(set_to, session_user = session_user)
    return(TRUE)
  }
  
  FALSE
  
}

#' Setup wizard help - asking users to edit .Renviron
#' 
#' @param to_paste The line to paste into .Renviron
#' @param session_user whether its a 1 = user level or 2=project level .Renviron file
#' 
#' Intended to get user input from a menu, 1 indicating user level, 2 project level
#' 
#' \link{gar_setup_check_session} creates a menu for the user to choose which
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' choice <- gar_setup_check_session()
#' gar_setup_edit_renviron("ENV_ARG=blah", session_user = choice)
#' 
#' }
#' @export
#' @importFrom cli cli_ul cli_code cli_alert_success
#' @family setup functions
gar_setup_edit_renviron <- function(to_paste, session_user){
  
  if(session_user == 1){
    scope <- "user"
  } else if(session_user == 2){
    scope <- "project"
  } else {
    stop("User cancelled setup", call. = FALSE)
  }
  cli_ul("Configuring your .Renviron...")
  cli_code(to_paste)
  add_renviron(scope = scope, line = to_paste)
  cli_alert_success("Added {to_paste} to .Renviron and set via Sys.setenv()")
}

add_renviron <- function(scope = c("user", "project"), line){
  the_file <- switch(
    scope,
    user = file.path(Sys.getenv("HOME"), ".Renviron"),
    project = file.path(rstudioapi::getActiveProject(), ".Renviron")
  )
  cli_alert_info("Writing to {the_file}")
  
  if(!file.exists(the_file)){
    file.create(the_file)
  }
  
  # set it locally too
  env_set <- unlist(strsplit(line, "="))
  if(length(env_set) == 2){
    do.call(Sys.setenv, stats::setNames(list(env_set[2]),env_set[1]))
  }
  
  add_line(line, the_file)
}

#' @export
#' @rdname gar_setup_edit_renviron
#' @importFrom utils menu
gar_setup_check_session <- function(session_user = NULL){
  # its already set
  if(!is.null(session_user)){
    return(session_user)
  }
  
  local_file <- file.path(rstudioapi::getActiveProject(), ".Renviron")
  if(file.exists(local_file)){
    cli_alert_info("Using local project .Renviron")
    return(2)
  }
  
  session_user <- menu(
    title = "Do you want to configure for all R sessions or just this project?",
    choices = c("All R sessions (Recommended)", "Project only"))
  if(session_user == 2){
    if(!file.exists(local_file)){
      file.create(local_file)
      cli_alert_success(
        "Created local .Renviron file - please restart R then rerun setup.")
      stop("Need R restart", call. = FALSE)
    }
  }
  cli_rule()
  
  session_user
}