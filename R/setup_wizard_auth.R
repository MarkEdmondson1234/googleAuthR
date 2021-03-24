#' Check service key works via environment argument
#' 
#' @param env_arg The authentication environment argument
#' @param scope The scope of the GCP request
#' 
#' @export
#' @family setup functions
gar_setup_auth_check <- function(
  env_arg = "GCE_AUTH_FILE", 
  scope = "https://www.googleapis.com/auth/cloud-platform"){
  tryCatch(
    gar_attach_auto_auth(scope, environment_var = env_arg),
    error = function(err){
      cli_alert_danger("{env_arg} is set but authentication not yet valid.  
                       Restart R to check auth setup.")
      stop("Authentication error", call. = FALSE)
    })
  cli_alert_success("Validated authentication in {env_arg}")
  TRUE
}

#' Check for a client JSON
#' 
#' @param session_user 1 for user level, 2 for project level, leave \code{NULL} to be prompted
#' @param client_json The environment argument to be used for client_id/secret
#' 
#' @export
#' @family setup functions
#' @return TRUE is client_id is ready, FALSE if it is not
gar_setup_clientid <- function(session_user = NULL,
                               client_json = "GAR_CLIENT_JSON"){
  
  currently_set <- Sys.getenv(client_json)
  
  if(currently_set != ""){
    valid <- validate_json(currently_set)
    if(valid){
      cli_alert_info("Using Client ID via {client_json}={currently_set}")
      cli_rule()
      return(TRUE)
    }
    
    cli_alert_warning("Invalid client ID found - reconfiguring...")
    
  }
  
  cli_alert_info("Could not find a OAuth 2.0 Client ID via {client_json}")
  
  client_id <- usethis::ui_yeah("Have you downloaded a Client ID file?",
                                yes = "Yes", no = "No")
  
  if(!client_id){
    cli_alert_warning("You must have a client ID file to proceed.")
    cli_alert_info("Download via https://console.cloud.google.com/apis/credentials/oauthclient :")
    cli_li(c("Desktop app > Name > Create >",
             "OAuth 2.0 Client IDs >",
             "Click Download Arrow to the right >",
             "Download to your computer"))
    cli_rule()
    cli_alert_info("Rerun this wizard once you have your Client ID file")
    if(usethis::ui_yeah("Open up service credentials URL?")){
      utils::browseURL("https://console.cloud.google.com/apis/credentials/oauthclient")
    }
    return(FALSE)
  }
  
  ff <- usethis::ui_yeah("Select location of your client ID file:",
                         yes = "Ready", no = "Cancel")
  
  if(!ff){
    return(FALSE)
  }
  
  json <- clean_windows(file.choose())
  valid <- validate_json(json)
  
  if(!valid){
    cli_alert_danger("ClientId JSON is not valid - downloaded wrong file?")
    return(FALSE)
  }
  
  # also sets it via Sys.setenv
  gar_setup_edit_renviron(paste0(client_json,"=",json), 
                          session_user = session_user)

  TRUE
  
}

#' Create a service account for googleCloudRunner
#'
#' This will use your Google OAuth2 user to create a suitable service account
#'
#' @param email What email to open OAuth2 with
#' @param file Where to save the authentication file
#' @param session_user 1 for user level, 2 for project level, leave \code{NULL} to be prompted
#' @param client_json The location of the env arg holding client json
#' @param roles Whether to assign roles to the service key
#' @param default_key The default name of the service key
#'
#' @return TRUE if the file is ready to be setup, FALSE if need to stop
#'
#' @export
#' @importFrom cli cli_alert_warning cli_alert_info cli_li cli_rule cli_alert_danger
#' @family setup functions
gar_setup_auth_key <- function(email = Sys.getenv("GARGLE_EMAIL"),
                           file = "googleauthr-auth-key.json",
                           session_user = NULL,
                           client_json = "GAR_CLIENT_JSON",
                           roles = NULL,
                           default_key = "googleauthr"){
  
  session_user <- gar_setup_check_session(session_user)
  
  client_done <- gar_setup_clientid(session_user, client_json = client_json)
  
  if(!client_done){
    cli_alert_danger("Need a clientId to be set before configuring further")
    return(FALSE)
  }
  
  create_service <- usethis::ui_yeah("Do you want to provision a service account for your project?",
                                     yes = "Yes, I need a service account key",
                                     no = "No, I already have one downloaded")
  
  if(!create_service){
    cli_alert_info("No service account provisioned, using existing")
    return(TRUE)
  }
  
  if(is.null(roles)){
    cli_alert_info("No roles specified to configure for service key")
  }
  
  cli_alert_info("Creating service key file - choose service account name (Push enter for default '{default_key}')")
  account_id <- readline("service account name: ")
  if(account_id == ""){
    account_id <- default_key
  }
  
  cli_alert_info("Creating service account {account_id}")
  
  gar_service_provision(
    account_id,
    roles = roles,
    json = Sys.getenv(client_json),
    file = file,
    email = email)
  
  cli_alert_success("Move {file} to a secure folder location outside of your working directory")
  moved <- usethis::ui_yeah("Have you moved the file?")
  if(!moved){
    cli_alert_danger("Beware! Authentication files can be used to compromise your GCP account. Do not check into git or otherwise share the file")
    return(FALSE)
  }
  
  cli_alert_success("Service JSON key is now created")
  cli_alert_info("Configure any authentication environment args to point at this service JSON key")
  
  TRUE
  
}

#' Setup wizard helper - add authentication file to .Renviron
#' 
#' @param ... Other arguments passed to \link{gar_setup_auth_key}
#' @param env_arg The environment argument to set
#' 
#' @return A string to paste into an .Renviron, or NULL
#' @export
#' @importFrom cli cli_alert_danger cli_alert_success
gar_setup_get_authenv <- function(env_arg = "GCE_AUTH_FILE", 
                                  ...){
  # checks if client JSON present
  auth <- gar_setup_auth_key(...)
  
  if(!auth){
    return(NULL)
  }
  
  fs <- usethis::ui_yeah("Ready to browse to the file to be used for authentication?",
                           yes = "Ready", no = "Cancel")
  if(!fs) return(NULL)
    
  auth_file <- file.choose()
  check_file <- tryCatch(jsonlite::fromJSON(auth_file),
      error = function(err){
        cli_alert_danger("Could not read JSON file: {auth_file}")
        NULL})
  
  if(!is.null(check_file$type) &&
      check_file$type == "service_account" &&
      !is.null(check_file$private_key)){
    
      cli_alert_success("Validated authentication JSON file")
      return(paste0(env_arg,"=", clean_windows(auth_file)))
  }
    
  cli_alert_danger("Checked {auth_file} and it was not a valid JSON file? Confirm file is JSON service account auth key")
  
  NULL
  
}

validate_json <- function(json){
  validated <- tryCatch(
    jsonlite::fromJSON(json),
    error = function(err){
      cli::cli_alert_danger("Could not load alleged Client ID file: {err$message}")
      return(FALSE)
    })
  if(!is.null(validated$installed$client_id)){
    cli::cli_alert_success("Validated Client ID file {json}")
    cli::cli_alert_success("Found Client ID project: {validated$installed$project_id}")
    return(TRUE)
  } else {
    cli::cli_alert_danger("Could not read details from client ID file - is it the right one?")
    return(FALSE)
  }
  
}

extract_project_number <- function(json = Sys.getenv("GAR_CLIENT_JSON")){
  gsub("^([0-9]+?)\\-(.+)\\.apps.+","\\1",jsonlite::fromJSON(json)$installed$client_id)
}

clean_windows <- function(x){
  gsub("\\\\","/",x)
}
