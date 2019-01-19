#' Perform auto authentication
#' 
#' This helper function lets you use environment variables to auto-authenticate on package load, intended for calling by \link{gar_attach_auto_auth}
#' 
#' @param new_user If TRUE, reauthenticate via Google login screen
#' @param no_auto If TRUE, ignore auto-authentication settings
#' @param required_scopes Required scopes needed to authenticate - needs to match at least one
#' @param environment_var Name of environment var that contains auth file path
#' 
#' The authentication file can be a \code{.httr-oauth} file created via \link{gar_auth} 
#'   or a Google service JSON file downloaded from the Google API credential console, 
#'   with file extension \code{.json}.
#' 
#' You can use this in your code to authenticate from a file location specified in file, 
#'   but it is mainly intended to be called on package load via \link{gar_attach_auto_auth}.
#' 
#' 
#' \code{environment_var} This is the name that will be called via \link{Sys.getenv} on library load.  The environment variable will contain an absolute file path to the location of an authentication file.
#' 
#' @seealso 
#' 
#' Help files for \link{.onAttach}
#' 
#' @return an OAuth token object, specifically a
#'   \code{\link[=Token-class]{Token2.0}}, invisibly
#'
#' @export
#' @family authentication functions
#' @import assertthat
#' @importFrom tools file_ext
gar_auto_auth <- function(required_scopes,
                          new_user = FALSE, 
                          no_auto = FALSE,
                          environment_var = "GAR_AUTH_FILE"){
  
  if(is.null(required_scopes)){
    myMessage("No scopes have been set, set them via 
               options(googleAuthR.scopes.selected) 
              - no authentication attempted.", level = 2)
    return(NULL)
  }
  
  assert_that(
    is.character(required_scopes),
    is.string(environment_var)
  )
  
  if(!any(getOption("googleAuthR.scopes.selected") %in% required_scopes)){
    stop("Cannot authenticate - options(googleAuthR.scopes.selected) needs to be set to include", 
         paste(required_scopes, collapse = " or "))
  }
  
  if(any(no_auto, new_user)){
    return(invisible(gar_auth(new_user = new_user)))
  }
  
  auth_file <- Sys.getenv(environment_var)
  
  if(auth_file == ""){
    ## normal auth looking for .httr-oauth in working folder or new user
    out <- gar_auth(new_user = new_user)
  } else {
    ## auth_file specified in environment_var
    if(file.exists(auth_file)){
      ## Service JSON file
      if(file_ext(auth_file) == "json"){
        myMessage("Auto-auth - json", level = 2)
        out <- gar_auth_service(auth_file)
      } else {
        ## .httr-oauth file
        myMessage("Auto-auth - .httr-oauth", level = 2)

        out <- gar_auth(token = auth_file)

      }
    } else {
      ## auth_file specified but not present
      stop(environment_var, " specified in environment variables but file not found - 
           looked for ", auth_file, " and called from ", getwd())
    }
  }
  
  invisible(out)
  
}

#' Auto Authentication function for use within .onAttach
#' 
#' To be placed within \link{.onAttach} to auto load an authentication file from an environment variable.
#' 
#' @param required_scopes A character vector of minimum required scopes for this API library
#' @param environment_var The name of the environment variable where the file path to the authentication file is kept
#' 
#' This function works with \link{gar_auto_auth}.  It is intended to be placed within the \link{.onAttach} hook so that it loads when you load your library.
#' 
#' For auto-authentication to work, the environment variable needs to hold a file path to an existing auth file such as created via \link{gar_auth} or a JSON file file download from the Google API console.
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' .onAttach <- function(libname, pkgname){
#' 
#'   googleAuthR::gar_attach_auto_auth("https://www.googleapis.com/auth/urlshortener", "US_AUTH_FILE")
#' 
#' }
#' 
#' ## will only work if you have US_AUTH_FILE environment variable pointing to an auth file location
#' ## .Renviron example
#' US_AUTH_FILE="/home/mark/auth/urlshortnerauth.json"
#' 
#' }
#' 
#' @return Invisible, used for its side effects of calling auto-authentication.
#' @export
#' @family authentication functions
#' @import assertthat
gar_attach_auto_auth <- function(required_scopes,
                                 environment_var = "GAR_AUTH_FILE"){
  
  if(is.null(required_scopes)){
    myMessage("No scopes have been set, set them via 
              options(googleAuthR.scopes.selected) - 
              no authentication attempted.", level = 2)
    return(NULL)
  }
  
  if(Sys.getenv(environment_var) == ""){
    myMessage("No environment argument found, looked in ", environment_var, level = 2)
    return(NULL)
  }
  
  assert_that(
    is.character(required_scopes),
    is.string(environment_var)
  )
  
  scopes <- getOption("googleAuthR.scopes.selected")
  if(all(!(required_scopes %in% scopes))){
    packageStartupMessage("Setting scopes to ", paste(required_scopes, collapse = " and "))
    new_scopes <- required_scopes
  } else {
    new_scopes <- scopes
  }
  
  options(googleAuthR.scopes.selected = new_scopes)
  
  tryCatch({gar_auto_auth(required_scopes = required_scopes,
                          environment_var = environment_var)
    
    packageStartupMessage("Successfully auto-authenticated via ", Sys.getenv(environment_var))
  }, error = function(ex){
    packageStartupMessage("Failed! Auto-authentication via ", Sys.getenv(environment_var), 
                          " - error was: ", ex$error)
  })
  
  invisible()
  
}
