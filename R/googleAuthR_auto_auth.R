#' Perform auto authentication
#' 
#' This helper function lets you use environment variables to auto-authenticate on package load, intended for calling by \link{gar_attach_auto_auth}
#' 
#' @param new_user If TRUE, reauthenticate via Google login screen
#' @param no_auto If TRUE, ignore auto-authentication settings
#' @param required_scopes Required scopes needed to authenticate - needs to match at least one
#' @param environment_var Name of environment var that contains auth file path
#' @param travis_environment_var Name of Travis environment var that contains auth file path
#' 
#' The authentication file can be a \code{.httr-oauth} file created via \link{gar_auth} or a Google service JSON file downloaded from the Google API crudential console, with file extension \code{.json}.
#' 
#' You can use this in your code to authenticate from a filelocation specified in in file, but it is mainly intended to be called on package load via \link{gar_attach_auto_auth}.
#' 
#' 
#' \code{environment_var} This is the name that will be called via \link{Sys.getenv} on library load.  The environment variable will contain an absolute file path to the location of an authentication file.
#' 
#' \code{travis_environment_var} Name that will be called via \link{Sys.getenv} in Travis tests.  
#'   As the working directory is different on travis, 
#'   this environment variable should contain a relative path to your Github repository home folder.
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
gar_auto_auth <- function(required_scopes,
                          new_user = FALSE, 
                          no_auto = FALSE,
                          environment_var = "GAR_AUTH_FILE",
                          travis_environment_var = "TRAVIS_GAR_AUTH_FILE"){
  
  testthat::expect_type(required_scopes, "character")
  testthat::expect_type(environment_var, "character")
  testthat::expect_length(environment_var, 1)
  testthat::expect_type(travis_environment_var, "character")
  testthat::expect_length(travis_environment_var, 1)
  
  if(!any(getOption("googleAuthR.scopes.selected") %in% required_scopes)){
    stop("Cannot authenticate - options(googleAuthR.scopes.selected) needs to be set to include", 
         paste(required_scopes, collapse = " or "))
  }
  
  if(no_auto){
    return(invisible(gar_auth(new_user = new_user)))
  }
  
  ## Travis checks are relative file paths to getwd()
  if(Sys.getenv(travis_environment_var) != ""){
    cat("\nAuthentication on travis with ", travis_environment_var)
    auth_file <- Sys.getenv(travis_environment_var)
    auth_file <- file.path(getwd(), auth_file)
    cat("\nAuth file:", auth_file,"\n")
  } else {
    auth_file <- Sys.getenv(environment_var)
  }
  
  if(auth_file == ""){
    ## normal auth looking for .httr-oauth in working folder or new user
    out <- gar_auth(new_user = new_user)
  } else {
    ## auth_file specified in environment_var
    if(file.exists(auth_file)){
      ## Service JSON file
      if(tools::file_ext(auth_file) == "json"){
        myMessage("Auto-auth - json")
        out <- gar_auth_service(auth_file)
      } else {
        ## .httr-oauth file
        cat("\nAuto-auth - .httr-oauth\n")
        token <- readRDS(auth_file)

        out <- gar_auth(token = token[[1]])

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
#' @param environment_var The name of the environment variable where the filepath to the authentication file is kept
#' @param travis_environment_var Name of Travis environment var that contains auth file path
#' 
#' This function works with \link{gar_auto_auth}.  It is intended to be placed within the \link{.onAttach} hook so that it loads when you load your library.
#' 
#' For auto-authentication to work, the environment variable needs to hold a file path to an existing auth file such as created via \link{gar_auth} or a JSON file file download from the Google API console.
#' 
#' @section Travis:
#' 
#' If you are using Travis to make tests, then a specific environment name for a Travis auth file is also needed, that should be relative to the home directory of your Github repository.  
#' 
#' You should then also encrypt the auth file and include the encrypted file in your Github repository.  See \href{https://cran.r-project.org/web/packages/googlesheets/vignettes/managing-auth-tokens.html#tokens-for-testing}{googlesheets vignette on managing auth tokens} and \href{https://docs.travis-ci.com/user/encrypting-files/}{Travis encrypting files how-to} for background.
#' 
#' To work on travis, you will need one auth token for the library load in the home folder, and another in the \code{testthat} folder.  You can achieve this by using the same encrypted file command twice within your \code{.travis.yml} configuration, writing out to the two different folders:
#' 
#' \code{openssl aes-256-cbc -K $encrypted_0a6446eb3ae3_key -iv $encrypted_0a6446eb3ae3_key -in auth.json.enc -out auth.json -d}
#' \code{openssl aes-256-cbc -K $encrypted_0a6446eb3ae3_key -iv $encrypted_0a6446eb3ae3_key -in auth.json.enc -out tests/testthat/auth.json -d}
#' 
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
gar_attach_auto_auth <- function(required_scopes,
                                 environment_var = "GAR_AUTH_FILE",
                                 travis_environment_var = "TRAVIS_GAR_AUTH_FILE"){
  
  testthat::expect_type(required_scopes, "character")
  testthat::expect_type(environment_var, "character")
  testthat::expect_length(environment_var, 1)
  
  scopes <- getOption("googleAuthR.scopes.selected")
  if(any(!(required_scopes %in% scopes))){
    packageStartupMessage("Setting scopes to ", paste(required_scopes, collapse = " and "))
    packageStartupMessage("If you need additional scopes set do so via options(googleAuthR.scopes.selected = c('scope1', 'scope2')) before loading library and include one required scope.")
    new_scopes <- required_scopes
  } else {
    new_scopes <- scopes
  }
  
  options(googleAuthR.scopes.selected = new_scopes)
  
  if(Sys.getenv(environment_var) != ""){
    
    googleAuthR::gar_auto_auth(required_scopes = required_scopes,
                               environment_var = environment_var,
                               travis_environment_var = travis_environment_var)
    
    packageStartupMessage("Successfully authenticated via ", Sys.getenv(environment_var))
  }
  
  invisible()
  
}
