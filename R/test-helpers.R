#' Skip test if not authenticated
#' 
#' Use within tests to skip if a local authentication file isn't available through an environment variable.
#' 
#' @param env_arg The name of the environment argument pointing to the auth file
#' 
#' @export
skip_if_no_env_auth <- function(env_arg){
  local_auth <- Sys.getenv(env_arg) != ""
  
  testthat::skip_if_not(local_auth, 
                        message = paste(deparse(substitute(env_arg)), 
                                        " environment argument is not found "))
}
