# functions needed for pre-gargle auth compatibility

#' Reads a token from a filepath
#' 
#' Also sets the option of token cache name to the supplied filepath 
#'   "googleAuthR.httr_oauth_cache"
#' 
#' httr cache files such as .httr-oauth can hold multiple tokens for different scopes, 
#'   this only returns the first one and raises a warning if there are multiple 
#'   in the rds file
#' @noRd
#' @import assertthat
read_cache_token <- function(token_path){
  
  assert_that(is.readable(token_path))
  
  myMessage("Reading token from file path", level = 2)
  
  google_token <- tryCatch({readRDS(token_path)},
                           error = function(ex){
                             stop(sprintf("Cannot read token from alleged .rds file:\n%s ",
                                          token_path), 
                                  ex$message, 
                                  call. = FALSE)
                           })
  
  if(is.list(google_token)){
    myMessage("Multiple httr-tokens in cache ",
              token_path, ", only returning first found token", level = 2)
    google_token <- google_token[[1]]
  } else if(is.token2.0(google_token)){
    myMessage("Read token successfully from file", level = 2)
  } else {
    stop("Unknown object read from ", token_path, " of class ", class(google_token))
  }
  
  ## for existing tokens, set the options to what is in the token
  overwrite_options(google_token, token_path = token_path)


}

overwrite_options <- function(google_token, token_path){
  options("googleAuthR.httr_oauth_cache" = token_path)
  google_token$cache_path <- token_path
  
  if(is.different(google_token$params$scope, "googleAuthR.scopes.selected")){
    myMessage("Overwriting googleAuthR.scopes.selected from ", getOption("googleAuthR.scopes.selected"),
              "to ", paste(google_token$params$scope, collapse = " "), level = 2)
    options("googleAuthR.scopes.selected" = google_token$params$scope)
  }
  
  if(is.null(google_token$app)){
    myMessage("No client_id in token, authentication from JSON key file", level = 2)
    return(google_token)
  }
  
  if(is.different(google_token$app$key, "googleAuthR.client_id")){
    myMessage("Overwriting googleAuthR.client_id from", getOption("googleAuthR.client_id"),
              "to ", google_token$app$key, level = 2)
    options("googleAuthR.client_id" = google_token$app$key)
  }
  
  if(is.different(google_token$app$secret, "googleAuthR.client_secret")){
    myMessage("Overwriting googleAuthR.client_secret to ", google_token$app$secret, level = 2)
    options("googleAuthR.client_secret" = google_token$app$secret)
  }
  
  google_token
  
}
