#' googleAuthR: Easy Authentication with Google OAuth2 APIs
#' 
#' Get a startup guide by viewing the vignette: \code{vignette("googleAuthR")}
#' 
#' There are two main functions in the googleAuthR package:
#' \itemize{
#'   \item \code{\link{gar_auth}} provides local authentication token.
#'   \item \code{\link{gar_api_generator}} A function factory for easy enabling of Google API functions.
#' }
#' 
#' @section Batching:
#' 
#' If you have many API calls, you can save a lot of time by using batching.  
#'   This takes your many calls and sends them in one POST request to \code{www.googleapis.com/batch}, 
#'   returning a list of any responses.  See \code{\link{gar_batch}} for details.
#'   
#' Another common batch task is to call the same function with one parameter changing each call.  
#'  This is supported using the \code{\link{gar_batch_walk}} function.
#' 
#' @section Shiny functions:
#' 
#' If you need Shiny authentication, then these functions work together to give a smooth authentication flow.
#' 
#' \itemize{
#'   \item \code{\link{reactiveAccessToken}} provides the Shiny authentication token.
#'   \item \code{\link{renderLogin}} Creates the login button server.R side.
#'   \item \code{\link{loginOutput}} Creates the login button ui.R side.
#'   \item \code{\link{with_shiny}} Wrap the functions you created with {\link{gar_api_generator}} 
#'       with this so you can pass the \code{\link{reactiveAccessToken}}
#' }
#' 
#' @docType package
#' @name googleAuthR
#' 
NULL