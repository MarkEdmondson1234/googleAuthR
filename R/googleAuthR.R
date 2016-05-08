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
#' @section Default options:
#' 
#' These are the default options that you can override via \code{options()}
#' 
#' \itemize{
#'   \item \code{googleAuthR.rawResponse = FALSE}
#'   \item \code{googleAuthR.httr_oauth_cache = TRUE}
#'   \item \code{googleAuthR.verbose = 3}
#'   \item \code{googleAuthR.client_id = "201908948134-rm1ij8ursrfcbkv9koc0aqver84b04r7.apps.googleusercontent.com"}
#'   \item \code{googleAuthR.client_secret = "nksRJZ5K3nm9FUWsAtBoBArz"}
#'   \item \code{googleAuthR.webapp.client_id = "201908948134-cjjs89cffh3k429vi7943ftpk3jg36ed.apps.googleusercontent.com"}
#'   \item \code{googleAuthR.webapp.client_secret = "mE7rHl0-iNtzyI1MQia-mg1o"}
#'   \item \code{googleAuthR.webapp.port = 1221}
#'   \item \code{googleAuthR.jsonlite.simplifyVector = TRUE}
#'   \item \code{googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/webmasters",
#'                                       "https://www.googleapis.com/auth/analytics",
#'                                       "https://www.googleapis.com/auth/analytics.readonly",
#'                                       "https://www.googleapis.com/auth/analytics.manage.users.readonly",
#'                                       "https://www.googleapis.com/auth/tagmanager.readonly",
#'                                       "https://www.googleapis.com/auth/urlshortener")}
#'   \item \code{googleAuthR.ok_content_types=c("application/json; charset=UTF-8", ("text/html; charset=UTF-8"))}
#'   \item \code{googleAuthR.securitycode = paste0(sample(c(1:9, LETTERS, letters), 20, replace = T), collapse='')}
#'   \item \code{googleAuthR.tryAttempts = 5}
#'  }
#'
#' 
#' 
#' 
#' @docType package
#' @name googleAuthR
#' 
NULL