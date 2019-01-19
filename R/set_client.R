#' Setup the clientId, clientSecret and scopes
#' 
#' Help setup the client ID and secret with the OAuth 2.0 clientID.  
#'   Do not confuse with Service account keys.
#' 
#' @param json The file location of an OAuth 2.0 client ID json file
#' @param web_json The file location of client ID json file for web applications
#' @param scopes A character vector of scopes to set
#' 
#' @details 
#' 
#' This function helps set the \code{options(googleAuthR.client_id)}, 
#'   \code{options(googleAuthR.client_secret)} and 
#'   \code{options(googleAuthR.scopes.selected)} for you.
#' 
#' You can also set the web application client IDs that are used in Shiny authentication, 
#'   that are set via the options \code{options(googleAuthR.webapp.client_id)}, 
#'   \code{options(googleAuthR.webapp.client_secret)}
#'   
#' Note that if you authenticate with a cache token with different values it 
#'   will overwrite them.
#' 
#' For successful authentication, the API scopes can be browsed via the 
#'   googleAuthR RStudio addin or the Google API documentation.
#' 
#' Do not confuse this JSON file with the service account keys, that are
#'   used to authenticate a service email.  This JSON only sets up which
#'   app you are going to authenticate with - use \link{gar_auth_service} with
#'   the Service account keys JSON to perform the actual authentication. 
#'   
#' By default the JSON file will be looked for in the location specified by the
#'   \code{"GAR_CLIENT_JSON"} environment argument, or via \code{"GAR_CLIENT_WEB_JSON"} for webapps.
#' 
#' @author Idea via @jennybc and @jimhester from \code{gargle and gmailr} libraries.
#' 
#' @return The \code{project-id} the app has been set for
#' 
#' @seealso \url{https://console.cloud.google.com/apis/credentials}
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' 
#' gar_set_client("google-client.json", scopes = "http://www.googleapis.com/auth/webmasters")
#' gar_auth_service("google-service-auth.json")
#' }
#' 
#' @importFrom jsonlite fromJSON
#' @import assertthat
gar_set_client <- function(json = Sys.getenv("GAR_CLIENT_JSON"), 
                           web_json = Sys.getenv("GAR_CLIENT_WEB_JSON"),
                           scopes = NULL){
  
  if(json == "" && web_json == ""){
    stop("No client JSON files found", call. = FALSE)
  }
  
  if(json != ""){
    assert_that(is.readable(json))
    the_json <- fromJSON(json)
    if(is.null(the_json$installed)){
      stop("$installed not found in JSON - have you downloaded the correct JSON file? 
           (Service account client > Other, not Service Account Keys)", call. = FALSE)
    }
    
    options(googleAuthR.client_id = the_json$installed$client_id,
            googleAuthR.client_secret = the_json$installed$client_secret)
    
    project_id <- the_json$installed$project_id
    
  }

  
  ## web apps
  if(web_json != ""){
    assert_that(is.readable(web_json))
    web_json <- fromJSON(web_json)
    
    if(is.null(web_json$web)){
      stop("$web not found in JSON - have you downloaded the corret JSON file for web apps?
           (Service account client > Web Application, not Service Account Keys or Other)", 
           call. = FALSE)
    }
    
    options(googleAuthR.webapp.client_id = web_json$web$client_id,
            googleAuthR.webapp.client_secret = web_json$web$client_secret)
    
    # fix for shinyapps #57 etc
    Sys.setenv("GAR_WEB_CLIENTID" = web_json$web$client_id)
    Sys.setenv("GAR_WEB_CLIENT_SECRET" = web_json$web$client_secret)
    Sys.setenv("GAR_SCOPES" = paste(scopes, collapse = ","))
    
    project_id <- web_json$web$project_id
    
  }
  
  if(web_json != "" && json != ""){
    if(web_json$web$project_id != the_json$installed$project_id){
      warning("Web and offline projects don't match:", 
              "Web:", web_json$web$project_id, 
              "Installed:", the_json$installed$project_id, 
              call. = FALSE)
    }
  }
  
  if(!is.null(scopes)){
    assert_that(is.character(scopes))
    options(googleAuthR.scopes.selected = scopes)
  }
  
  myMessage("\noptions(googleAuthR.scopes.selected=c('",
            paste(getOption("googleAuthR.scopes.selected"), collapse = "','"),"'))",
            "\noptions(googleAuthR.client_id='", getOption("googleAuthR.client_id"),"')",
            "\noptions(googleAuthR.client_secret=' ", getOption("googleAuthR.client_secret"),"')", 
            "\noptions(googleAuthR.webapp.client_id='", getOption("googleAuthR.webapp.client_id"),"')",
            "\noptions(googleAuthR.webapp.client_secret=' ", getOption("googleAuthR.webapp.client_secret"),"')", 
            level = 2)
  
  project_id
  
}
