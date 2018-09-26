.onLoad <- function(libname, pkgname) {
  
  sys_or_null <- function(x){
    sys <- Sys.getenv(x)
    if (sys == "") return(NULL)
    sys
  }
  
  scopes_split <- function(x){
    sys <- sys_or_null("GAR_SCOPES")
    if (is.null(sys)) return(NULL)
    strsplit(sys, 
             split = ",", fixed = TRUE)[[1]]
  }
  op <- options()
  op.googleAuthR <- list(
    googleAuthR.batch_endpoint = "https://www.googleapis.com/batch",
    googleAuthR.rawResponse = FALSE,
    googleAuthR.httr_oauth_cache = ".httr-oauth",
    googleAuthR.verbose = 3,
    googleAuthR.client_id = sys_or_null("GAR_CLIENTID"),
    googleAuthR.client_secret = sys_or_null("GAR_CLIENT_SECRET"),
    googleAuthR.webapp.client_id = sys_or_null("GAR_WEB_CLIENTID"),
    googleAuthR.webapp.client_secret = sys_or_null("GAR_WEB_CLIENT_SECRET"),
    googleAuthR.scopes.selected = scopes_split("GAR_SCOPES"),
    googleAuthR.webapp.port = 1221,
    googleAuthR.jsonlite.simplifyVector = TRUE,
    googleAuthR.ok_content_types = c("application/json; charset=UTF-8", "text/html; charset=UTF-8"),
    googleAuthR.securitycode = 
      paste0(sample(c(1:9, LETTERS, letters), 20, replace = T), collapse = ''),
    googleAuthR.tryAttempts = 5,
    googleAuthR.HttrRetryTimes = 3,
    googleAuthR.HttrRetryTerminateOn = NULL
  )
  toset <- !(names(op.googleAuthR) %in% names(op))
  if (any(toset)) options(op.googleAuthR[toset])
  
  invisible()
  
}

.onAttach <- function(libname, pkgname) {
 
  default_scopes <- getOption("googleAuthR.scopes.selected")
  
  googleAuthR::gar_attach_auto_auth(default_scopes)
  
}