.onLoad <- function(libname, pkgname) {
  
  op <- options()
  op.googleAuthR <- list(
    googleAuthR.rawResponse = FALSE,
    googleAuthR.httr_oauth_cache = TRUE,
    googleAuthR.verbose = 3,
    googleAuthR.cache_function = function(req) {TRUE},
    googleAuthR.client_id = Sys.getenv("GAR_CLIENTID"),
    googleAuthR.client_secret = Sys.getenv("GAR_CLIENT_SECRET"),
    googleAuthR.webapp.client_id = Sys.getenv("GAR_WEB_CLIENTID"),
    googleAuthR.webapp.client_secret = Sys.getenv("GAR_WEB_CLIENT_SECRET"),
    googleAuthR.scopes.selected = Sys.getenv("GAR_SCOPES"),
    googleAuthR.webapp.port = 1221,
    googleAuthR.jsonlite.simplifyVector = TRUE,
    googleAuthR.ok_content_types=c("application/json; charset=UTF-8", "text/html; charset=UTF-8"),
    googleAuthR.securitycode = 
      paste0(sample(c(1:9, LETTERS, letters), 20, replace = T), collapse=''),
    googleAuthR.tryAttempts = 5
  )
  toset <- !(names(op.googleAuthR) %in% names(op))
  if(any(toset)) options(op.googleAuthR[toset])
  
  invisible()
  
}

.onAttach <- function(libname, pkgname) {
 
  default_scopes <- getOption("googleAuthR.scopes.selected")
  
  googleAuthR::gar_attach_auto_auth(default_scopes)
  
}