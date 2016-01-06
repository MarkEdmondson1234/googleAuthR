.onLoad <- function(libname, pkgname) {
  
  op <- options()
  op.googleAuthR <- list(
    googleAuthR.httr_oauth_cache = TRUE,
    googleAuthR.client_id = "201908948134-rm1ij8ursrfcbkv9koc0aqver84b04r7.apps.googleusercontent.com",
    googleAuthR.client_secret = "nksRJZ5K3nm9FUWsAtBoBArz",
    googleAuthR.webapp.client_id = "201908948134-cjjs89cffh3k429vi7943ftpk3jg36ed.apps.googleusercontent.com",
    googleAuthR.webapp.client_secret = "mE7rHl0-iNtzyI1MQia-mg1o",
    googleAuthR.webapp.port = 1221,
    googleAuthR.jsonlite.simplifyVector = TRUE,
    googleAuthR.scopes = c("https://www.googleapis.com/auth/webmasters",
                           "https://www.googleapis.com/auth/analytics",
                           "https://www.googleapis.com/auth/analytics.readonly",
                           "https://www.googleapis.com/auth/analytics.manage.users.readonly",
                           "https://spreadsheets.google.com/feeds",
                           "https://docs.google.com/feeds",
                           "https://www.googleapis.com/auth/tagmanager.readonly",
                           "https://www.googleapis.com/auth/tagmanager.edit.containers",
                           "https://www.googleapis.com/auth/tagmanager.delete.containers",
                           "https://www.googleapis.com/auth/tagmanager.edit.containerversions",
                           "https://www.googleapis.com/auth/tagmanager.publish",
                           "https://www.googleapis.com/auth/tagmanager.manage.users",
                           "https://www.googleapis.com/auth/tagmanager.manage.accounts",
                           "https://www.googleapis.com/auth/urlshortener",
                           "https://www.googleapis.com/auth/yt-analytics.readonly"),
    googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/webmasters",
                                    "https://www.googleapis.com/auth/analytics",
                                    "https://www.googleapis.com/auth/analytics.readonly",
                                    "https://www.googleapis.com/auth/analytics.manage.users.readonly",
                                    "https://www.googleapis.com/auth/tagmanager.readonly",
                                    "https://www.googleapis.com/auth/urlshortener"),
    googleAuthR.ok_content_types=c("application/json; charset=UTF-8", ("text/html; charset=UTF-8")),
    googleAuthR.securitycode = 
      paste0(sample(c(1:9, LETTERS, letters), 20, replace = T), collapse=''),
    googleAuthR.tryAttempts = 5
  )
  toset <- !(names(op.googleAuthR) %in% names(op))
  if(any(toset)) options(op.googleAuthR[toset])
  
  invisible()
  
}