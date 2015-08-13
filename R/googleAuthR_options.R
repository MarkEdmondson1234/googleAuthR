.onLoad <- function(libname, pkgname) {
  
  op <- options()
  op.googleAuthR <- list(
    googleAuthR.httr_oauth_cache = TRUE,
    googleAuthR.client_id = "858905045851-3beqpmsufml9d7v5d1pr74m9lnbueak2.apps.googleusercontent.com",
    googleAuthR.client_secret = "bnmF6C-ScpSR68knbGrHBQrS",
    googleAuthR.webapp.client_id = "858905045851-iuv6uhh34fqmkvh4rq31l7bpolskdo7h.apps.googleusercontent.com",
    googleAuthR.webapp.client_secret = "rFTWVq6oMu5ZgYd9e3sYu2tm",
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
                           "https://www.googleapis.com/auth/tagmanager.manage.accounts"),
    googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/webmasters",
                                    "https://www.googleapis.com/auth/analytics",
                                    "https://www.googleapis.com/auth/analytics.readonly",
                                    "https://www.googleapis.com/auth/analytics.manage.users.readonly",
                                    "https://www.googleapis.com/auth/tagmanager.readonly"),
    googleAuthR.ok_content_types=c("application/json; charset=UTF-8", ("text/html; charset=UTF-8")),
    googleAuthR.securitycode = 
      paste0(sample(c(1:9, LETTERS, letters), 20, replace = T), collapse=''),
    googleAuthR.valid.categories = c('authPermissions', 
                                     'manyToOneRedirect',
                                     'notFollowed',
                                     'notFound',
                                     'other',
                                     'roboted',
                                     'serverError',
                                     'soft404'),
    
    googleAuthR.valid.platforms = c('mobile',
                                    'smartphoneOnly',
                                    'web')
  )
  toset <- !(names(op.googleAuthR) %in% names(op))
  if(any(toset)) options(op.googleAuthR[toset])
  
  invisible()
  
}