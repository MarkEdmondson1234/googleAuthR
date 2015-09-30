list_websites <- function() {
  
  l <- gar_api_generator("https://www.googleapis.com/webmasters/v3/sites",
                                      "GET",
                                      data_parse_function = function(x) x$siteEntry)
  l()
}

list_websites2 <- function() {
  
  l <- gar_api_generator("https://www.googleapis.com/webmasters/v3/sites",
                                      "GET",
                                      data_parse_function = function(x) x$siteEntry)
  l()
}

source('~/dev/R/googleAuthR/R/googleAuthR_batch.R')
gar_auth()
ggg <- httr::with_verbose(gar_batch(list(list_websites(), list_websites2())))
ggg <- gar_batch(list(list_websites(), list_websites2()))
