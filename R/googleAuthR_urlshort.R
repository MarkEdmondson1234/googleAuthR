options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/urlshortener"))

#' Shortens a url using goo.gl
#'
#' @param url URl to shorten with goo.gl
#' 
#' @return a string of the short URL
#'
#' @export
shorten_url <- function(url){
  
  body = list(
    longUrl = url
  )
  
  f <- googleAuth_fetch_generator("https://www.googleapis.com/urlshortener/v1/url",
                             "POST",
                             data_parse_function = function(x) x$id)
  
  f(the_body = body)
  
}

#' Expands a url that has used goo.gl
#'
#' @param shortUrl Url that was shortened with goo.gl
#' 
#' @return a string of the expanded URL
#'
#' @export
expand_url <- function(shortUrl){
  
  f <- googleAuth_fetch_generator("https://www.googleapis.com/urlshortener/v1/url",
                                  "GET",
                                  pars_args = list(shortUrl = "shortUrl"),
                                  data_parse_function = function(x) x)
  f(pars_arguments = list(shortUrl = shortUrl))
  
}

#' Get analyitcs of a url that has used goo.gl
#'
#' @param shortUrl Url that was shortened with goo.gl
#' @param timespan The time period for the analytics data
#' 
#' @return a dataframe of the goo.gl Url analytics
#'
#' @export
analytics_url <- function(shortUrl, timespan = c("allTime", "month","week","day","twoHours")){
  
  timespan <- match.arg(timespan)
    
  f <- googleAuth_fetch_generator("https://www.googleapis.com/urlshortener/v1/url",
                                  "GET",
                                  pars_args = list(shortUrl = "shortUrl",
                                                   projection = "FULL"),
                                  data_parse_function = function(x) { 
                                    a <- x$analytics 
                                    return(a[timespan][[1]])
                                    })
  
  f(pars_arguments = list(shortUrl = shortUrl))
}

#' Get the history of the authenticated user
#' 
#' @return a dataframe of the goo.gl user's history
#'
#' @export
user_history <- function(){
  f <- googleAuth_fetch_generator("https://www.googleapis.com/urlshortener/v1/url/history",
                                  "GET",
                                  data_parse_function = function(x) x$items)
  
  f()
}