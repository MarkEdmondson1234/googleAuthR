#' Substitute in a (nested) list
#' 
#' @param template A template named list
#' @param replace_me A similar named list with different values to substitute
#' 
#' @return The template with the values substituted.
#' @keywords internal
#' If replace_me has list names not in template, the value stays the same.
substitute.list <- function(template, replace_me){

  postwalk(template, function(x) replace.kv(x,replace_me))
  
}

#' Walk into a list
#' 
#' If passed an object such as a nested list, will apply function
#'   on inner elements that are not lists.
#' 
#' @param x what to check
#' @param func Function to apply if not a list
#' @keywords internal 
#' @return the function acting on x or an inner element of x
postwalk <- function(x,func){
  if(is.list(x)){
    func(lapply(x,postwalk,func))
  } else {
    func(x)
    }
}

#' Create a modified list
#' 
#' @param template a (nested) list with elements to replace
#' @param replace a subset of template with same names but replacement values
#' @keywords internal
#' @return a list like template but with values replace from replace
replace.kv <- function(template,replace) {
  if(!is.list(template)) return(template)
  
  i <- match(names(template),names(replace))
  w <- which(!is.na(i))
  
  replace(template,w,replace[i[w]])
  
}



#' Is this a valid shiny session object?
#' 
#' Checks that a valid Shiny session object has been passed.
#' 
#' @param shiny_session a Shiny session object.
#' 
#' @return Boolean
#' 
#' @keywords internal
is_shiny <- function(shiny_session){
  inherits(shiny_session, "ShinySession")
}

#' Is this a try error?
#' 
#' Utility to test errors
#' 
#' @param test_me an object created with try()
#' 
#' @return Boolean
#' 
#' @keywords internal
is.error <- function(test_me){
  inherits(test_me, "try-error")
}

#' Checks Urls are in right format for API request
#' 
#' @param url The URL to check for use in API request
#' @param checkProtocol Check if URL starts with 'http'
#' @param ... Passed to URLencode()
#' 
#' @return Modified url if successful, raises an error if not.
#' 
#' @keywords internal
check.Url <- function(url, checkProtocol=TRUE){
  
  if(!is.null(url)){
    if(checkProtocol && !grepl("http",url)){
      stop("URL must include protocol, e.g. http://example.com - got:", url) 
    }
    
    url <- utils::URLencode(url, reserved = TRUE)
    message("Encoding URL to ", url)

    url    
    
  } else {
    stop("Null URL")
  }
}

#' Converts RFC3339 to as.Date
#' 
#' @keywords internal
RFC_convert <- function(RFC, drop_time=FALSE){
  
  if(drop_time){
    d <-   as.Date(strptime(as.character(RFC), 
                            tz="UTC", 
                            "%Y-%m-%dT%H:%M:%OSZ"))
  } else {
    d <- strptime(as.character(RFC), 
                  tz="UTC", 
                  "%Y-%m-%dT%H:%M:%OSZ")
  }
  
  return(d)
}