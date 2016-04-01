#' Customer message log level
#' 
#' @param ... The message(s)
#' @param level The severity
#' 
#' @details 0 = everything, 1 = debug, 2=normal, 3=important
myMessage <- function(..., level = 2){
  
  compare_level <- getOption("googleAuthR.verbose")
  
  if(level >= compare_level){
    message(...)
  }

}

#' split a vector on an element
#' 
#' Why doesn't this exist?
#' @param vector a vector
#' @param index where to split the vector
#' @param remove_splits to return with or without the index points
#' 
#' @return a list of vectors split not including split point
#' @keywords internal
split_vector <- function(vector, index, remove_splits=TRUE){
  
  l <- list()
  for(i in 2:length(index)){
    if(is.na(index[i])){
      warning("No index found")
      return(c("{", '"error":"no index"', "}"))
    }
    s <- vector[index[i-1]:index[i]] 
    
    if(remove_splits){
      s <- s[-1]
      s <- s[-length(s)]
    }
    
    # remove leading or trailing empty strings
    if(s[1] == "") s <- s[-1]
    if(s[length(s)] == "") s <- s[-length(s)]
    
    if(exists("l")){
      l <- c(l, list(s))
    } else {
      l <- list(s)
    }
  }
  l
}


#' A helper function that tests whether an object is either NULL _or_
#' a list of NULLs
#'
#' @keywords internal
is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))

#' Recursively step down into list, removing all such objects
#'
#' @keywords internal
rmNullObs <- function(x) {
  x <- Filter(Negate(is.NullOb), x)
  lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
}


#' Substitute in a (nested) list
#' 
#' @param template A template named list
#' @param replace_me A similar named list with different values to substitute
#' 
#' @return The template with the values substituted.
#' @keywords internal
#' If replace_me has list names not in template, the value stays the same.
substitute.list <- function(template, replace_me){
  
  ## remove possible NULL entries
  template <- rmNullObs(template)
  replace_me <- rmNullObs(replace_me)

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

#' Get the error message
#'
#' @param test_me an object that has failed is.error
#'
#' @return The error message
#'
#' @keywords internal
error.message <- function(test_me){
  if(is.error(test_me)) attr(test_me, "condition")$message
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