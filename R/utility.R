# https://adv-r.hadley.nz/conditions.html
abort_http <- function(status_code, msg = NULL){
  myMessage("Custom error", status_code, msg, level = 2)
  rlang::abort(paste0("http_",status_code), 
               message = paste0("http_", status_code, " ", msg)
  )
}


isFALSE <- function(x) identical(x, FALSE) # replicate R 3.5 function 

#' @noRd
#' @param filename location of JavaScript file with %s template locations in this package's inst folder
#' @param ... The correct number of strings to be replaced into %s's locations of filename
#' @return JavaScript script tag
load_js_template <- function(filename, ...){
  f <- system.file(filename, package = "googleAuthR")
  assertthat::assert_that(assertthat::is.readable(f))
  o <- readChar(f, file.info(f)$size)
  shiny::tags$script(type="text/javascript", shiny::HTML(gsub("\n|  ","",sprintf(o, ...))))
}


# check loaded package
check_package_loaded <- function(package_name){
  if (!requireNamespace(package_name, quietly = TRUE)) {
    stop(paste0(package_name, " needed for this function to work. Please install it"),
         call. = FALSE)
  }
}



#' Custom message log level
#' 
#' @param ... The message(s)
#' @param level The severity
#' 
#' @details 0 = everything, 1 = debug, 2=normal, 3=important
#' @keywords internal
#' @noRd
#' @import cli
myMessage <- function(..., level = 2){
  
  compare_level <- getOption("googleAuthR.verbose")
  
  if(level >= compare_level){
    time <- paste(Sys.time(),">")
    mm <- paste(...)
    if(grepl("^#", mm)){
      cli_h1(mm)
    } else {
      cli_div(theme = list(span.time = list(color = "grey")))
      cli_alert_info("{.time {time}} {mm}")
      cli_end()
    }
    
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
#' @noRd
split_vector <- function(vector, index, remove_splits=TRUE){
  
  l <- list()
  for(i in 2:length(index)){
    if(is.na(index[i])){
     stop("No index found: i-", i, "for vector-", vector[[1]])
    }
    s <- vector[index[i-1]:index[i]] 
    
    if(remove_splits){
      s <- s[-1]
      s <- s[-length(s)]
    }
    
    # remove leading or trailing empty strings
    if(s[1] == "") s <- s[-1]
    if(s[length(s)] == "") s <- s[-length(s)] # bug in searchConsoleR #43
    
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
#' @noRd
is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))

#' Recursively step down into list, removing all such objects
#'
#' @keywords internal
#' @noRd
rmNullObs <- function(x) {
  x <- Filter(Negate(is.NullOb), x)
  lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
}


#' Substitute in a (nested) list
#' 
#' If replace_me has list names not in template, the value stays the same.
#' 
#' @param template A template named list
#' @param replace_me A similar named list with different values to substitute
#' 
#' @return The template with the values substituted.
#' @keywords internal
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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