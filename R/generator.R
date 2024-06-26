#' googleAuthR data fetch function generator
#'
#' This function generates other functions for use with Google APIs
#'
#' @param baseURI The stem of the API call.
#' @param http_header Type of http request.
#' @param path_args A named list with name=folder in request URI, value=the function variable.
#' @param pars_args A named list with name=parameter in request URI, value=the function variable.
#' @param data_parse_function A function that takes a request response, parses it and returns the data you need.
#' @param customConfig list of httr options such as \link[httr]{use_proxy}
#'   or \link[httr]{add_headers} that will be added to the request.
#' @param simplifyVector Passed to \link[jsonlite]{fromJSON} for response parsing
#' @param checkTrailingSlash Default TRUE will append a trailing slash to baseURI if missing
#'
#' @details
#' \strong{path_args} and \strong{pars_args} add default values to the baseURI.
#'   NULL entries are removed. Use "" if you want an empty argument.
#'
#' You don't need to supply access_token for OAuth2 requests in pars_args,
#'   this is dealt with in gar_auth()
#'
#' Add custom configurations to the request in this syntax:
#'  \code{customConfig = list(httr::add_headers("From" = "mark@example.com")}
#'
#' @examples
#' \dontrun{
#' library(googleAuthR)
#' ## change the native googleAuthR scopes to the one needed.
#' options("googleAuthR.scopes.selected" = "email")
#'
#' get_email <- function(){
#'    f <- gar_api_generator("https://openidconnect.googleapis.com/v1/userinfo",
#'                           "POST",
#'                           data_parse_function = function(x) x$email,
#'                           checkTrailingSlash = FALSE)
#'                           
#'    f()
#'    }
#'
#' To use the above functions:
#' library(googleAuthR)
#' # go through authentication flow
#' gar_auth()
#' s <- get_email()
#' s
#' }
#'
#'
#' @return A function that can fetch the Google API data you specify
#' @export
#' @import assertthat
#' @importFrom httr set_config config
#' @importFrom digest digest
#' @importFrom utils URLencode
#' @importFrom assertthat assert_that is.string
gar_api_generator <- function(baseURI,
                              http_header = c("GET","POST","PUT","DELETE", "PATCH"),
                              path_args = NULL,
                              pars_args = NULL,
                              data_parse_function=NULL,
                              customConfig=NULL,
                              simplifyVector=getOption("googleAuthR.jsonlite.simplifyVector"),
                              checkTrailingSlash = TRUE){

  assert_that(
    is.string(baseURI),
    is.logical(checkTrailingSlash),
    is.logical(simplifyVector)
  )
  
  ## temp fix until Google severs support HTTP2 properly
  set_config(config(http_version = 0))

  http_header <- match.arg(http_header)

  if(checkTrailingSlash){
    if(substr(baseURI,nchar(baseURI),nchar(baseURI))!="/") {
      myMessage("No trailing slash in URL, adding it.", level = 1)
      baseURI <- paste0(baseURI, "/")
    }
  }

  path <- NULL
  pars <- NULL

  if(!is.null(path_args)) {
    path <- paste(names(path_args), path_args, sep="/", collapse="/" )
  }

  if(!is.null(pars_args)){
    ## it has to be a character for pars
    pars_args <- vapply(pars_args, as.character, character(1))
    pars <-
      paste(names(pars_args), 
            vapply(pars_args, URLencode, reserved = TRUE, character(1)), 
            sep='=', collapse='&')    

  }

  func <- function(path_arguments=NULL,
                   pars_arguments=NULL,
                   the_body=NULL,
                   batch=FALSE,
                   url_override=NULL,
                   ...){

    # extract the shiny token from the right environments
    ## gets the environments
    all_envs <- as.character(sys.calls())

    if(!checkTokenAPI()){
      stop("Invalid token", call. = FALSE)
    }

    ## for path_arguments present, change path_args
    if(!is.null(path_arguments)){
      path_args <- substitute.list(path_args, path_arguments)
      path <- paste(names(path_args), path_args, sep="/", collapse="/" )
    }

    ## for pars_arguments present, change pars_args
    if(!is.null(pars_arguments)){
      pars_args <- substitute.list(pars_args, pars_arguments)
      pars <- paste(names(pars_args), pars_args, sep='=', collapse='&')
    }

    if(!is.null(pars_args)){
      pars <- paste0("?", pars)
    }
    
    # change the URL for the API request
    if(!is.null(url_override)){
      req_url <- url_override
    } else {
      req_url <- paste0(baseURI, path, pars)
    }

    ## if called with gar_batch wrapper set batch to TRUE
    with_gar_batch <- which(grepl("gar_batch", all_envs))
    if(any(with_gar_batch)){
      ## batching
      req <- list(req_url = req_url,
                  http_header = http_header,
                  the_body = the_body,
                  name = digest(c(req_url, the_body)))

      if(!is.null(data_parse_function)){
        req <- c(req, data_parse_function = data_parse_function)
      }
      ## early return for batching
      return(req)
    }

    myMessage("Request: ", req_url, level = 2)

    cached_call <- !is.null(gar_cache_get_loc())
    if(cached_call){
      req <- memDoHttrRequest(req_url,
                              request_type=http_header,
                              the_body=the_body,
                              customConfig=customConfig,
                              simplifyVector=simplifyVector)

    } else {
      req <- doHttrRequest(req_url,
                           request_type=http_header,
                           the_body=the_body,
                           customConfig=customConfig,
                           simplifyVector=simplifyVector)
    }


    if(!is.null(data_parse_function)){
      reqtry <- try(data_parse_function(req$content, ...))
      if(any(is.error(reqtry), is.null(reqtry))){
        
        error_object <- structure(list(request = list(req_url = req_url,
                                            request_type = http_header,
                                            the_body = the_body,
                                            customConfig=customConfig),
                             response = list(data_parse_args = list(...),
                                             data_parse_func = data_parse_function,
                                             content = req$content),
                             authentication = list(
                               token = .auth$cred
                             )),
                             class = "gar_parse_error")
        
        saveRDS(error_object, file = "gar_parse_error.rds")
        stop("API Data failed to parse.  
             Wrote diagnostic object to 'gar_parse_error.rds', use googleAuthR::gar_debug_parsing('gar_parse_error.rds') to 
             debug the data_parse_function.", call. = FALSE)
      } else {
        req <- reqtry
      }
    }

    req

  }
  ##returns a function that can call the API
  
  structure(func, class = c("gar_function","function"))

}

is.gar_function <- function(x){
  inherits(x, "gar_function")
}




#' ReTry API requests for certain errors using exponential backoff.
#'
#' @param f A function of a http request
#'
#' @keywords internal
#' @importFrom httr with_verbose content
#' @importFrom jsonlite fromJSON
#' @importFrom utils browseURL
retryRequest <- function(f){

  verbose <- getOption("googleAuthR.verbose")

  if(verbose <= 1){
    the_request <- try(with_verbose(f))
  } else {
    the_request <- try(f)
  }

  if(is.error(the_request)){
    stop("Request failed before finding status code: ", 
         error.message(the_request), call. = FALSE)
  } else {
    status_code <- as.character(the_request$status_code)
  }

  if(!(grepl("^20",status_code))){
    myMessage("Request Status Code: ", status_code, level = 3)

    content <- content(the_request, as = "text", 
                       type = "application/json", encoding = "UTF-8")
    
    content <- tryCatch(fromJSON(content), error = function(err){
      myMessage("Could not parase error content to JSON", level = 2)
      return(content)
    })
    
    if(!is.string(content) && !is.null(content$error$message)){
      error <- content$error$message
    } else {
      error <- "Unspecified error"
    }

    myMessage("API error: ",paste(error), level = 2)

    if(grepl("^5|429|408",status_code)){
      try_attempts <- getOption("googleAuthR.tryAttempts")
      for(i in 1:try_attempts){
        myMessage("Trying again: ", i, " of ", try_attempts, level = 3)
        Sys.sleep((2 ^ i) + stats::runif(n = 1, min = 0, max = 1))
        the_request <- try(f)
        status_code <- as.character(the_request$status_code)
        if(grepl("^20",status_code)) break
      }
      myMessage("All attempts failed.", level = 3)
    } else {
      myMessage("No retry attempted: ", error, level = 2)
    }

  }

  ## either reraise the error or should be good now
  if(is.error(the_request)){
    abort_http(status_code, error.message(the_request))
  }
  
  # if not a 2XX response make a custom error
  if(!grepl("^2", status_code)){
    abort_http(status_code, error)
  }

  the_request
}

#' Get URL content based on if its Shiny or local
#'
#' @description
#' This changes the auth type depending on if its local or on Shiny
#'
#' @param url the url of the page to retrieve
#' @param request_type the type of httr request function: GET, POST, PUT, DELETE etc.
#' @param the_body body of POST request
#' @param customConfig list of httr options such as \code{httr::use_proxy}
#'   or \code{httr::add_headers} that will be added to the request.
#' @param simplifyVector Passed to jsonlite::fromJSON
#'
#' @details Example of params: c(param1="foo", param2="bar")
#'
#' @importFrom utils packageVersion
#' @importFrom httr add_headers user_agent content
#' @importFrom jsonlite fromJSON
#' @keywords internal
doHttrRequest <- function(url,
                          request_type="GET",
                          the_body=NULL,
                          customConfig=NULL,
                          simplifyVector=getOption("googleAuthR.jsonlite.simplifyVector")){

  
  arg_list <- list(
    verb = request_type,
    url = url,
    config = get_google_token(),
    body = the_body,
    encode = if (!is.null(customConfig$encode)) customConfig$encode else "json",
    add_headers("Accept-Encoding" = "gzip"),
    user_agent(paste0("googleAuthR/",
                     packageVersion("googleAuthR"),
                     " (gzip)")),
    times = getOption("googleAuthR.HttrRetryTimes"),
    terminate_on = getOption("googleAuthR.HttrRetryTerminateOn")
  )

  arg_list <- modify_custom_config(arg_list, customConfig = customConfig)

  check_body(arg_list, the_body, request_type)

  ## httr's retry, and googleAuthR's
  req <- retryRequest(do.call("RETRY",
                              args = arg_list,
                              envir = asNamespace("httr")))

  ## do we parse or return raw response
  if(getOption("googleAuthR.rawResponse")){
    myMessage("No checks on content due to option googleAuthR.rawResponse,
              returning raw", level=2)
    return(req)
  }

  ## will raise error if checks not passed
  good_call <- checkGoogleAPIError(req)

  if(good_call){
    content <- content(req,
                       as = "text",
                       type = "application/json",
                       encoding = "UTF-8")
    
    content <- fromJSON(content, simplifyVector = simplifyVector)
    
    req$content <- content

  } else {
    warning("API checks failed, returning request without JSON parsing")
  }

  req
}


check_body <- function(arg_list, the_body, request_type){

  if(!is.null(the_body) && arg_list$encode == "json"){

    tryCatch({
      myMessage("Body JSON parsed to: ",
                jsonlite::toJSON(the_body, auto_unbox=T),
                level = 2)
    }, error = function(ex){
      myMessage("Could not parse body JSON", level = 2)
    })

    ## if verbose = 0 then write the JSON body to a file
    if(getOption("googleAuthR.verbose") == 0){
      class(the_body) <- c(class(the_body), "list")
      write_out <- list(url = arg_list$url,
                        request_type = request_type,
                        body_json = jsonlite::toJSON(the_body, auto_unbox=T))
      saveRDS(write_out, file = "request_debug.rds")
      myMessage("Written url, request_type and body_json to file 'request_debug.rds'.
                Use readRDS('request_debug.rds') to see it. ", level = 1)
    }
  }

}

modify_custom_config <- function(arg_list, customConfig){
  if(!is.null(customConfig)){
    assertthat::assert_that(
      is.list(customConfig)
    )
    ## fix bug where unnamed customConfigs were ignored
    ## encode is only named customConfig that has an effect
    if(!is.null(names(customConfig))){
      arg_list <- c(arg_list, customConfig[names(customConfig) == ""])
    } else {
      arg_list <- c(arg_list, customConfig)
    }

  }

  arg_list
}

