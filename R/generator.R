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
#' options("googleAuthR.scopes.selected" =
#'   c("https://www.googleapis.com/auth/urlshortener"))
#'
#' shorten_url <- function(url){
#'
#'   body = list(
#'     longUrl = url
#'     )
#'
#'   f <- gar_api_generator("https://www.googleapis.com/urlshortener/v1/url",
#'                       "POST",
#'                       data_parse_function = function(x) x$id)
#'
#'    f(the_body = body)
#'  }
#'
#' To use the above functions:
#' library(googleAuthR)
#' # go through authentication flow
#' gar_auth()
#' s <- shorten_url("http://markedmondson.me")
#' s
#' }
#'
#'
#' @return A function that can fetch the Google API data you specify
#' @export
#' @import assertthat
#' @importFrom httr set_config config
#' @importFrom digest digest
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
    path <-
      paste(names(path_args), path_args, sep="/", collapse="/" )

  }

  if(!is.null(pars_args)){
    pars <-
      paste(names(pars_args), pars_args, sep='=', collapse='&')

  }



  func <- function(path_arguments=NULL,
                   pars_arguments=NULL,
                   the_body=NULL,
                   batch=FALSE,
                   ...){

    # extract the shiny token from the right environments
    ## gets the environments
    all_envs <- as.character(sys.calls())
    ## gets the one with_shiny
    with_shiny_env <- which(grepl("with_shiny", all_envs))
    ## gets the arguments of with_shiny
    if(any(with_shiny_env)){
      call_args <- as.list(match.call(definition = sys.function(with_shiny_env),
                                      call = sys.call(with_shiny_env),
                                      expand.dots = F)[-1])
      ## gets the calling function of with_shiny to evaluate the reactive token in
      f <- do.call("parent.frame", args = list(), envir = sys.frame(with_shiny_env))
      ## evaluates the shiny_access_token in the correct environment
      shiny_access_token <- eval(call_args$shiny_access_token,
                                 envir = f)
      myMessage("Shiny Token found in environment", level=1)
    } else {
      shiny_access_token <- NULL
    }

    if(!checkTokenAPI(shiny_access_token)){
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

    req_url <- paste0(baseURI, path, pars)

    ## if called with gar_batch wrapper set batch to TRUE
    with_gar_batch <- which(grepl("gar_batch", all_envs))
    if(any(with_gar_batch)){
      ## batching
      req <- list(req_url = req_url,
                  shiny_access_token = shiny_access_token,
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
                              shiny_access_token=shiny_access_token,
                              request_type=http_header,
                              the_body=the_body,
                              customConfig=customConfig,
                              simplifyVector=simplifyVector)

    } else {
      req <- doHttrRequest(req_url,
                           shiny_access_token=shiny_access_token,
                           request_type=http_header,
                           the_body=the_body,
                           customConfig=customConfig,
                           simplifyVector=simplifyVector)
    }


    if(!is.null(data_parse_function)){
      reqtry <- try(data_parse_function(req$content, ...))
      if(any(is.error(reqtry), is.null(reqtry))){
        warning("API Data failed to parse.  Returning parsed from JSON content.
                    Use this to test against your data_parse_function.")
        req <- req$parsed_content
      } else {
        req <- reqtry
      }
    }

    req

  }
  ##returns a function that can call the API
  func

}





#' ReTry API requests for certain errors using exponential backoff.
#'
#' @param f A function of a http request
#'
#' @keywords internal
retryRequest <- function(f){

  verbose <- getOption("googleAuthR.verbose")

  if(verbose <= 1){
    the_request <- try(httr::with_verbose(f))
  } else {
    the_request <- try(f)
  }

  if(is.error(the_request)){
    stop("Request failed before finding status code: ", error.message(the_request), call. = FALSE)
  } else {
    status_code <- as.character(the_request$status_code)
  }

  if(!(grepl("^20",status_code))){
    myMessage("Request Status Code: ", status_code, level = 3)

    content <- try(jsonlite::fromJSON(httr::content(the_request,
                                              as = "text",
                                              type = "application/json",
                                              encoding = "UTF-8")))
    if(is.error(content)){

      warning("No JSON content found in request", call. = FALSE)
      error <- "Could not fetch response"

    } else if(exists("error", where=content)) {

      error <- content$error$message

    } else {
      error <- "Unspecified Error"
    }

    myMessage("API returned error: ",paste(error), level = 2)

    if(grepl("^5|429",status_code)){
      try_attempts <- getOption("googleAuthR.tryAttempts")
      for(i in 1:try_attempts){
        myMessage("Trying again: ", i, " of ", try_attempts, level = 3)
        Sys.sleep((2 ^ i) + stats::runif(n = 1, min = 0, max = 1))
        the_request <- try(f)
        if(grepl("^20",status_code)) break
      }
      myMessage("All attempts failed.", level = 3)
    } else {
      myMessage("No retry attempted: ", error, level = 2)
    }

  }

  ## either reraise the error or should be good now
  if(is.error(the_request)){
    stop(error.message(the_request))
  }

  the_request
}

#' Get URL content based on if its Shiny or local
#'
#' @description
#' This changes the auth type depending on if its local or on Shiny
#'
#' @param url the url of the page to retrieve
#' @param session a shiny session object if in a shiny app.
#' @param request_type the type of httr request function: GET, POST, PUT, DELETE etc.
#' @param the_body body of POST request
#' @param params A named character vector of other parameters to add to request.
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
                          shiny_access_token = NULL,
                          request_type="GET",
                          the_body=NULL,
                          customConfig=NULL,
                          simplifyVector=getOption("googleAuthR.jsonlite.simplifyVector")){

  arg_list <- list(verb = request_type,
                   url = url,
                   config = get_google_token(shiny_access_token),
                   body = the_body,
                   encode = if(!is.null(customConfig$encode)) customConfig$encode else "json",
                   add_headers("Accept-Encoding" = "gzip"),
                   user_agent(paste0("googleAuthR/",
                                     packageVersion("googleAuthR"),
                                     " (gzip)"))
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

