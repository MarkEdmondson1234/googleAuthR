#' googleAuthR data fetch function generator
#' 
#' This function generates other functions for use with Google APIs
#' 
#' @param baseURI The stem of the API call.
#' @param http_header Type of http request.
#' @param path_args A named list with name=folder in request URI, value=the function variable. 
#' @param pars_args A named list with name=parameter in request URI, value=the function variable.
#' @param data_parse_function A function that takes a request response, parses it and returns the data you need.
#' 
#' \strong{path_args} and \strong{pars_args} add default values to the baseURI.  
#' You don't need to supply access_token for OAuth2 requests in pars_args, 
#'   this is dealt with in gar_auth()
#' 
#' @return A function that can fetch the Google API data you specify
#' @export
gar_api_generator <- function(baseURI,
                              http_header = c("GET","POST","PUT","DELETE", "PATCH"),
                              path_args = NULL,
                              pars_args = NULL,
                              data_parse_function=NULL){
  
  http_header <- match.arg(http_header)
  if(substr(baseURI,nchar(baseURI),nchar(baseURI))!="/") baseURI <- paste0(baseURI, "/")
  
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
  
  # wizardry to extract the shiny token from the right environments
  ## gets the environments
  all_envs <- as.character(sys.calls())
  ## gets the one with_shiny
  with_shiny_env <- which(grepl("with_shiny", all_envs))
  ## gets the arguments of with_shiny
  call_args <- as.list(match.call(definition = sys.function(with_shiny_env),
                                  call = sys.call(with_shiny_env),
                                  expand.dots = F)[-1])
  ## gets the calling function of with_shiny to evaluate the reactive token in
  f <- do.call("parent.frame", args = list(), envir = sys.frame(with_shiny_env))
  ## evaluates the shiny_access_token in the correct environment
  shiny_access_token <- eval(call_args$shiny_access_token, 
                             envir = f)

  if(!is.null(shiny_access_token)) message("Found Shiny Token") else {message("No Shiny Token")}
  
  func <- function(path_arguments=NULL, 
                   pars_arguments=NULL,
                   the_body=NULL,
                   ...){
    
    if(checkTokenAPI(shiny_access_token)){
      
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
      
      message("request: ", req_url)
      
      req <- doHttrRequest(req_url, 
                           shiny_access_token, 
                           http_header, 
                           the_body)
      
      if(!is.null(data_parse_function)){
        req <- data_parse_function(req$content, ...)
      } 
      
    } else {
      stop("Invalid Token")
    }
    
    req

  }
  ##returns a function that can call the API
  func
  
}

#' Check API data token
#' 
#' @param shiny_access_token auth token
#' 
#' @return boolean if it works.
#' 
#' @keywords internal
#' @family data fetching functions
checkTokenAPI <- function(shiny_access_token=NULL, verbose=F){
  
  if(is.null(shiny_access_token)){
    ## local token
    token <- Authentication$public_fields$token
    
    if(token_exists(verbose = verbose) && is_legit_token(token, verbose=verbose)) {
      if(verbose) message("Valid local token")
      TRUE
    } else {
      if(verbose) message("Invalid local token")
      FALSE
    }
    
  } else {
    ## is it a valid shiny token passed?
    if(is_legit_token(shiny_access_token)){
      if(verbose) message("Valid Shiny token")
      TRUE
    } else {
      if(verbose) message("Invalid Shiny token")
      FALSE
    }
  }
  
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
#' 
#' @details Example of params: c(param1="foo", param2="bar")
#' 
#' 
#' @keywords internal
doHttrRequest <- function(url,
                          shiny_access_token = NULL,
                          request_type="GET", 
                          the_body=NULL){
  
  arg_list <- list(url = url, 
                   config = get_google_token(shiny_access_token), 
                   body = the_body,
                   encode = "json")
  
  if(!is.null(the_body)){
    message("Body JSON parsed to: ", jsonlite::toJSON(the_body, auto_unbox=T)) 
  }
  
  req <- do.call(request_type, 
                 args = arg_list,
                 envir = asNamespace("httr"))
  
  if(checkGoogleAPIError(req)){
    content <- httr::content(req, as = "text", type = "application/json",encoding = "UTF-8")
    content <- jsonlite::fromJSON(content)
    req$content <- content
  }
  
  req
}

#' Get Google API errors
#' 
#' @param req a httr request
#' @param ok_content_types Expected content type of request
#' @keywords internal
checkGoogleAPIError <- function (req, 
                                 ok_content_types=getOption("googleAuthR.ok_content_types")) {

  ga.json <- httr::content(req, as = "text", type = "application/json")
  if(nchar(ga.json) > 0) {
    ga.json <- jsonlite::fromJSON(ga.json)
  
    if (is.null(ga.json)) { 
      stop('data fetching did not output correct format') 
    }
    
    if (!is.null(ga.json$error$message)) {
      stop("JSON fetch error: ",paste(ga.json$error$message))
    }
    
    if (grepl("Error 400 (Bad Request)",ga.json[[1]][1])) {
      stop('JSON fetch error: Bad request URL - 400. Fetched: ', url)
    }
  }
  
  if(!is.null(req$headers$`content-type`)){
    if(!(req$headers$`content-type` %in% ok_content_types)) {
      
      stop(sprintf(paste("Not expecting content-type to be:\n%s"),
                   req$headers[["content-type"]]))
      
    }
  } else {
    message("No content-type returned.")
    return(FALSE)
  }

  
  httr::stop_for_status(req)
  
  TRUE
}
