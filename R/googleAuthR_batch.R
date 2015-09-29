#' Turn a vector of gar_fetch_functions into batch functions
#' 
#'
gar_batch <- function(function_list){
  
  ## call functions that return just request URL and body with batch turned on
  # funcs <- eval(substitute(alist(function_list)))
  boundary <- "--gar_batch"
  
  names(function_list) <- 1:length(function_list)
  str(function_list[1])
  parsed <- paste(boundary, 
                  "Content-Type: application/http",
                  "Content-ID:",names(function_list[1]),
                  function_list[[1]]$http_header, 
                  gsub("https://www.googleapis.com","",function_list[[1]]$req_url),
                  jsonlite::toJSON(function_list[[1]]$the_body))
  
  parsed <- paste(parsed, boundary, "--")
  
  list(parsed = parsed,
       shiny_access_token = function_list[[1]]$shiny_access_token)

                  
  
  ## construct batch POST request
  
  ## call doHttrRequest with batched together functions
  
}


#' Batch Requests
#' 
#' @description
#' Batch requests to Google APIs that support it
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
doBatchRequest <- function(batched){
  
  arg_list <- list(url = "http://www.googleapis.com/batch", 
                   config = get_google_token(batched$shiny_access_token), 
                   body = batched$parsed,
                   encode = "json",
                   httr::add_headers("Accept-Encoding" = "gzip",
                                     "Content-Type" = "multipart/mixed; boundary=gar_batch"),
                   httr::user_agent("libcurl/7.43.0 r-curl/0.9.3 httr/1.0.0 googleAuthR/0.1.2 (gzip)"))
  
  
  req <- retryRequest(do.call("POST", 
                              args = arg_list,
                              envir = asNamespace("httr")))
  
#   if(checkGoogleAPIError(req)){
#     content <- httr::content(req, as = "text")
#     # content <- jsonlite::fromJSON(content)
#     req$content <- content
#   }
  
  req
}