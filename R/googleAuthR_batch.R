#' Turn a vector of gar_fetch_functions into batch functions
#' 
#'
gar_batch <- function(function_list){
  
  
  ## construct batch POST request
  parse_list <- lapply(function_list, makeBatchRequest)
  
  parsed <- paste(c(parse_list, "--gar_batch--"), collapse="")
  
  l <- list(parsed = parsed,
            shiny_access_token = function_list[[1]]$shiny_access_token)
  
  ## call doHttrRequest with batched together functions
  doBatchRequest(l)
  
}

#' Make the batch request inner content
#' 
#' 
makeBatchRequest <- function(f){
  
  boundary <- "--gar_batch"
  url_stem <- gsub("https://www.googleapis.com","",f$req_url)
  
  ## construct batch POST request
  header <- paste(boundary,
                  "Content-Type: application/http",
                  paste0("Content-ID: ",f$name),
                  "\r\n",
                  sep = "\r\n")
  
  content <- paste0(f$http_header," ", 
                    url_stem, "\r\n")
  
  if(!is.null(f$the_body)){
    batch_body <- paste0(jsonlite::toJSON(f$the_body), "\r\n")
    part_content_length <- nchar(batch_body)
    header <- paste(header, 
                    paste0("Content-Length: ", part_content_length),
                    sep = "\r\n")
  } else {
    batch_body <- NULL
  }
  
  parsed <- paste(header, content, batch_body, sep = "\r\n")
  
  parsed
  
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
  
  arg_list <- list(config = get_google_token(batched$shiny_access_token), 
                   # url = "http://www.httpbin.org/post", 
                   url = "https://www.googleapis.com/batch", 
                   body = batched$parsed,
                   encode = "multipart",
                   # httr::user_agent("libcurl/7.43.0 r-curl/0.9.3 httr/1.0.0 googleAuthR/0.1.2 (gzip)"),
                   httr::add_headers("Content-Type" = "multipart/mixed; boundary=gar_batch")
                   )
  
  
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