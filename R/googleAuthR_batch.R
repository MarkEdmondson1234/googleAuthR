#' Turn a vector of gar_fetch_functions into batch functions
#' 
#'
gar_batch <- function(...){
  
  batch_g <- TRUE
  
  ## call functions that return just request URL and body with batch turned on
  funcs <- eval(substitute(alist(...)))
  funcs
  
  ## construct batch POST request
  
  ## call doHttrRequest with bathced together functions
  
}


#' doBatchRequest
#' 
#' Returns whats necessary for a batch request
doBatchRequest <- function(req_url, 
                           shiny_access_token, 
                           http_header, 
                           the_body){
  
  
  
}