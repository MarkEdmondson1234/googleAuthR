#' Takes a generated API function and lets you page through results
#' 
#' A helper function to help with the common task of paging through large API results.
#' 
#' @param f a function created by \link{gar_api_generator}
#' @param page_f A function that will extract the next page information from \code{f()}.  Should return \code{NULL} if no paging is required, or the value for \code{page_arg} if it is.
#' @param page_method Method of paging: \code{url} will fetch by changing the fetch URL; \code{param} will fetch the next page via a parameter set in \code{page_arg}; \code{path} will change a path variable set in \code{page_arg}
#' @param page_arg If \code{page_method="param"}, you need to set this to the parameter that will change for each API page.
#' @param body_list If \code{page_method="body"}, you need to set the body that will be used in each API call, including the top level parameter \code{page_arg} that will be modified by \code{page_f} 
#' 
#' @details 
#' 
#'  The \code{page_f} function operates on the object returned from the \code{data_parse_function} of the function \code{f}
#'  
#'  If using \code{page_method="url"} then then \code{page_f} function needs to return the URL that will fetch the next page of results.  The default finds this via \code{x$nextLink}.  This is the easiest to implement if available and is recommended. 
#' 
#'  If using \code{page_method = "param"}, then \code{page_f} needs to extract the parameter specified in \code{page_arg} that will fetch the next page of the results, or NULL if no more pages are required.
#'  e.g. if response is x,  \code{page_f} should extract the next value for the parameter of \code{page_arg} that fetches the next results.  It should also return \code{NULL} if no (more) paging is necessary.  See examples.
#'  Remember to add the paging argument (e.g. \code{start-index}) to the generated function too, so it can be modified.   
#'  
#'  
#' @examples 
#' 
#' \dontrun{
#' # demos the two methods for the same function.
#' # The example is for the Google Analytics management API, 
#' #  you need to authenticate with that to run them. 
#' 
#' 
#' # paging by using nextLink that is returned in API response
#' ga_segment_list1 <- function(){
#' 
#'   # this URL will be modified by using the url_override argument in the generated function
#'   segs <- gar_api_generator("https://www.googleapis.com/analytics/v3/management/segments",
#'                             "GET",
#'                             pars_args = list("max-results"=10),
#'                             data_parse_function = function(x) x)
#'                          
#'                          
#'   gar_api_page(segs, 
#'                page_method = "url",
#'                page_f = function(x) x$nextLink)
#' 
#' }
#' 
#' # paging by looking for the next start-index parameter
#' 
#' ## start by creating the function that will output the correct start-index
#' paging_function <- function(x){
#'   next_entry <- x$startIndex + x$itemsPerPage
#' 
#'   # we have all results e.g. 1001 > 1000
#'   if(next_entry > x$totalResults){
#'     return(NULL)
#'   }
#' 
#'   next_entry
#'   }
#'   
#' ## remember to add the paging argument (start-index) to the generated function too, 
#' ##  so it can be modified.    
#' ga_segment_list2 <- function(){
#' 
#'   segs <- gar_api_generator("https://www.googleapis.com/analytics/v3/management/segments",
#'                            "GET",
#'                             pars_args = list("start-index" = 1,
#'                                              "max-results"=10),
#'                             data_parse_function = function(x) x)
#'                            
#'   gar_api_page(segs, 
#'                page_method = "param",
#'                page_f = paging_function,
#'                page_arg = "start-index")
#' 
#'   }
#' 
#' 
#' identical(ga_segment_list1(), ga_segment_list2())
#' 
#' 
#' }
#'  
#' @return A list of the API page responses, that you may need to process further into one object.
#'
#' @export
#' @import assertthat
gar_api_page <- function(f, 
                         page_f = function(x) x$nextLink,
                         page_method = c("url", "param", "path","body"),
                         page_arg = NULL,
                         body_list = NULL){

  page_method <- match.arg(page_method)
  assert_that(is.gar_function(f),
              is.function(page_f))
  
  if(!is.null(page_arg) && page_method %in% c("param","path")){
    assert_that(is.string(page_arg))
  }
  
  if(!is.null(page_arg) && page_method == "body"){
    assert_that(is.string(page_arg), is.list(body_list))
  }
  
  if(page_method != "body"){
    first <- f()
  } else {
    first <- f(the_body = body_list)
  }
  
  needs_paging <- tryCatch(page_f(first),
                           error = function(err){
                             stop(paste("The paging function errored with: ", err$message),
                                  call. = FALSE)
                           })
  
  if(is.null(needs_paging)){
    myMessage("No paging required", level = 2)
    return(list(first))
  }
  
  all_pages <- list(first)
  while(!is.null(needs_paging)){
    myMessage("Paging through API - token:", needs_paging, level = 2)
    # api call with new page token
    
    if(page_method == "body"){
      l <- body_list
    } else {
      l <- list()
    }
    
    if(page_method != "url"){
      l[[page_arg]] <- needs_paging
    }
    
    a_page <- switch(page_method,
                     param = f(pars_arguments = l),
                     path  = f(path_arguments = l),
                     url   = f(url_override = needs_paging),
                     body  = f(the_body = l))

    all_pages <- c(all_pages, list(a_page))
    
    needs_paging <- page_f(a_page)
  }
  
  all_pages
  
}