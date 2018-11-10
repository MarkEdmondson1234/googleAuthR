#' Takes a generated API function and lets you page through results
#' 
#' A helper function to help with the common task of paging through large API results.
#' 
#' @param f a function created by gar_api_generator
#' @param next_f A function that will let you get the nextPageToken from objects returned by f
#' @param page_arg In f, you need to set a pars_argument that will change for each page.
#' 
#' @details 
#' 
#'  The next_token_f needs to extract the nextPageToken from the API response after the data_parse_function has been applied.
#'  e.g. if response is x and next_token_f = function(x) x$npt then the nextPageToken should be available at x$npt after parsing.  It should return NULL if no (more) paging is necessary.
#'  
#' @return A list of the API responses
#'
#' @export
#' @import assertthat
gar_api_page <- function(f, 
                         next_f = function(x) x$nextPageToken,
                         page_arg = "start-index"){

  assert_that(is.gar_function(f),
              is.function(next_f),
              is.string(page_arg))
  
  first <- f()
  
  needs_paging <- next_f(first)
  
  myMessage()
  
  if(is.null(needs_paging)){
    myMessage("No paging required", level = 2)
    return(list(first))
  }
  
  all_pages <- list(first)
  while(!is.null(needs_paging)){
    myMessage("Paging through API - token:", needs_paging, level = 2)
    # api call with new page token
    l <- list()
    l[[page_arg]] <- needs_paging
    a_page <- f(pars_arguments = l)
    
    all_pages <- c(all_pages, list(a_page))
    
    needs_paging <- next_f(a_page)
  }
  
  all_pages
  
  
}