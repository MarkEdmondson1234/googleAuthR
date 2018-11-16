library(httptest)
context("Paging")


auth_env <- "GAR_AUTH_FILE"


test_that("Paging via parameters", {
  skip_on_cran()
  skip_if_no_env_auth(auth_env)
  paging_function <- function(x){
    next_entry <- x$startIndex + x$itemsPerPage
    
    # we have all results e.g. 1001 > 1000
    if(next_entry > x$totalResults){
      return(NULL)
    }
    
    next_entry
  }
  
  ga_segment_list <- function(){
    
    segs <- gar_api_generator("https://www.googleapis.com/analytics/v3/management/segments",
                              "GET",
                              pars_args = list("start-index" = 1,"max-results"=10),
                              data_parse_function = function(x) x)
    
    pages <- gar_api_page(segs, 
                          page_f = paging_function,
                          page_method = "param",
                          page_arg = "start-index")
    
    pages
    
  }
  
  o <- ga_segment_list()
  
  expect_true(is.data.frame(o[[1]]$items))
  
})

test_that("Paging via nextLink", {
  skip_on_cran()
  skip_if_no_env_auth(auth_env)
  ga_segment_list <- function(){
    
    url <- "https://www.googleapis.com/analytics/v3/management/segments"
    segs <- gar_api_generator(url,
                              "GET",
                              pars_args = list("max-results"=10),
                              data_parse_function = function(x) x)
    
    gar_api_page(segs)
    
    
  }
  
  o <- ga_segment_list()
  
  expect_true(is.data.frame(o[[1]]$items))
  
})