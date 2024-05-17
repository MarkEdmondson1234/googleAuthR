#' Read the diagnostic object returned on API parse errors.
#' 
#' @param filename The file created from API errors, usually called \code{gar_parse_error.rds}
#' 
#' @details 
#' 
#' When googleAuthR API parsing fails, it will write a file called gar_parse_error.rds to the directory.  Feed that file into this function to help diagnose the problem.
#' 
#' @import assertthat
#' @export
gar_debug_parsing <- function(filename = "gar_parse_error.rds"){
  assert_that(is.readable(filename))
  
  myMessage("# When creating a GitHub issue, please include this output.", level = 3)
  obj <- readRDS(filename)
  
  assert_that(is.gar_parse_error(obj))
  
  print(obj)
  
  tryCatch({
    myMessage("- Attempting data parsing", level = 3)
    do.call(obj$response$data_parse_func, args = obj$response$data_parse_args)
  }, error = function(err){
    myMessage("** Parsing failed with this error: ", err$message)
  })
  
  obj
}

is.gar_parse_error <- function(x){
  inherits(x, "gar_parse_error")
}

#' @export
print.gar_parse_error <- function(x, ...){
  utils::str(x)
}
