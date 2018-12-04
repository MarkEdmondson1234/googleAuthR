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
  myMessage("# When creating a GitHub issue, please include this output.")
  obj <- readRDS(filename)
  str(obj)
  obj
}