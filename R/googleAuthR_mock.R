# find the function call we want, e.g. in the package testing
mock_call <- function(){
  call_funcs <- as.character(sys.calls())
  package_funcs <- ls(paste0("package:",packageName()))
  just_funcs <- gsub("^(.+)\\(.+$", "\\1", call_funcs)
  out <- just_funcs[just_funcs %in% package_funcs]
  if(length(out) != 1){
    stop("Matched more than one function for mock_call", out)
  }
  myMessage("Matched package call: ", out, level = 3)
  out
}


#' See which functions have mock cache
#' 
#' When testing API calls, see which functions have API caches 
#'   activated when \code{options(googleAuthR.mock_test = TRUE)}
#' 
#' @export
gar_mock_list <- function(){
  mock_dir <- file.path("tests","mock","cached_list.rds")

  if(!file.exists(mock_dir)){
    stop("No mock meta data found in ", normalizePath(mock_dir))
  }
  
  readRDS(mock_dir)

}

#' Delete mock API caches
#' @export
gar_mock_delete <- function(){
  mock_dir <- file.path("tests","mock")
  if(!file.exists(mock_dir)){
    stop("No mock meta data found in ", normalizePath(mock_dir))
  }
  
  unlink(mock_dir, recursive = TRUE)
}


make_mock_hash <- function(call_func){
  lcf <- as.list(call_func)
  call_args_string <- paste(names(lcf[-1]), lcf[-1], collapse = ",", sep="=")
  digest::digest(paste(lcf[[1]], call_args_string, sep =":"))
}

# save the mock data
## save meta data
save_mock_cache <- function(call_func) {
  meta_cache <- file.path("tests","mock","cached_list.rds")
  if(file.exists(meta_cache)){
    cached_list <- readRDS(meta_cache)
  } else {
    cached_list <- NULL
  }
  
  lcf <- as.list(call_func)
  call_args_string <- paste(names(lcf[-1]), lcf[-1], collapse = ",", sep="=")

  this_cache <- data.frame(function_name = as.character(lcf[[1]]), 
                           arguments = call_args_string,
                           hash = make_mock_hash(call_func),
                           created = Sys.time(), 
                           stringsAsFactors = FALSE)
  
  cached_list <- rbind(this_cache, cached_list)
  saveRDS(cached_list, file = meta_cache)
  myMessage("Updated cached_list ", meta_cache, level = 3)
}