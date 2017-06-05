# read from cache - return NULL if its not there
read_cache <- function(arg_list, cache_dir = "mock"){
  ## check for presence of API output saved in mock folder
  cache_name <- make_cache_name(arg_list, cache_dir = cache_dir)   
  
  cache_exists <- file.exists(cache_name)
  
  ## if present, use mock result instead
  if(cache_exists){
    myMessage("# Cached API call from ", cache_name, level = 3)
    cat("\n# Cached API call from ", cache_name) # also cat for test logs
    mock_cache <- readRDS(cache_name)
    req <- mock_cache
  } else {
    req <- NULL
  }
  
  req
}


# make the cache name
make_cache_name <- function(arg_list, cache_dir = "mock"){
  call_func <- cache_call()
  hash_string <- make_cache_hash(call_func, arg_list)
  myMessage("Caching API call for ", call_func, level = 3)
  
  ## create directories if needed
  dir.create(cache_dir, showWarnings = FALSE)    
  
  file.path(cache_dir, hash_string)

}


#' Setup which package to perform mock testing upon
#' 
#' This helps make sure the right functions are mocked
#' 
#' @param package_name The package to cache API calls for
#' 
#' Only functions within the package will be mocked.  
#' The \code{options(googleAuthR.mock_test = TRUE)} flag will also need to be on.
#' 
#' You can also set the package via \code{options(googleAuthR.mock_package = "yourPackage")}
#' 
#' @export
gar_setup_cache <- function(package_name){
  
  options(googleAuthR.cache_package = package_name)
  
}


# get the functions to test
cache_call <- function(package_name = getOption("googleAuthR.cache_package")){
  
  if(package_name == ""){
    stop("Need to set the package to mock API calls against using gar_setup_mock()")
  }
  
  call_funcs <- as.character(sys.calls())
  package_funcs <- ls(paste0("package:",package_name))
  just_funcs <- gsub("^(.+?)\\(.*\\)$", "\\1", call_funcs)
  out <- sys.calls()[just_funcs %in% package_funcs][[1]]
  myMessage("Matched package call: ", out, level = 2)
  out
}


#' See which functions have mock cache
#' 
#' When testing API calls, see which functions have API caches 
#'   activated when \code{options(googleAuthR.mock_test = TRUE)}
#' 
#' @export
gar_cache_list <- function(cache_dir = "mock"){
  cache_meta <- file.path(cache_dir,"cached_list.rds")

  if(!file.exists(cache_meta)){
    stop("No cache meta data found in ", normalizePath(cache_meta))
  }
  
  readRDS(cache_meta)

}

#' Delete mock API caches
#' @export
gar_cache_delete <- function(cache_dir = "mock"){

  if(!file.exists(cache_dir)){
    stop("No mock meta data found in ", normalizePath(cache_dir))
  }
  
  unlink(cache_dir, recursive = TRUE)
}


make_cache_hash <- function(call_func, arg_list){
  lcf <- as.list(call_func)
  call_args_string <- paste(names(lcf[-1]), lcf[-1], collapse = ",", sep="=")
  arg_list_string <- paste(names(arg_list), unlist(arg_list), collapse = ",", sep="=")
  
  digest::digest(paste(lcf[[1]], call_args_string, arg_list_string, sep =":"))
}

# save the cache data
## save meta data
save_cache <- function(req, call_func, arg_list, cache_dir = "mock") {
  
  ## save req cache
  cache_name <- make_cache_name(arg_list, cache_dir = cache_dir)
  myMessage("Saving cached API call to ", cache_name, level = 3)
  cat("\n# Saving cached API call to ", cache_name) # also cat for test logs
  saveRDS(req, file = cache_name)
  
  ## save meta data
  meta_cache <- file.path(cache_dir,"cached_list.rds")
  if(file.exists(meta_cache)){
    cached_list <- readRDS(meta_cache)
  } else {
    cached_list <- NULL
  }
  
  lcf <- as.list(call_func)
  call_args_string <- paste(names(lcf[-1]), lcf[-1], collapse = ",", sep="=")
  arg_list_string <- paste(names(arg_list), unlist(arg_list), collapse = ",", sep="=")

  this_cache <- data.frame(function_name = as.character(lcf[[1]]), 
                           arguments = call_args_string,
                           arg_list = arg_list_string, 
                           hash = make_mock_hash(call_func, arg_list),
                           created = Sys.time(), 
                           stringsAsFactors = FALSE)
  
  cached_list <- rbind(this_cache, cached_list)
  saveRDS(cached_list, file = meta_cache)
  myMessage("Updated cached_list ", meta_cache, level = 3)
}