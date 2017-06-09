# cache global
.gar_cache <- new.env(parent = emptyenv())
.gar_cache$cache <- NULL  # what type of caching
.gar_cache$api <- NULL # if in memory, the objects cache

#' Set cache location
#' 
#' These functions let you set the cache behaviour for your API calls
#' 
#' @param cache The directory to save cache to, or \code{"memory"} to save to RAM
#' 
#' @details 
#' 
#' This is ignored if \code{getOption("googleAuthR.mock_test") == TRUE} 
#'   as the cache will default to folder \code{"mock"}
#' 
#' @export
gar_cache_set_loc <- function(cache){
  assertthat::assert_that(
    assertthat::is.string(cache)
  )
  .gar_cache$cache <- cache
}

#' @rdname gar_cache_set_loc
#' @export
gar_cache_get_loc <- function(){
  .gar_cache$cache
}

#' @rdname gar_cache_set_loc
#' @export
gar_cache_empty <- function(){
  .gar_cache$cache <- NULL
}


load_cache <- function(cache_name, type){
  assertthat::assert_that(
    assertthat::is.string(type)
  )
  
  if(type != "memory"){

    if(file.exists(cache_name)){
      out <- readRDS(cache_name)

    } else {
      out <- NULL
    }

  } else {
    ## NULL if not there
    out <- .gar_cache$api[[cache_name]]
  }
  
  if(!is.null(out)){
    myMessage("# Cached API call from ", cache_name, level = 3)
    if(getOption("googleAuthR.verbose") < 3){
      cat("\n# Cached API call from ", cache_name) # also cat for test logs
    }

  } else {
    myMessage("No cache found, making API call", level = 3)
  }
  
  out
}

put_cache <- function(obj, cache_name, type){
  assertthat::assert_that(
    assertthat::is.string(type)
  )
  
  if(type != "memory"){

    dir.create(type, showWarnings = FALSE)    
    

    saveRDS(obj, file = cache_name)
  } else {
    save_to_mem(obj, cache_name = cache_name)
  }
  
  myMessage("Saving cached API call to ", cache_name, level = 2)
  if(getOption("googleAuthR.verbose") < 3){
    cat("\n# Saving cached API call to ", cache_name) # also cat for test logs
  }

  
  TRUE
}

save_to_mem <- function(obj, cache_name){
  .gar_cache$api[[cache_name]] <- obj
}

# read from cache - return NULL if its not there
read_cache <- function(arg_list, cache_dir){

  assertthat::assert_that(
    assertthat::is.string(cache_dir)
  )
  ## check for presence of API output saved in mock folder
  cache_name <- make_cache_name(arg_list, cache_dir = cache_dir)   
  
  load_cache(cache_name, type = cache_dir)

}


# make the cache name
make_cache_name <- function(arg_list, cache_dir){
  assertthat::assert_that(
    assertthat::is.string(cache_dir)
  )
  
  call_func <- cache_call()
  hash_string <- make_cache_hash(call_func, arg_list)
  myMessage("Lookup cache API call for ", call_func, level = 1)
  
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
  
  if(package_name %in%  loadedNamespaces()){
    myMessage("Do cache for library", package_name, level = 2)
  } else {
    stop("gar_cache_get_loc() is set to ", gar_cache_get_loc(), " but getOption('googleAuthR.cache_package') is set to ", package_name, " which is not loaded.")
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
gar_cache_list <- function(cache_dir = gar_cache_get_loc()){
  cache_meta <- file.path(cache_dir,"cached_list.rds")

  the_list <- load_cache(cache_meta, cache_dir)
  if(is.null(the_list)){
    stop("No cache meta data found in ", normalizePath(cache_meta))
  }
  
  the_list

}

#' Delete mock API caches
#' @export
gar_cache_delete <- function(cache_dir = gar_cache_get_loc()){

  if(!file.exists(cache_dir)){
    stop("No cache meta data found in ", normalizePath(cache_dir))
  }
  
  if(cache_dir != "memory"){
    unlink(cache_dir, recursive = TRUE)
  } else {
    .gar_cache$api <- NULL # if in memory, the objects cache
  }
  

}


make_cache_hash <- function(call_func, arg_list){
  lcf <- as.list(call_func)
  
  ## ignore config in arg_list so unauthenticated calls can be cached
  if(!is.null(arg_list$config)){
    arg_list$config <- NULL
  }
  
  call_args_string <- paste(names(lcf[-1]), lcf[-1], collapse = ",", sep="=")
  arg_list_string <- paste(names(arg_list), unlist(arg_list), collapse = ",", sep="=")
  
  digest::digest(paste(lcf[[1]], call_args_string, arg_list_string, sep =":"))
}

# save the cache data
## save meta data
save_cache <- function(req, call_func, arg_list, cache_dir = "mock") {

  ## save req cache
  cache_name <- make_cache_name(arg_list, cache_dir = cache_dir)

  put_cache(req, cache_name = cache_name, cache_dir)
  
  ## save meta data
  meta_name <- file.path(cache_dir,"cached_list.rds")
  cached_list <- load_cache(meta_name, type = cache_dir)
  
  lcf <- as.list(call_func)
  call_args_string <- paste(names(lcf[-1]), lcf[-1], collapse = ",", sep="=")
  arg_list_string <- paste(names(arg_list), unlist(arg_list), collapse = ",", sep="=")

  this_cache <- data.frame(function_name = as.character(lcf[[1]]), 
                           arguments = call_args_string,
                           arg_list = arg_list_string, 
                           hash = make_cache_hash(call_func, arg_list),
                           created = Sys.time(), 
                           cache_location = cache_dir,
                           stringsAsFactors = FALSE)
  
  cached_list <- rbind(this_cache, cached_list)
  put_cache(cached_list, cache_name = meta_name, cache_dir)
  myMessage("Updated cached_list ", meta_name, level = 1)
}