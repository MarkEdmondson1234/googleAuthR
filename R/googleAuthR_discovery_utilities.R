## from https://github.com/hadley/httr/blob/4624451f8cc395a90730b5a10b50ba005187f2ff/R/oauth-cache.R
add_line <- function(line, path, quiet = FALSE) {
  if (file.exists(path)) {
    lines <- readLines(path, warn = FALSE)
    lines <- lines[lines != ""]
  } else {
    lines <- character()
  }
  
  if (line %in% lines) return(TRUE)
  if (!quiet) message("Adding ", line, " to ", path)
  
  lines <- c(lines, line)
  writeLines(lines, path)
  TRUE
}

make_f_arguments <- function(f_name, arguments, exclude = NULL){
  arguments <- arguments[setdiff(names(arguments), exclude)]
  paste0(
    f_name, " <- function(",
    paste(make.names(names(arguments)), sep="\n", collapse = ",\n\n"),
    "){\n\n"
  )
}


make_api_url_string <- function(api_json_resource_method, api_json){
  
  start_url <- paste0(api_json$baseUrl, api_json_resource_method$path)
  path_args <- type_parameters(api_json_resource_method)
  
  if(length(path_args) > 0){
    ## replace path args with %s
    end_url <- gsub(paste0("\\{(",paste(path_args, collapse = "|"),")\\}"), "%s", start_url)
    out <- paste0("url <- sprintf('", end_url, "'\n,", 
                  paste(make.names(type_parameters(api_json_resource_method)), collapse = ", "),")\n")
  } else {
    out <- paste0("url <- '", start_url, "'\n")
  }
  
  out
}

## extract names of parameters of type path/query
type_parameters <- function(api_json_resource_method, type = c("path","query")){
  
  type <- match.arg(type)
  pars <- api_json_resource_method$parameters
  
  unlist(lapply(names(pars), function(x) if(pars[[x]]$location == type) x))
  
}

make_pars_list <- function(api_json_resource_method){
  
  query_pars <- type_parameters(api_json_resource_method, "query")
  
  if(length(query_pars) == 0) return(NULL)
  
  list_vars <- lapply(query_pars, 
                      make_pars_entry,
                      api_json_resource_method = api_json_resource_method)
  paste0(
    "pars_args = list(\n",
    paste(collapse = ",", list_vars),
    "),\n" 
  )
  
}

make_pars_entry <- function(x, api_json_resource_method){
  paste0("\t\t`",x, "` = ", make.names(x), "\n", collapse = "")
}

make_vars_description <- function(x, 
                                  api_json_resource_method){
  
  paste0("@param ", make.names(x), " ", api_json_resource_method[[x]]$description,"\n", 
         collapse = "")
  
}

# get the methods
get_json_methods <- function(api_json_resources){
  
  set_global(list())
  out <- recursive_key_finder(api_json_resources)
  set_global(list())
  out
}



recursive_key_finder <- function(the_list, key = "methods"){
  
  if(key %in% names(the_list)){
    ## success - add to global
    set_global(c(get_global(), the_list[[key]]))
    ## but there could also be some within
  } else {
    ## recursive
    lapply(the_list, recursive_key_finder, key = key)
  }
  
  get_global()
}

## environment hoops for globals
rmf_env <- new.env(parent = emptyenv())
rmf_env$a <- 1

get_global <- function() {
  rmf_env$a
}
set_global <- function(value) {
  old <- rmf_env$a
  rmf_env$a <- value
  invisible(old)
}


apply_json_props <- function(object_schema, id=NULL){
  lapply(names(object_schema), 
         function(x) {
           idx <- paste0(id,".",x)
           get_json_properties(object_schema[[x]], id = idx)
         })
}

descriptions <- function(x){
  attr(x, "description")
}

## extracts a vector of the attribute that lies within a list
## e.g. a <- list(a = 1, b = 2, c = list(blah = 22), d = list(blah = 33))
## returns c(22, 33)
extract_attribute <- function(y, 
                              attribute = "readOnly", 
                              type_out = logical(1)){
  vapply(names(y), 
         function(x) {
           prop <- y[[x]]
           if(!is.null(prop[[attribute]])) prop[[attribute]] else type_out
         }, 
         type_out)
}
