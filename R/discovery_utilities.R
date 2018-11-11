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
  if(!inherits(arguments, "character")){
    ## a list for function methods
    defaults <- vapply(arguments, function(x) if(is.null(x[["required"]])) "= NULL" else "", character(1))
  } else {
    ## an api object - nothing is required
    defaults <- rep("= NULL", length(arguments))
  }

  paste0(
    f_name, " <- function(",
    paste(paste(make.names(names(arguments)), defaults), sep="", collapse = ","),
    "){\n"
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
  
  order_names <- c(api_json_resource_method$parameterOrder, 
                   setdiff(names(api_json_resource_method$parameters), 
                           api_json_resource_method$parameterOrder))
  
  unlist(lapply(order_names, function(x) if(pars[[x]]$location == type) x))
  # unlist(lapply(names(pars), function(x) if(pars[[x]]$location == type) x))
  
}

make_pars_list <- function(api_json_resource_method){
  
  query_pars <- type_parameters(api_json_resource_method, "query")
  
  if(length(query_pars) == 0) return(NULL)
  
  list_vars <- lapply(query_pars, 
                      make_pars_entry,
                      api_json_resource_method = api_json_resource_method)
  paste0(
    "pars = list(\n",
    paste(collapse = ",", list_vars),
    ")\n" 
  )
  
}

make_pars_entry <- function(x, api_json_resource_method){
  paste0("\t\t`",x, "` = ", make.names(x), "\n", collapse = "")
}

make_vars_description <- function(x, api_json_resource_method){
  
  desc <- first_sentence(api_json_resource_method[[x]]$description)
  
  paste0("@param ", make.names(x), " ", desc,"\n", 
         collapse = "")
  
}

# get the methods
get_json_methods <- function(api_json_resources){
  
  set_global(list())
  lapply(api_json_resources, recursive_key_finder, key = "methods")
  out <- get_global()
  set_global(list())
  out
}

recursive_key_finder <- function(the_list, key = "methods"){

  if(key %in% names(the_list)){
    ## success - add to global
    set_global(c(get_global(), the_list[[key]]))
    ## but there could also be some within
  }
  new_list <- the_list
  new_list[[key]] <- NULL
  ## recursive
  lapply(new_list, recursive_key_finder, key = key)
  
  invisible(NULL)
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

# recursive property objects
get_json_properties <- function(api_json_schema, id=NULL){
  
  if(is.null(api_json_schema$type)) return()
  
  type <- api_json_schema$type
  id <- if(is.null(api_json_schema$id)) id else api_json_schema$id
  
  if(type == "object"){
    ## return this level properties (and additionalProperties ?)
    
    build_entry(api_json_schema, id)
    
    ## go deeper in recursion
    apply_json_props(api_json_schema$properties, id = id)
    
  } else if(type == "array"){
    
    array_item <- api_json_schema$items
    
    if(is.null(array_item$properties)) return()

    ## return this level properties (and additionalProperties ?)
    build_entry(array_item$properties, id)
    
    ## go deeper in recursion
    apply_json_props(array_item$properties, id = id)
    
  } else if(type == "string"){
    
  }
  
  return(id)
}

build_entry <- function(api_json_schema, id){
  ## only those dimensions that aren't readOnly
  # find readOnly properties:
  readOnlyPos <- extract_attribute(api_json_schema$properties,
                                   "readOnly",
                                   logical(1))
  
  # find defaults properties:
  defaults <- extract_attribute(api_json_schema$properties,
                                "default",
                                character(1))
  
  # find descriptions of properties:
  descriptions <- extract_attribute(api_json_schema$properties,
                                    "description",
                                    character(1))
  
  ## readOnly props with defaults and description
  entry <- list(value = defaults[names(defaults)[!readOnlyPos]],
                description = descriptions[names(descriptions)[!readOnlyPos]])
  
  ## make the entry with a description and name attribute
  out <- list(c(entry, 
                description_api = api_json_schema$description))
  
  ## append to global list
  names(out) <- id
  set_global(c(get_global(), out))
}

first_sentence <- function(string){
  
  vapply(string, 
         function(x) {
           if(nchar(x) == 0) return("")
           unlist(strsplit(x, "\\.|\n"))[[1]]
         }, 
         character(1))
}

## return names of any children of this property_name
get_object_children <- function(property_name, properties){
  names(properties)[grepl(paste0("^",property_name,"\\."), names(properties))]
}