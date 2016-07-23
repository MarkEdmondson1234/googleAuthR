#' Get a list of Google API libraries
#' 
#' Doesn't require authentication
#' 
#' @seealso \href{https://developers.google.com/discovery/v1/reference/apis/list}
#' 
#' @return List of Google APIs and their resources
#' @export
gar_discovery_apis_list <- function(){
  
  url <- "https://www.googleapis.com/discovery/v1/apis"
  req <- httr::RETRY("GET", url)
  
  httr::stop_for_status(req)
  
  stuff <- httr::content(req, as = "text")
  apis <- jsonlite::fromJSON(stuff)
  
  if(!is.null(apis$kind) && apis$kind == "discovery#directoryList"){
    out <- apis$items
  } else {
    stop("Problem fetching Discovery APIs")
  }
  
  out
}

#' Get meta data details for specified Google API
#' 
#' Doesn't require authentication
#' 
#' @param api The API to fetch
#' @param version The API version to fetch
#' 
#' @seealso \href{https://developers.google.com/discovery/v1/reference/apis/getRest}
#' 
#' @return Details of the API 
#' @export
gar_discovery_api <- function(api, version){
  
  url <- sprintf("https://www.googleapis.com/discovery/v1/apis/%s/%s/rest",
                 api,
                 version)
  
  req <- httr::RETRY("GET", url)
  
  httr::stop_for_status(req)
  
  stuff <- httr::content(req, as = "text")
  api <- jsonlite::fromJSON(stuff)
  
  if(!is.null(api$kind) && api$kind == "discovery#restDescription"){
    out <- api
  } else {
    stop("Problem fetching API Description")
  }
  
  out
}

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


#' Create an API library skeleton
#' 
#' This will create a file with the skeleton of the API functions 
#'   for the specified library
#' 
#' @param filename R file to write skeleton to
#' @param api_json The json from \link{gar_discovery_api}
#' 
#' @return TRUE if successful, side effect will write a file
#' @export
gar_create_api_skeleton <- function(filename = "./R/new_api.R", 
                                    api_json = gar_discovery_api("urlshortener","v1")){
  
  stopifnot(api_json$kind == "discovery#restDescription")
  
  if(file.exists(filename)){
    stop("File: ", filename, " exists. Delete it and run again")
  }
  
  header <- paste0(
      "#' ", api_json$title, "\n",
      "#' ", api_json$description, "\n",
      "#' \n",
      "#' @docType package \n",
      "#' @name ", paste0(api_json$name,"_googleAuthR"), "\n",
      "#' \n",
      "NULL"
      )
  
  add_line(header, filename)
  
  fb <- lapply(api_json$resources$url$methods, function_body, api_json = api_json)

  lapply(fb, add_line, filename)
}

function_docs <- function(api_json_resource_method, api_json){
  
}

function_params <- function(api_json_resource_method, api_json){
  
}

function_body <- function(api_json_resource_method, api_json){
  
  path_list <- make_pathpars_list(api_json_resource_method, "path")
  pars_list <- make_pathpars_list(api_json_resource_method, "query")
  
  fb <- paste0(
    "url <- '", paste0(api_json$baseUrl, api_json_resource_method$path), "'\n",
    api_json_resource_method$id, " <- gar_api_generator(url, \n",
    "\t\t\t\t'",api_json_resource_method$httpMethod,"'\n",
    "\t\t\t\t", path_list,
    "\t\t\t\t", pars_list,
    "\t\t\t\t", "data_parse_function = function(x) x)\n\n",
    api_json_resource_method$id,"()"
  )
  
  fb
  
}

make_pathpars_list <- function(api_json_resource_method, 
                               type = c("path", "query")){
  
  type <- match.arg(type)
  
  if(is.null(api_json_resource_method$parameters)) return(NULL)

  out <- switch(type,
      path = paste0(
        "path_args = list(\n",
        paste(collapse = ",\n", 
              lapply(names(api_json_resource_method$parameters), make_path_pars_entry, "path", api_json_resource_method=api_json_resource_method)
              ),
        ")\n"
      ),
      query = paste0(
        "pars_args = list(\n",
        paste(collapse = ",\n", 
              lapply(names(api_json_resource_method$parameters), make_path_pars_entry, "query", api_json_resource_method=api_json_resource_method)
              ),
        ")\n" 
      )
      )
  
  out
  
}

make_path_pars_entry <- function(x, 
                                 type = c("path", "query"),
                                 api_json_resource_method){
  
  type <- match.arg(type)

  if(api_json_resource_method$parameters[[x]]$location == type){
    out <- paste0("\t\t",x, " = ", x, "\n", collapse = "")
  } else {
    out <- NULL
  }
  
  out
  
}