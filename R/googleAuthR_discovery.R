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
gar_create_api_skeleton <- function(filename = "./inst/new_api.R", 
                                    api_json = gar_discovery_api("urlshortener","v1")
                                    ){
  

  if(is.null(api_json$kind) && api_json$kind != "discovery#restDescription"){
    stop("api_json not recognised from gar_discovery_api")
  }
  
  if(file.exists(filename)){
    stop("File: ", filename, " exists. Delete it and run again")
  }
  
  temp <- tempfile()
  on.exit(file.remove(temp))
  
  header <- paste0(
      "#' ", api_json$title, "\n",
      "#' ", api_json$description, "\n",
      "#' \n",
      "#' Auto-generated code by googleAuthR::gar_create_api_skeleton\n",
      "#'  at ", as.character(Sys.time()), "\n",
      "#' filename: ", substitute(filename),"\n",
      "#' api_json: ", paste(substitute(api_json), collapse = " "),"\n",
      "#' \n",
      "#' @details \n",
      "#' Authentication scopes used are:\n",
      "#' \\itemize{\n",
      "#'   \\item ", paste(names(api_json$auth$oauth2$scopes), collapse = "\n#' \\item "),
      "\n#' }\n",
      "#' \n",
      "#' @docType package \n",
      "#' @name ", paste0(api_json$name,"_googleAuthR"), "\n",
      "#' \n",
      "NULL",
      "\n"
      )
  
  add_line(header, temp)
  
  ## apis can have methods at varying steps into JSON tree
  ## find the methods to extract into API
  api_method <- get_json_methods(api_json$resources)

  fd <- lapply(api_method, function_docs, api_json = api_json)
  fp <- lapply(api_method, function_params, api_json = api_json)
  fb <- lapply(api_method, function_body, api_json = api_json)

  lapply(paste(fd, fp,fb, sep = "\n\n"), add_line, temp)
  
  formatR::tidy_eval(temp, file = filename, width.cutoff = 80)
  
}




#' Create the API objects from the Discovery API
#' 
#' @param filename File to write the objects to
#' @param api_json The json from \link{gar_discovery_api}
#' 
#' @return TRUE if successful, side-effect creating filename
#' @export
gar_create_api_objects <- function(filename = "./inst/api_objects.R", api_json){
  
  if(is.null(api_json$kind) && api_json$kind != "discovery#restDescription"){
    stop("api_json not recognised from gar_discovery_api")
  }
  
  if(file.exists(filename)){
    stop("File: ", filename, " exists. Delete it and run again")
  }
  
  temp <- tempfile()
  on.exit(file.remove(temp))
  # temp <- filename
  
  header <- paste0(
    "#' ", api_json$title, " Objects \n",
    "#' ", api_json$description, "\n",
    "#' \n",
    "#' Auto-generated code by googleAuthR::gar_create_api_objects\n",
    "#'  at ", as.character(Sys.time()), "\n",
    "#' filename: ", substitute(filename),"\n",
    "#' api_json: ", paste(substitute(api_json), collapse = " "),"\n",
    "#' \n",
    "#' Objects for use by the functions created by googleAuthR::gar_create_api_skeleton\n",
    "\n"
  )
  
  add_line(header, temp)
  
  ## take the json and create a file of structures that will get passed to the functions that need them
  object_schema <- api_json$schemas
  set_global(NULL)
  apply_json_props(object_schema)
  properties <- get_global()
  set_global(list())
  
  fd <- lapply(names(properties), object_docs, properties)
  fp <- lapply(names(properties), object_params, properties)
  fb <- lapply(names(properties), object_body, properties)
  
  lapply(paste(fd, fp,fb, sep = "\n\n"), add_line, temp)
  
  formatR::tidy_eval(temp, file = filename, width.cutoff = 80)
  
}


