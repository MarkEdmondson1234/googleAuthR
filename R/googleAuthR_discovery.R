#' Create a Google API package
#' 
#' @param api_json json from \link{gar_discovery_api}
#' @param directory Where to build the package
#' @param rstudio Passed to \link[devtools]{create}, creates RStudio project file.
#' 
#' Uses \link[devtools]{create} to create a package structure then 
#'   \link{gar_create_api_skeleton} and \link{gar_create_api_objects} to create 
#'   starting files for a Google API package.
#' 
#' @seealso \href{https://developers.google.com/discovery/v1/reference/apis/list}
#'
#' @family Google Discovery API functions
#' @import devtools
#' @export
gar_create_package <- function(api_json, directory, rstudio = TRUE){
  
  package_name <- paste0("google",gsub("\\.","", make.names(api_json$id)),".auto")
  package_dir <- file.path(directory, package_name)
  message("Creating ", package_name, "in file location ", package_dir )
  
  devtools::create(file.path(directory, package_name),
                   list(
                     Package = package_name,
                     Version = "0.0.0.9000",
                     Title = api_json$title,
                     Description = paste0(api_json$description, " Auto-generated via googleAuthR"),
                     "Authors@R" = 'c(person("Mark", "Edmondson",email = "m@sunholo.com",
                  role = c("aut", "cre")))',
                     Imports = "googleAuthR (>= 0.3)"
                     ),
                   rstudio = rstudio)
  
  dir.create(file.path(package_dir, "R"))
  
  gar_create_api_skeleton(file.path(package_dir, "R", paste0(api_json$name, "_functions.R")), api_json)
  gar_create_api_objects(file.path(package_dir, "R", paste0(api_json$name,"_objects.R")), api_json)
  
  devtools::document(package_dir)
  ## use_readme_md
  ## user_github
  ## add_travis
  ## check
}


#' Get a list of Google API libraries
#' 
#' Doesn't require authentication
#' 
#' @seealso \href{https://developers.google.com/discovery/v1/reference/apis/list}
#' 
#' @return List of Google APIs and their resources
#' @family Google Discovery API functions
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
#' @family Google Discovery API functions
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
#' @family Google Discovery API functions
#' @export
gar_create_api_skeleton <- function(filename = "./inst/new_api.R", 
                                    api_json = gar_discovery_api("urlshortener","v1")
                                    ){
  

  if(is.null(api_json$kind) && api_json$kind != "discovery#restDescription"){
    stop("api_json not recognised from gar_discovery_api")
  }
  
  if(file.exists(filename)){
    warning("Overwriting file ", filename)
    file.remove(filename)
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
  
  invisible(formatR::tidy_eval(temp, file = filename, width.cutoff = 80))
  
}




#' Create the API objects from the Discovery API
#' 
#' @param filename File to write the objects to
#' @param api_json The json from \link{gar_discovery_api}
#' 
#' @return TRUE if successful, side-effect creating filename
#' @family Google Discovery API functions
#' @export
gar_create_api_objects <- function(filename = "./inst/api_objects.R", api_json){
  
  if(is.null(api_json$kind) && api_json$kind != "discovery#restDescription"){
    stop("api_json not recognised from gar_discovery_api")
  }
  
  if(file.exists(filename)){
    warning("Overwriting file ", filename)
    file.remove(filename)
  }
  
  temp <- tempfile()
  on.exit(file.remove(temp))

  
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
  
  invisible(formatR::tidy_eval(temp, file = filename, width.cutoff = 80))
  
}


