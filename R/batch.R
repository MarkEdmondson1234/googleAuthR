#' Turn a list of gar_fetch_functions into batch functions
#' 
#' 
#' @param function_list a list of functions from \code{\link{gar_api_generator}}
#' @param ... further arguments passed to the data parse function of f
#' @param batch_endpoint the batch API endpoint to send to
#' 
#' @return A list of the Google API responses
#' 
#' @seealso https://developers.google.com/webmaster-tools/v3/how-tos/batch
#' 
#'   Documentation on doing batch requests for the search console API.  
#'   Other Google APIs are similar.
#'   
#'   Walk through API calls changing parameters using \code{\link{gar_batch_walk}}
#'   
#' @details This function will turn all the individual Google API functions
#'   into one POST request to /batch. 
#'   
#' If you need to pass multiple data parse function arguments its probably best 
#'   to do it in separate batches to avoid confusion. 
#'  
#' @export
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' ## usually set on package load
#' options(googleAuthR.batch_endpoint = "https://www.googleapis.com/batch/urlshortener/v1")
#' 
#' ## from goo.gl API
#' shorten_url <- function(url){
#'   body = list(longUrl = url)
#'   f <- gar_api_generator("https://www.googleapis.com/urlshortener/v1/url",
#'                          "POST",
#'                           data_parse_function = function(x) x$id)
#'                         
#'   f(the_body = body)
#' }
#' 
#' 
#' ## from goo.gl API
#' user_history <- function(){
#'   f <- gar_api_generator("https://www.googleapis.com/urlshortener/v1/url/history",
#'                       "GET",
#'                       data_parse_function = function(x) x$items)
#'                       
#'   f()
#' }
#' 
#' gar_batch(list(shorten_url("http://markedmondson.me"), user_history()))
#' 
#' }
#' @family batch functions
#' @importFrom httr content
gar_batch <- function(function_list, 
    ..., 
    batch_endpoint = getOption("googleAuthR.batch_endpoint", 
                                default = "https://www.googleapis.com/batch")){
  
  
  ## construct batch POST request
  parse_list <- lapply(function_list, makeBatchRequest)
  
  parsed <- paste(c(parse_list, "--gar_batch--"), collapse="")
  
  l <- list(parsed = parsed)
  
  ## call doHttrRequest with batched together functions
  cached_call <- !is.null(gar_cache_get_loc())
  if(cached_call){
    req <- memDoBatchRequest(l, batch_endpoint = batch_endpoint)
  } else {
    req <- doBatchRequest(l, batch_endpoint = batch_endpoint)
  }
  
  if(req$status_code == 404){
    stop("Batch Request: 404 Not Found", call. = FALSE)
  }

  batch_content <-  tryCatch(parse_batch_response(req),
                             error = function(ee){
                               write_error_req(ee, req)
                             })
  
  # checks
  test_content <- batch_content[[1]]$content
  
  if(!is.null(test_content$error$message)){
    stop(test_content$error$message, call. = FALSE) 
  }
  
  if(length(test_content) < 1){
    myMessage("No content in batched response", level = 2)
    return(NULL)
  }
  
  # apply data parsing to responses
  ## now only one function allowed, use only first function's data_parse
  f <- function_list[[1]]$data_parse_function

  batch_content_content <- lapply(batch_content, function(x) x$content[[1]])
  
  ## apply data parse function from function_list$data_parse_function
  lapply(batch_content_content, f, ... = ...)

}

write_error_req <- function(err, req){
  if(getOption("googleAuthR.verbose") < 3){
    tmp <- tempfile(fileext = ".rds")
    saveRDS(req, file = tmp)
    message("Error with batch response - writing response to ",tmp)
  }
  
  stop(err$message)
}


#' Walk data through batches
#' 
#' Convenience function for walking through data in batches
#' @param f a function from \code{\link{gar_api_generator}}
#' @param walk_vector a vector of the parameter or path to change
#' @param gar_pars a list of parameter arguments for f
#' @param gar_paths a list of path arguments for f
#' @param the_body a list of body arguments for f
#' @param pars_walk a character vector of the parameter(s) to modify for each walk of f
#' @param path_walk a character vector of the path(s) to modify for each walk of f
#' @param body_walk a character vector of the body(s) to modify for each walk of f
#' @param batch_size size of each request to Google /batch API
#' @param batch_function a function that will act on the result list of each batch API call
#' @param data_frame_output if the list of lists are dataframes, you can bind them all by setting to TRUE
#' @param ... further arguments passed to the data parse function of f
#' @param batch_endpoint the batch API endpoint to send
#' 
#' @details
#' You can modify more than one parameter or path arg, 
#'   but it must be the same walked vector e.g. \code{start = end = x}
#'   
#' Many Google APIs have \code{batch_size} limits greater than 10, 1000 is common.
#' 
#' The `f` function needs to be a `gar_api_generator()` function that uses one of `path_args`, `pars_args` or `body_args` to construct the URL (rather than say using `sprintf()` to create the API URL). 
#' 
#' You don't need to set the headers in the Google docs for batching API functions - those are done for you.
#' 
#' The argument `walk_vector` needs to be a vector of the values of the arguments to walk over, which you indicate will walk over the pars/path or body arguments on the function via on of the `*_walk` arguments e.g. if walking over id=1, id=2, for a path argument then it would be `path_walk="id"` and `walk_vector=c(1,2,3,4)`
#' 
#' The `gar_*` parameter is required to pass intended for other arguments to the function `f` you may need to pass through.
#' 
#' `gar_batch_walk()` only supports changing one value at a time, for one or multiple arguments (I think only changing the `start-date`, `end-date` example would be the case when you walk through more than one per call)
#' 
#' `batch_size` should be over 1 for batching to be of any benefit at all
#' 
#' The `batch_function` argument gives you a way to operate on the parsed output of each call
#'   
#' @return \strong{if data_frame_output is FALSE}: A list of lists.  
#'   Outer list the length of number of batches required, inner lists the results from the calls
#'   
#'   \strong{if data_frame_output is TRUE}: The list of lists will attempt to rbind all the results
#' 
#' @export
#' 
#' @examples 
#' 
#' \dontrun{
#'                                
#'
#' # get a webproperty per account 
#' getAccountInfo <- gar_api_generator(
#'   "https://www.googleapis.com/analytics/v3/management/accounts",
#'   "GET", data_parse_function = function(x) unique(x$items$id))
#' 
#' getWebpropertyInfo <- gar_api_generator(
#'   "https://www.googleapis.com/analytics/v3/management/", # don't use sprintf to construct this
#'   "GET",
#'   path_args = list(accounts = "default", webproperties = ""),
#'   data_parse_function = function(x) x$items)
#' 
#' walkData <- function(){
#' 
#'   # here due to R lazy evaluation  
#'   accs <- getAccountInfo()
#'   gar_batch_walk(getWebpropertyInfo, 
#'                  walk_vector = accs,
#'                  gar_paths = list("webproperties" = ""),
#'                  path_walk = "accounts",
#'                  batch_size = 100, data_frame_output = FALSE)
#'                  }
#'                  
#' # do the walk
#' walkData()
#' 
#' # to walk body data, be careful to modify a top level body name:
#' changed_emails <- lapply(email, function(x){userRef = list(email = x)})
#' 
#' batched <- gar_batch_walk(users, 
#'                        walk_vector = changed_emails, 
#'                        the_body = list(
#'                            permissions = list(
#'                              local = list(permissions)
#'                            ),
#'                            userRef = list(
#'                              email = email[[1]]
#'                          )
#'                         ),
#'                         body_walk = "userRef",
#'                         batch_size = 300,
#'                         data_frame_output = FALSE)
#' }
#' 
#' 
#' 
#' @family batch functions
#' @importFrom utils modifyList
gar_batch_walk <- function(f,
    walk_vector,
    gar_pars=NULL, gar_paths=NULL, the_body=NULL,
    pars_walk=NULL, path_walk=NULL, body_walk=NULL, 
    batch_size=10,
    batch_function=NULL,
    data_frame_output=TRUE,
    ...,
    batch_endpoint = getOption("googleAuthR.batch_endpoint", 
                               default = "https://www.googleapis.com/batch")){
  
  ## A batch_group can hold several batch_index of size batch_size
  batch_groups <- split(walk_vector, 
                       ceiling(seq_along(walk_vector) / batch_size))

  myMessage("Batch API limited to [", batch_size ,"] calls at once.", level=2)

  ## A batch_group can hold several batch_index of size batch_size
  bl <- batch_group_requests(
    batch_groups = batch_groups, 
    f = f, 
    pars_walk = pars_walk, 
    path_walk = path_walk, 
    body_walk = body_walk, 
    gar_pars = gar_pars, 
    gar_paths = gar_paths, 
    the_body = the_body, 
    ... = ..., 
    batch_endpoint = batch_endpoint, 
    batch_function = batch_function)
  
  ## rbind all the dataframes if TRUE
  if(data_frame_output){
    myMessage("Binding dataframes into one.", level=1)
    the_data <- bind_dataframe_list(bl)
  } else {
    the_data <- bl
  }

  the_data
}



batch_group_requests <- function(batch_groups, f, 
    pars_walk, path_walk, body_walk, 
    gar_pars, gar_paths, the_body, 
    ..., batch_endpoint, batch_function) {
  
  ## A batch_group can hold several batch_index of size batch_size
  lapply(batch_groups, function(batch_group){
    if(length(batch_group) > 1){
      myMessage("Request #: ", paste(batch_group, collapse=" : "), level = 3)
    }

    ## make request for each API call in this batch_group
    fl <- make_batch_requests(batch_group, f, 
          pars_walk = pars_walk,path_walk = path_walk,body_walk = body_walk,
          gar_pars = gar_pars,gar_paths = gar_paths,the_body = the_body)
    
    ## do the API calls within the batch_group
    batch_data <- gar_batch(fl, ..., batch_endpoint = batch_endpoint)
    
    # apply a function across this batch_group responses
    if(!is.null(batch_function)) {
      batch_data <- batch_function(batch_data)
    } 
    batch_data
  })
}

bind_dataframe_list <- function(dfs_list){
  myMessage("Binding dataframes into one.", level=1)
  are_dfs <- vapply(dfs_list, is.data.frame, logical(1))
  if(!all(are_dfs)){
    stop("Not all elements are data.frames. Set data_frame_output to FALSE?",
         call. = FALSE)
  }
  
  Reduce(rbind, dfs_list)
}

make_batch_requests <- function(batch_index, f,
    pars_walk=NULL,path_walk=NULL,body_walk=NULL,
    gar_pars = NULL,gar_paths = NULL,the_body=NULL){
  
  fl <- lapply(batch_index, function(x){
    
    ## modify the arguments of f to include walked argument
    pars_walk_list <- make_walk_list(pars_walk, x)
    path_walk_list <- make_walk_list(path_walk, x)
    body_walk_list <- make_walk_list(body_walk, x)
    
    #if(length(pars_walk) > 0) gar_pars  <- modifyList(gar_pars, pars_walk_list)

    gar_pars  <- modify_walk_list(pars_walk, gar_pars, pars_walk_list)
    gar_paths <- modify_walk_list(path_walk, gar_paths, path_walk_list)
    the_body  <- modify_walk_list(body_walk, the_body, body_walk_list)
    
    ## create the API call
    f(pars_arguments = gar_pars, 
      path_arguments = gar_paths, 
      the_body = the_body, 
      batch = TRUE)
  })
  names(fl) <- as.character(batch_index)  

  fl
}

make_walk_list <- function(walk, this){
  o <- lapply(walk, function(x) x = this)
  names(o) <- walk
  
  o
}

modify_walk_list <- function(walk, original, change){
  if(is.null(walk) || length(walk) == 0) return(original)
  
  modifyList(original, change)
}


#' Make the batch request inner content
#' 
#' @param f The original unbatched Google API function call generated by \code{gar_api_generator}
#' 
#' @keywords internal
#' @family batch functions
#' @importFrom digest digest
#' @noRd
makeBatchRequest <- function(f){
  
  # if(!is.gar_function(f)){
  #   stop("Passed function f is not of class gar_function (not generated by gar_api_generator?)", 
  #        call. = FALSE)
  # }
  
  boundary <- "--gar_batch"
  url_stem <- gsub("https://www.googleapis.com","",f$req_url)

  myMessage("Constructing batch request URL for: ", url_stem, level = 2)  
  
  ## construct batch POST request
  req <- paste0("\r\n",
                f$http_header," ", 
                url_stem)
  
  if(!is.null(f$the_body)){
    batch_body <- jsonlite::toJSON(f$the_body, auto_unbox = TRUE)
    myMessage("Batch Body JSON parsed to:", batch_body, level=1)
    part_content_length <- nchar(batch_body, type="bytes")
    
    header <- paste(boundary,
                    "Content-Type: application/http",
                    # paste0("Content-ID: ",f$name),
                    paste0("Content-ID: ", digest(c(batch_body, Sys.time()))),
                    sep = "\r\n")
    body_header <- paste(req,
                         "Content-Type: application/json",
                         paste("Content-Length: ", part_content_length),
                         "\r\n",
                         sep="\r\n")

    parsed <- paste(header, body_header, batch_body, "\r\n", sep = "\r\n")
    
  } else {

    header <- paste(boundary,
                    "Content-Type: application/http",
                    paste0("Content-ID: ",f$name),
                    sep = "\r\n")
    parsed <- paste(header, req,"\r\n", sep = "\r\n")

  }
  
  parsed
  
}

#' Batch Requests
#' 
#' @description
#' Batch requests to Google APIs that support it
#' 
#' @param batched an element of a list of parsed batch requests
#' @param batch_endpoint the batch API endpoint to send to
#' 
#' @keywords internal
#' @family batch functions
#' @importFrom httr add_headers user_agent 
#' @noRd
doBatchRequest <- function(batched, 
                           batch_endpoint){
  
  if(batch_endpoint == "https://www.googleapis.com/batch"){
    stop("Deprecated batch endpoint being used.  Use options('googleAuthR.batch_endpoint') to select 
          specific endpoint for this API. See 
         http://code.markedmondson.me/googleAuthR/articles/advanced-building.html#batching-api-requests", 
         call. = FALSE)  
  }
  
  arg_list <- list(url = batch_endpoint, 
                   config = get_google_token(), 
                   body = batched$parsed,
                   encode = "multipart",
                   add_headers("Accept-Encoding" = "gzip"),
                   user_agent(paste0("googleAuthR/",packageVersion("googleAuthR"), " (gzip)")),
                   add_headers("Content-Type" = "multipart/mixed; boundary=gar_batch")
                   )
  
  myMessage("Making Batch API call", level=2)
  
  # ensure batch requests only occur per second to help calculation of QPS limits
  Sys.sleep(1)
  
  req <- retryRequest(do.call("POST", 
                              args = arg_list,
                              envir = asNamespace("httr")))
  
  req
  
}