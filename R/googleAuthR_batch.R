#' Turn a list of gar_fetch_functions into batch functions
#' 
#' 
#' @param call_list a list of functions from \code{\link{gar_api_generator}}
#' @param ... further arguments passed to the data parse function of f
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
#'   to do it in seperate batches to avoid confusion. 
#'  
#' @export
#' @family batch functions
gar_batch <- function(call_list, ...){
  
  # function_list <- lapply(call_list, eval)
  function_list <- call_list
  ## construct batch POST request
  parse_list <- lapply(function_list, makeBatchRequest)
  
  parsed <- paste(c(parse_list, "--gar_batch--"), collapse="")
  
  l <- list(parsed = parsed,
            shiny_access_token = function_list[[1]]$shiny_access_token)
  
  ## call doHttrRequest with batched together functions
  req <- doBatchRequest(l)
  batch_content <-  parseBatchResponse(req)
  
  parsed_batch_content <- lapply(function_list, applyDataParseFunction, batch_content, ...)
  myMessage("Batched API request successful", level=2)
  
  parsed_batch_content
  
}


#' Walk data through batches
#' 
#' Convienience function for walking through data in batches
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
#' 
#' @details
#' You can modify more than one parameter or path arg, 
#'   but it must be the same walked vector e.g. \code{start = end = x}
#'   
#' Many Google APIs have \code{batch_size} limits greater than 10, 1000 is common.
#'   
#' @return \strong{if data_frame_output is FALSE}: A list of lists.  
#'   Outer list the length of number of batches required, inner lists the results from the calls
#'   
#'   \strong{if data_frame_output is TRUE}: The list of lists will attempt to rbind all the results
#' 
#' @export
#' @family batch functions
gar_batch_walk <- function(f,
                           walk_vector,
                           gar_pars=NULL, gar_paths=NULL, the_body=NULL,
                           pars_walk=NULL, path_walk=NULL, body_walk=NULL, 
                           batch_size=10,
                           batch_function=NULL,
                           data_frame_output=TRUE,
                           ...){
  
  limit_batch <- split(walk_vector, ceiling(seq_along(walk_vector) / batch_size))

  myMessage("Batch API limited to [", batch_size ,"] calls at once.", level=2)
  
  ## lapply for each batch
  bl <- lapply(limit_batch, function(y){
    if(length(limit_batch) > 1) message("Request #: ", paste(y, collapse=" : "))
    ## lapply for each call in batch
    fl <- lapply(y, function(x){
      
      ## modify the arguments of f to include walked argument
      pars_walk_list <- lapply(pars_walk, function(z) z = x)
      names(pars_walk_list) <- pars_walk
      path_walk_list <- lapply(path_walk, function(z) z = x)
      names(path_walk_list) <- path_walk
      body_walk_list <- lapply(body_walk, function(z) z = x)
      names(body_walk_list) <- body_walk
      
      if(length(pars_walk) > 0) gar_pars <- utils::modifyList(gar_pars, pars_walk_list)
      if(length(path_walk) > 0) gar_paths <- utils::modifyList(gar_paths, path_walk_list)
      if(length(body_walk) > 0) the_body <- utils::modifyList(the_body, body_walk_list)      
      ## create the API call
      f(pars_arguments = gar_pars, 
        path_arguments = gar_paths, 
        the_body = the_body, 
        batch = TRUE)
    })
    names(fl) <- as.character(y)
    
    ## do the API call in batches
    # batch_data <- httr::with_verbose(googleAuthR::gar_batch(fl, ...))
    batch_data <- googleAuthR::gar_batch(fl, ...)
    
    if(!is.null(batch_function)) {
      batch_data <- batch_function(batch_data)
    } 
    batch_data
  })
  
  ## rbind all the dataframes if TRUE
  if(data_frame_output){
    myMessage("Binding dataframes into one.", level=1)
    the_data <- Reduce(rbind,
                       lapply(bl,
                              function(x) {
                                if(!inherits(x[[1]], "data.frame")){
                                  stop("Attempting to rbind a non dataframe output. 
                                       Set data_frame_output to FALSE?")
                                } else Reduce(rbind, x)
                                
                                })
                       )
  } else {
    the_data <- bl
  }

  the_data
}
  
  
  

#' Apply parsing function if a good response
#' 
#' @param function_entry a Google API function generated by \code{gar_api_generator}
#' @param the content returned from a batch call
#' 
#' @keywords internal
#' @family batch functions
applyDataParseFunction <- function(function_entry, batch_content, ...){
  
  x <- batch_content[paste0("response",function_entry$name)][[1]]
  
  id <- x$meta[[1]][2]
  status <- x$header[[1]][1]
  content <- x$content[[1]]
 
  ## check that its the right response 
  if(checkGoogleAPIError(content, batched = TRUE)){
    ## apply data parse function from function_list$data_parse_function    
    f <- function_entry$data_parse_function
    contentp <- f(content, ...)
    if(is.null(contentp)){
      warning("Error1: parsing data for:", id, " Returning unparsed content.")
      contentp <- content
    }
    
    contentp
    
  } else {
    warning("Error2: parsing data for:", id, " Returning unparsed content.")
    contentp <- content
  }

  contentp
  
}

#' Parse batch request
#' 
#' @param batch_response An element of the list of responses from a batched request
#' 
#' @keywords internal
#' @family batch functions
parseBatchResponse <- function(batch_response){

  
  b_content <- textConnection(httr::content(batch_response, as="text", encoding = "UTF-8"))
  r <- readLines(b_content)
  
  if(grepl("Error",r[1])) stop("Error in API response.  Got: ", r) 

  index <- which(grepl(r[1], r))
  responses <- split_vector(r, index)
  
  responses_content <- lapply(responses, function(x){
    # index <- which(grepl("^(\\{|\\})$", x))
    index <- which(grepl("Content-Length:", x))
    index <- c(index+1, length(x))
    if(any(is.na(index))){
      warning("Index has an NA. Not splitting JSON")
      return(unlist(split_vector(x, index, remove_splits = FALSE)))
    }
    jsonlite::fromJSON(unlist(split_vector(x, index, remove_splits = FALSE)))
  })
  
  responses_meta <- lapply(responses, function(x){
    index <- c(1:2)
    unlist(split_vector(x, index, remove_splits = FALSE))
  })
  
  responses_header <- lapply(responses, function(x){
    index <- which(grepl("HTTP|Content-Length", x))
    rh <- unlist(split_vector(x, index, remove_splits = FALSE))
    if(grepl("40", rh[2])){
      warning("400 type error in response")
    }
    if(grepl("50", rh[2])){
      warning("500 type error in response")
    }
    rh
    
  })
  

  
  batch_list <- lapply(1:length(responses), 
                       function(x) {
                         list(meta = responses_meta[x], 
                              header = responses_header[x], 
                              content = responses_content[x])
                         })
  names(batch_list) <- gsub("(Content-ID: )|-", "", Reduce(c, lapply(responses_meta, function(x) x[2])))

  
  batch_list
}


#' Make the batch request inner content
#' 
#' @param f The original unbatched Google API function call generated by \code{gar_api_generator}
#' 
#' @keywords internal
#' @family batch functions
makeBatchRequest <- function(f){

  
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
                    paste0("Content-ID: ",f$name),
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
#' 
#' @keywords internal
#' @family batch functions
doBatchRequest <- function(batched){
  
  arg_list <- list(config = get_google_token(batched$shiny_access_token), 
                   # url = "http://www.httpbin.org/post", 
                   url = "https://www.googleapis.com/batch", 
                   body = batched$parsed,
                   encode = "multipart",
                   httr::add_headers("Accept-Encoding" = "gzip"),
                   httr::user_agent("libcurl/7.43.0 r-curl/0.9.3 httr/1.0.0 googleAuthR/0.1.2 (gzip)"),
                   httr::add_headers("Content-Type" = "multipart/mixed; boundary=gar_batch")
                   )
  
  myMessage("Making Batch API call", level=2)
  req <- retryRequest(do.call("POST", 
                              args = arg_list,
                              envir = asNamespace("httr")))
  
  req
}