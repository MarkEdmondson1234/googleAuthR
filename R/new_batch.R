parse_batch_response <- function(batch_response){
  raw <- httr::content(batch_response, as="raw")
  lines <- strsplit(rawToChar(raw), "\r?\n")[[1]]
  
  # split on --batch boundaries
  new_response <- grepl("^--batch_", lines)
  grps <- cumsum(new_response)
  split_responses <- unname(split(lines, grps))
  # remove length 1 responses
  split_responses <- split_responses[vapply(split_responses, 
                                            function(x) length(x) !=1, 
                                            logical(1))]
  batch_response_bits <- lapply(split_responses, parse_single_response)
  
  # process individual batch parts
  batch_meta <- lapply(batch_response_bits, function(x) x$meta_header)
  
  o <- lapply(batch_response_bits, function(x){
    list(meta = x$meta_header,
         header = parse_http_headers(x$response_header),
         content = parse_response_content(x$response_content))
  })
  the_names <- vapply(batch_meta, 
                      function(x) x[grepl("content-id", x, ignore.case = TRUE)], 
                      character(1))
  the_names <- gsub("content-id: ", "", the_names, ignore.case = TRUE)
  names(o) <- the_names
  
  o
  
  
}

parse_response_content <- function(x){
  o <- jsonlite::fromJSON(x)
  if(!is.null(o$error)){
    myMessage("Error in batch response:", o$error$code, o$error$message, 
              level = 3)
  }
  o
}

parse_single_response <- function(lines){
  bits <- grepl("^(--batch_|HTTP|\\{)", lines)
  grps <- cumsum(bits)
  bit_grps <- unname(split(lines, grps))
  
  list(meta_header = bit_grps[[1]],
       response_header = bit_grps[[2]],
       response_content = bit_grps[[3]])
}


# https://github.com/r-lib/httr/blob/cb4e20c9e0b38c0c020a8756db8db7a882288eaf/R/insensitive.r
insensitive <- function(x) {
  names(x) <- tolower(names(x))
  structure(x, class = c("insensitive", class(x)))
}

# https://github.com/r-lib/httr/blob/cb4e20c9e0b38c0c020a8756db8db7a882288eaf/R/headers.r
parse_http_headers <- function(lines) {
  
  new_response <- grepl("^HTTP", lines)
  grps <- cumsum(new_response)
  
  lapply(unname(split(lines, grps)), parse_single_header)
}

parse_single_header <- function(lines) {
  status <- parse_http_status(lines[[1]])
  
  # Parse headers into name-value pairs
  header_lines <- lines[lines != ""][-1]
  pos <- regexec("^([^:]*):\\s*(.*)$", header_lines)
  pieces <- regmatches(header_lines, pos)
  
  n <- vapply(pieces, length, integer(1))
  if (any(n != 3)) {
    bad <- header_lines[n != 3]
    pieces <- pieces[n == 3]
    
    warning("Failed to parse headers:\n", paste0(bad, "\n"), call. = FALSE)
  }
  
  names <- vapply(pieces, "[[", 2, FUN.VALUE = character(1))
  values <- lapply(pieces, "[[", 3)
  headers <- insensitive(stats::setNames(values, names))
  
  list(status = status$status, version = status$version, headers = headers)
}

parse_http_status <- function(x) {
  status <- as.list(strsplit(x, "\\s+")[[1]])
  names(status) <- c("version", "status", "message")[seq_along(status)]
  status$status <- as.integer(status$status)
  
  
  status
}