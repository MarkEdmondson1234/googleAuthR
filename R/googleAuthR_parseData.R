#' Parsing search_analytics data
#' 
#' @param x the req$content response
#' @param dimensions the dimensions
#' @param prettyNames whether to translate the country codes

parse_search_analytics <- function(x, dimensions, prettyNames){
  # return(x)
  the_data <- x$rows
  
  # a bit of jiggery pokery (data processing)
  dimensionCols <- data.frame(Reduce(rbind, 
                                     lapply(the_data$keys, function(x) 
                                       rbind(x))), 
                              row.names=NULL, stringsAsFactors = F)
  
  ## if no rows, get out of here.
  if(!NROW(dimensionCols) > 0) return(the_data)
  
  names(dimensionCols ) <- dimensions
  dimensionCols <- lapply(dimensionCols, unname)
  
  if('date' %in% names(dimensionCols)){
    dimensionCols$date <- as.Date(dimensionCols$date)
  }
  
  if(all('country' %in% names(dimensionCols), prettyNames)){
    dimensionCols$countryName <- lookupCountryCode(dimensionCols$country)
  }
  
  metricCols <- the_data[setdiff(names(the_data), 'keys')]
  
  the_df <- data.frame(dimensionCols , metricCols, stringsAsFactors = F, row.names = NULL)
  attr(the_df, "aggregationType") <- x$responseAggregationType
  
  the_df
}
