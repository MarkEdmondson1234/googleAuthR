#' Query search traffic keyword data
#' 
#' @description Download your Google keyword SEO data.
#' 
#' @param siteURL The URL of the website you have auth access to.
#' @param startDate Start date of requested range, in YYYY-MM-DD.
#' @param endDate End date of the requested date range, in YYYY-MM-DD.
#' @param dimensions Zero or more dimensions to group results by: "date", "country", "device", "page" or "query"
#' @param searchType Search type filter, default 'web'.
#' @param dimensionFilterExp A character vector of expressions to filter. e.g. c("device==TABLET", "country~~GBR")
#' @param aggregationType How data is aggregated.
#' @param rowLimit How many rows, maximum is 5000.
#' @param prettyNames If TRUE, converts SO 3166-1 alpha-3 country code to full name and creates new column called countryName.
#' @param shiny_access_token If in a Shiny session, supply its shiny_access_token object.
#' 
#' @return A dataframe with columns in order of dimensions plus metrics, with attribute "aggregationType"
#' 
#' @family search analytics
#' @seealso Guide to Search Analytics: \url{https://support.google.com/webmasters/answer/6155685}
#'   API docs: \url{https://developers.google.com/webmaster-tools/v3/searchanalytics/query}
#' @export
#' 
#' @details 
#'  \strong{startDate}: Start date of the requested date range, in YYYY-MM-DD format, 
#'    in PST time (UTC - 8:00). Must be less than or equal to the end date. 
#'    This value is included in the range.
#'    
#'  \strong{endDate}: End date of the requested date range, in YYYY-MM-DD format, 
#'    in PST time (UTC - 8:00). Must be greater than or equal to the start date. 
#'    This value is included in the range.
#'    
#'  \strong{dimensions}: [Optional] Zero or more dimensions to group results by. 
#'       \itemize{
#'         \item 'date'
#'         \item 'country'
#'         \item 'device'
#'         \item 'page'
#'         \item 'query'
#'       }
#'  The grouping dimension values are combined to create a unique key 
#'    for each result row. If no dimensions are specified, 
#'    all values will be combined into a single row. 
#'    There is no limit to the number of dimensions that you can group by, 
#'    but you cannot group by the same dimension twice. Example: c(country, device)
#'  
#'  \strong{dimensionFilterExp}:
#'  Results are grouped in the order that you supply these dimensions. 
#'  dimensionFilterExp expects a character vector of expressions in the form:
#'   ("device==TABLET", "country~~GBR", "dimension operator expression")
#'   \itemize{
#'     \item dimension  
#'       \itemize{
#'         \item 'country'
#'         \item 'device'
#'         \item 'page'
#'         \item 'query'
#'       }
#'     \item operator 
#'       \itemize{
#'         \item '~~' meaning 'contains'
#'         \item '==' meaning 'equals'
#'         \item '!~' meaning 'notContains'
#'         \item '!=' meaning 'notEquals
#'       }
#'     
#'     \item expression 
#'        \itemize{
#'          \item country: an ISO 3166-1 alpha-3 country code.
#'          \item device: 'DESKTOP','MOBILE','TABLET'.
#'          \item page: not checked, a string in page URLs without hostname.
#'          \item query: not checked, a string in keywords.
#'        
#'        }
#'   }
#'  
#'  
#'  \strong{searchType}: [Optional] The search type to filter for. Acceptable values are:
#'  \itemize{
#'    \item "web": [Default] Web search results
#'    \item "image": Image search results
#'    \item "video": Video search results
#'  }
#'  
#'  \strong{aggregationType}: [Optional] How data is aggregated. 
#'  \itemize{
#'    \item If aggregated by property, all data for the same property is aggregated; 
#'    \item If aggregated by page, all data is aggregated by canonical URI. 
#'    \item If you filter or group by page, choose auto; otherwise you can aggregate either by property or by page, depending on how you want your data calculated; 
#'  }
#'    See the API documentation to learn how data is calculated differently by site versus by page. 
#'    Note: If you group or filter by page, you cannot aggregate by property.
#'    If you specify any value other than auto, the aggregation type in the result will match the requested type, or if you request an invalid type, you will get an error. 
#'    The API will never change your aggregation type if the requested type is invalid.
#'    Acceptable values are:
#'  \itemize{
#'    \item "auto": [Default] Let the service decide the appropriate aggregation type.
#'    \item "byPage": Aggregate values by URI.
#'    \item "byProperty": Aggregate values by property.
#'  }
#'  
search_analytics <- function(siteURL, 
                             startDate, endDate, 
                             dimensions = NULL, 
                             searchType = c("web","video","image"),
                             dimensionFilterExp = NULL,
                             aggregationType = c("auto","byPage","byProperty"),
                             rowLimit = 1000,
                             prettyNames = TRUE,
                             shiny_access_token=NULL){
  
  searchType      <- match.arg(searchType)
  aggregationType <- match.arg(aggregationType)
  
  siteURL <- check.Url(siteURL)
  
  startDate <- as.character(startDate)
  endDate   <- as.character(endDate)
  
  if(any(is.na(as.Date(startDate, "%Y-%m-%d")), is.na(as.Date(endDate, "%Y-%m-%d")))){
    stop("dates not in correct %Y-%m-%d format. Got these:", startDate, " - ", endDate)
  }
  
  if(!is.null(dimensions) && !dimensions %in% c('date','country', 'device', 'page', 'query')){
    stop("dimension must be NULL or one or more of 'date','country', 'device', 'page', 'query'. 
         Got this: ", paste(dimensions, sep=", "))
  }
  
  if(!searchType %in% c("web","image","video")){
    stop('searchType not one of "web","image","video".  Got this: ', searchType)
  }
  
  
  if(!aggregationType %in% c("auto","byPage","byProperty")){
    stop('aggregationType not one of "auto","byPage","byProperty". Got this: ', aggregationType)
  }
  
  if(aggregationType %in% c("byProperty") && 'page' %in% dimensions ){
    stop("Can't aggregate byProperty and include page in dimensions.")
  }
  
  
  if(rowLimit > 5000){
    stop("rowLimit must be 5000 or lower. Got this: ", rowLimit)
  }
  
  
  ## a list of filter expressions 
  ## expects dimensionFilterExp like c("device==TABLET", "country~~GBR")
  parsedDimFilterGroup <- lapply(dimensionFilterExp, gsc.parseDimFilterGroup)
  
  body <- list(
    startDate = startDate,
    endDate = endDate,
    dimensions = as.list(dimensions),  
    searchType = searchType,
    dimensionFilterGroups = list(
      list( ## you don't want more than one of these until different groupType available
        groupType = "and", ##only one available for now
        filters = parsedDimFilterGroup
      )
    ),
    aggregationType = aggregationType,
    rowLimit = rowLimit
  )
  
  search_analytics_g <- googleAuth_fetch_generator("https://www.googleapis.com/webmasters/v3/",
                                                   "POST",
                                                   path_args = list(sites = "siteURL",
                                                                    searchAnalytics = "query"),
                                                   ,
                                                   data_parse_function = function(x) {
                                                     parse_search_analytics(x, 
                                                                            dimensions, 
                                                                            prettyNames)
                                                     })
  result <- search_analytics_g(path_arguments = list(sites = check.Url(siteURL)),
                               the_body = body)
  
  result
}


#' Retrieves dataframe of websites user has in Search Console
#'
#' @param shiny_access_token If in a Shiny session, supply its shiny_access_token object.
#' 
#' @return a dataframe of siteUrl and permissionLevel
#'
#' @export
#' @family search console website functions
list_websites <- googleAuth_fetch_generator("https://www.googleapis.com/webmasters/v3/",
                                              "GET",
                                              path_args = list(sites = ""),
                                              data_parse_function = function(x) x$siteEntry)

#' Adds website to Search Console
#' 
#' @param siteURL The URL of the website to add.
#'
#' @return TRUE if successful, raises an error if not.
#' @family search console website functions
#' 
#' @export
add_website <- function(siteUrl){
  f <- googleAuth_fetch_generator("https://www.googleapis.com/webmasters/v3/",
                                            "PUT",
                                            path_args = list(sites = "siteURL"))
  f(path_arguments=list(sites=check.Url(siteUrl)))
}
 
#' Deletes website in Search Console
#' 
#' @param siteURL The URL of the website to delete.
#' @param shiny_access_token If in a Shiny session, supply its shiny_access_token object.
#' 
#' @return TRUE if successful, raises an error if not.
#' @family data fetching functions
#' 
#' @export
#' 
#' @family search console website functions
delete_website <- function(siteUrl) {
  
  f <- googleAuth_fetch_generator("https://www.googleapis.com/webmasters/v3/",
                                  "DELETE",
                                  path_args = list(sites = "siteURL"))
  f(path_arguments=list(sites=check.Url(siteUrl)))
  
}

#' Gets sitemap information for the URL supplied.
#' 
#' See here for details: https://developers.google.com/webmaster-tools/v3/sitemaps 
#' 
#' @param siteURL The URL of the website to get sitemap information from. Must include protocol (http://).
#' @param shiny_access_token If in a Shiny session, supply its shiny_access_token object.
#' 
#' @return A list of two dataframes: $sitemap with general info and $contents with sitemap info.
#' @family data fetching functions
#' 
#' @details In a shiny session, wrap the function in \code{with_shiny} and supply the session object
#'   in a \code{shiny_access_token} parameter 
#'
#' @family sitemap admin functions
#' @export
list_sitemaps <- function(siteUrl) {
  
  f <- googleAuth_fetch_generator("https://www.googleapis.com/webmasters/v3/",
                                  "GET",
                                  path_args = list(sites = "siteURL",
                                                   sitemaps = ""),
                                  data_parse_function = function(x){
                                    list(sitemap = x$sitemap[, setdiff(names(x$sitemap), "contents")],
                                         contents = x$sitemap$contents[[1]])
                                  })
  f(path_arguments=list(sites=check.Url(siteUrl)))
  
}

#' Look up the country codes
#' 
#' @param code the ISO 3166-1 alpha-3 country code (as used in searchConsoleR)
#' @param name_type One of "Name", "Official_name" or "Common_name"
#' 
#' @return The country name string
#' @keywords internal
#' @family search analytics
lookupCountryCode <- function(country.code, 
                              name_type = "Name"){
  
  if(!name_type %in% c("Name","Official_name","Common_name")){
    stop('name_type not one of "Name","Official_name","Common_name".  Got: ', name_type)
  }
  
  country.code <- stringr::str_to_upper(country.code, "en")
  
  ## get lookup data
  country.codes <- ISO_3166_1[,name_type]
  names(country.codes) <- ISO_3166_1$Alpha_3
  
  if(all(country.code %in% names(country.codes))){
    country.codes[country.code] 
  } else {
    stop("country.code not in country.codes. Got: ", country.code)
  }
  
}


#' Helper function for the query dimension filters
#' 
#' Saves the need for 3 parameters when you just have one
#' 
#' @param dfe A string of form: dimension operator expression e.g. country!~GBR
#' 
#' @details dimension = c('country','device','page','query')
#'   operator = c(`~~` = 'contains',
#'                `==` = 'equals',
#'                `!~` = 'notContains',
#'                 `!=` = 'notEquals)
#'  
#'  expression = country: an ISO 3166-1 alpha-3 country code.
#'               device: 'DESKTOP','MOBILE','TABLET'
#'               page: not checked
#'               query: not checked
#'          
#' @keywords internal
#' @family search analytics
gsc.parseDimFilterGroup <- function(dfe){
  
  ## get lookup data
  country.codes <- ISO_3166_1$Alpha_3
  devices <- c('DESKTOP','MOBILE','TABLET')
  op_symbol <-  c("~~" = 'contains',
                  "==" = 'equals',
                  "!~" = 'notContains',
                  "!=" = 'notEquals')
  
  ## extract variables needed
  operator <- stringr::str_extract(dfe, "[\\!=~]{2}")
  dim_ex <- stringr::str_split_fixed(dfe, "[\\!=~]{2}", n=2)
  
  ## remove whitespace apart from expression  
  dim_ex[1] <- stringr::str_replace_all(dim_ex[1]," ","")
  operator <- stringr::str_replace_all(operator," ","")
  dim_ex[2] <- stringr::str_trim(dim_ex[2])
  
  ## check variables
  if(!dim_ex[1] %in% c('country','device','page','query')){
    stop("dimension not one of: ", 
         paste(c('country','device','page','query')), 
         " Got this: ", 
         dim_ex[1])
  }
  
  if(!op_symbol[operator] %in% op_symbol){
    stop("Operator not one of ~~, ==, !~ or !=.")
  }
  
  if(is.na(operator) || nchar(dim_ex[2]) == 0){
    stop("Incorrect format of filter: Need 'dimension' 'operator' 'expression'. e.g. 'country!~GBR' or 'device==MOBILE'. Got this:   ", dfe)
  }
  
  if(dim_ex[1] == "country" && 
     !stringr::str_to_upper(dim_ex[2], "en") %in% country.codes ){
    stop("country dimension indicated, but country code not an ISO 3166-1 alpha-3 type. e.g. GBR = United Kingdom. Got this:   ", dim_ex[2])
  }
  
  if(dim_ex[1] == "device" && !dim_ex[2] %in% devices ){
    stop("device dimension indicated, but device not one of DESKTOP, MOBILE or TABLET (no quotes). Got this:   ", dim_ex[2])
  }
  
  list(dimension = dim_ex[1],
       operator  = unname(op_symbol[operator]),
       expression = dim_ex[2]
  )
}

#' Checks valid parameters for Google Search Console
#' 
#' The error type listings have certain categories/platform allowed.
#' 
#' @return TRUE if valid, raises an error if not.
#' 
#' @keywords internal
gsc.is.valid.category.platform <- function (category, platform, include.all=FALSE) {
  
  categories <- getOption("searchConsoleR.valid.categories")
  platforms  <- getOption("searchConsoleR.valid.platforms")
  
  if(include.all){
    categories <- c('all', categories)
    platforms <- c('all', platforms)
  }
  
  if(!category %in% categories) {
    stop("Incorrect category.  Must be one of ", paste(categories, collapse=","))
  }
  
  if(!platform %in% platforms){
    stop("Incorrect platform. Must be one of ", paste(platforms, collapse=", "))
  }
  
  TRUE
}

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
  
#   if(all('country' %in% names(dimensionCols), prettyNames)){
#     dimensionCols$countryName <- lookupCountryCode(dimensionCols$country)
#   }
  
  metricCols <- the_data[setdiff(names(the_data), 'keys')]
  
  the_df <- data.frame(dimensionCols , metricCols, stringsAsFactors = F, row.names = NULL)
  attr(the_df, "aggregationType") <- x$responseAggregationType
  
  the_df
}