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
#' @details
#' 
#' In a shiny session, wrap the function in \code{with_shiny} and supply the session object
#'   in a \code{shiny_access_token} parameter 
#' @export
#' @family sitemap admin functions
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


#' Submit a sitemap.
#' 
#' See here for details: https://developers.google.com/webmaster-tools/v3/sitemaps/submit
#' 
#' @param siteURL The URL of the website to delete. Must include protocol (http://).
#' @param feedpath The URL of the sitemap to submit. Must include protocol (http://).
#' @param shiny_access_token If in a Shiny session, supply its shiny_access_token object.
#' 
#' @return TRUE if successful, raises an error if not.
#'
#' @export
#' @family sitemap admin functions
add_sitemap <- function(siteURL, feedpath, shiny_access_token=NULL) {
  
  siteURL  <- check.Url(siteURL, reserved=T)
  feedpath <- check.Url(feedpath, reserved = T)
  
  ## require pre-existing token, to avoid recursion
  if(checkTokenAPI(shiny_access_token)) {
    
    ## docs here
    ## https://developers.google.com/webmaster-tools/v3/sitemaps/submit
    req_url <- paste0("https://www.googleapis.com/webmasters/v3/sites/", 
                      siteURL,"/sitemaps/",
                      feedpath)
    
    searchconsole_PUT(req_url, shiny_access_token, the_body = NULL)
    
    TRUE
    
  } else {
    
    stop("Invalid Token")
    
  }
  
}

#' Delete a sitemap.
#' 
#' See here for details: https://developers.google.com/webmaster-tools/v3/sitemaps/delete
#' 
#' @param siteURL The URL of the website you are deleting the sitemap from. Must include protocol (http://).
#' @param feedpath The URL of the sitemap to delete. Must include protocol (http://).
#' @param session If in a Shiny session, supply its shiny_access_token object.
#' 
#' @return TRUE if successful, raises an error if not.
#'
#' @export
#' @family sitemap admin functions
delete_sitemap <- function(siteURL, feedpath, shiny_access_token=NULL) {
  
  siteURL  <- check.Url(siteURL, reserved=T)
  feedpath <- check.Url(feedpath, reserved = T)
  
  ## require pre-existing token, to avoid recursion
  if(checkTokenAPI(shiny_access_token)) {
    
    ## docs here
    ## https://developers.google.com/webmaster-tools/v3/sitemaps/delete
    req_url <- paste0("https://www.googleapis.com/webmasters/v3/sites/", 
                      siteURL,"/sitemaps/",
                      feedpath)
    
    searchconsole_DELETE(req_url, shiny_access_token)
    
    TRUE
    
  } else {
    
    stop("Invalid Token")
    
  }
  
}

#' Fetch a time-series of Googlebot crawl errors.
#' 
#' @description 
#' Get a list of errors detected by Googlebot over time.
#' See here for details: https://developers.google.com/webmaster-tools/v3/urlcrawlerrorscounts/query
#' 
#' @param siteURL The URL of the website to delete. Must include protocol (http://).
#' @param category Crawl error category. Defaults to 'all'
#' @param platform The user agent type. 'all', 'mobile', 'smartphoneOnly' or 'web'.
#' @param latestCountsOnly Default FALSE. Only the latest crawl error counts returned if TRUE.
#' @param shiny_access_token If in a Shiny session, supply its shiny_access_token object.
#' 
#' @return dataframe of errors with $platform $category $count and $timecount.
#'
#' @details The timestamp is converted to a date as they are only available daily.
#' 
#' Category is one of: authPermissions, manyToOneRedirect, notFollowed, notFound,
#'   other, roboted, serverError, soft404.
#'   
#'   Platform is one of: mobile, smartphoneOnly or web.
#' 
#' @export
#' @family working with search console errors
crawl_errors <- function(siteURL, 
                         category="all",
                         platform=c("all","mobile","smartphoneOnly","web"),
                         latestCountsOnly=FALSE,
                         shiny_access_token=NULL) {
  platform <- match.arg(platform)
  siteURL <- check.Url(siteURL, reserved=T)
  
  latestCountsOnly <- ifelse(latestCountsOnly, 'true', 'false')
  
  ## require pre-existing token, to avoid recursion
  if(checkTokenAPI(shiny_access_token) && is.valid.category.platform(category, platform, include.all = TRUE)) {
    
    ## docs here
    ## https://developers.google.com/webmaster-tools/v3/urlcrawlerrorscounts/query
    req_url <- paste0("https://www.googleapis.com/webmasters/v3/sites/", 
                      siteURL,
                      "/urlCrawlErrorsCounts/query")
    
    param_vector <- c('category'=category,
                      'latestCountsOnly'=latestCountsOnly,
                      'platform'=platform)
    param_vector <- param_vector[param_vector != 'all']
    
    req <- searchconsole_GET(req_url, shiny_access_token, params = param_vector)
    
    cpt <- req$content$countPerTypes
    ## data parsing gymnastics
    ldf <- Reduce(rbind, 
                  apply(cpt, 1, function(row) {
                    data.frame(platform = row['platform'], 
                               category = row['category'], 
                               count = row$entries$count, 
                               timecount = row$entries$timestamp )
                  })
    )
    
    ## transform to something useable
    ldf$platform <- as.character(ldf$platform)
    ldf$category <- as.character(ldf$category)
    ldf$count <- as.numeric(as.character(ldf$count))
    ldf$timecount <- RFC_convert(ldf$timecount, drop_time = TRUE)
    
    ldf
    
  } else {
    
    stop("Invalid Token")
    
  }
  
}



#' Lists a site's sample URLs for crawl errors.
#' 
#' @description Category is one of: authPermissions, manyToOneRedirect, notFollowed, notFound,
#'   other, roboted, serverError, soft404.
#'   
#'   Platform is one of: mobile, smartphoneOnly or web.
#' 
#' @param siteURL The URL of the website to delete. Must include protocol (http://).
#' @param category Crawl error category. Default 'notFound'.
#' @param platform User agent type. Default 'web'. 
#' @param shiny_access_token If in a Shiny session, supply its shiny_access_token object.
#'
#' @details
#' See here for details: \url{https://developers.google.com/webmaster-tools/v3/urlcrawlerrorssamples}
#' 
#' @return A dataframe of $pageUrl, $last_crawled, $first_detected, $response
#'
#' @export
#' @family working with search console errors
list_crawl_error_samples <- function(siteURL,
                                     category="notFound",
                                     platform="web",
                                     shiny_access_token=NULL) {
  
  siteURL <- check.Url(siteURL, reserved=T)
  
  ## require pre-existing token, to avoid recursion
  if(checkTokenAPI(shiny_access_token) && 
     is.valid.category.platform(category, platform)) {
    
    ## docs here
    ## https://developers.google.com/webmaster-tools/v3/urlcrawlerrorssamples
    req_url <- paste0("https://www.googleapis.com/webmasters/v3/sites/", siteURL,"/urlCrawlErrorsSamples")
    
    param_vector <- c('category'=category,
                      'platform'=platform)
    
    req <- searchconsole_GET(req_url, shiny_access_token, params = param_vector)
    
    errs <- req$content$urlCrawlErrorSample
    
    if(!is.null(errs)){
      errs$last_crawled <- RFC_convert(errs$last_crawled)
      errs$first_detected <- RFC_convert(errs$first_detected)
      
      errs      
    } else {
      message("No error samples available.")
      NULL
    }
    
    
  } else {
    
    stop("Invalid Token")
    
  }
  
}

#' Shows details of errors for individual sample URLs
#' 
#' See here for details: https://developers.google.com/webmaster-tools/v3/urlcrawlerrorssamples/get 
#' 
#' @param siteURL The URL of the website to delete. Must include protocol (http://).
#' @param pageURL A PageUrl taken from list_crawl_error_samples.
#' @param category Crawl error category. Default 'notFound'.
#' @param platform User agent type. Default 'web'. 
#' @param shiny_access_token If in a Shiny session, supply its shiny_access_token object.
#' 
#' @return Dataframe of $linkedFrom, with the calling URLs $last_crawled, $first_detected and a $exampleURL
#' @family working with search console errors
#' @description 
#' pageURL is the relative path (without the site) of the sample URL. 
#' It must be one of the URLs returned by list_crawl_error_samples. 
#' For example, for the URL https://www.example.com/pagename on the site https://www.example.com/, 
#' the url value is pagename (string)
#' 
#' Category is one of: authPermissions, manyToOneRedirect, notFollowed, notFound,
#'   other, roboted, serverError, soft404.
#'   
#'   Platform is one of: mobile, smartphoneOnly or web.
#'
#' @export
error_sample_url <- function(siteURL,
                             pageURL,
                             category="notFound",
                             platform="web",
                             shiny_access_token=NULL) {
  
  siteURL <- check.Url(siteURL, reserved=T)
  pageURL <- check.Url(pageURL, checkProtocol = F, reserved = T, repeated=T)
  
  
  ## require pre-existing token, to avoid recursion
  if(checkTokenAPI(shiny_access_token) && 
     is.valid.category.platform(category, platform)){
    
    ## docs here
    ## https://developers.google.com/webmaster-tools/v3/urlcrawlerrorssamples
    req_url <- paste0("https://www.googleapis.com/webmasters/v3/sites/", 
                      siteURL,
                      "/urlCrawlErrorsSamples/",
                      pageURL)
    
    param_vector <- c('category'=category,
                      'platform'=platform)
    
    req <- searchconsole_GET(req_url, shiny_access_token, params = param_vector)
    
    raw_details <- req$content
    
    if(all(c('last_crawled', 'first_detected') %in% names(raw_details))){
      raw_details$last_crawled <- RFC_convert(raw_details$last_crawled)
      raw_details$first_detected <- RFC_convert(raw_details$first_detected)
      inner_details <- Reduce(rbind, raw_details$urlDetails)
      
      detail_df <- data.frame(linkedFrom=inner_details, 
                              last_crawled=raw_details$last_crawled,
                              first_detected=raw_details$first_detected,
                              pageUrl=raw_details$pageUrl) 
      
    }
    
  } else {
    
    stop("Invalid Token")
    
  }
  
}

#' Mark As Fixed the individual sample URLs
#' 
#' See here for details: 
#' https://developers.google.com/webmaster-tools/v3/urlcrawlerrorssamples/markAsFixed
#' 
#' @param siteURL The URL of the website to delete. Must include protocol (http://).
#' @param pageURL A PageUrl taken from list_crawl_error_samples.
#' @param category Crawl error category. Default 'notFound'.
#' @param platform User agent type. Default 'web'. 
#' @param shiny_access_token If in a Shiny session, supply its shiny_access_token object.
#' 
#' @return TRUE if successful, raises an error if not.
#' @family working with search console errors
#' 
#' @description 
#' pageURL is the relative path (without the site) of the sample URL. 
#' It must be one of the URLs returned by list_crawl_error_samples. 
#' For example, for the URL https://www.example.com/pagename on the site https://www.example.com/, 
#' the url value is pagename (string)
#' 
#' Category is one of: authPermissions, manyToOneRedirect, notFollowed, notFound,
#'   other, roboted, serverError, soft404.
#'   
#'   Platform is one of: mobile, smartphoneOnly or web.
#'
#' @export
fix_sample_url <- function(siteURL,
                           pageURL,
                           category="notFound",
                           platform="web",
                           shiny_access_token=NULL) {
  
  siteURL <- check.Url(siteURL, reserved=T)
  pageURL <- check.Url(pageURL, checkProtocol = F, reserved = T)
  
  ## require pre-existing token, to avoid recursion
  if(checkTokenAPI(shiny_access_token) && 
     is.valid.category.platform(category, platform)) {
    
    ## docs here
    ## https://developers.google.com/webmaster-tools/v3/urlcrawlerrorssamples/markAsFixed
    req_url <- paste0("https://www.googleapis.com/webmasters/v3/sites/", 
                      siteURL,
                      "/urlCrawlErrorsSamples/",
                      pageURL)
    
    param_vector <- c('category'=category,
                      'platform'=platform)
    
    req <- searchconsole_DELETE(req_url, shiny_access_token, params = param_vector)
    
    req
    
  } else {
    
    stop("Invalid Token")
    
  }
  
}