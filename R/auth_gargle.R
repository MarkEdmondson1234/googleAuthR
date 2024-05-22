# specific to gargle auth functions
# from https://github.com/tidyverse/googledrive/blob/master/R/drive_auth.R

#' Environment to store authentication credentials
#' 
#' Used to keep persistent state. Initialized in .onLoad.
#' @noRd
.auth <- NULL


## The roxygen comments for these functions are mostly generated from data
## in this list and template text maintained in gargle.
gargle_lookup_table <- list(
  PACKAGE     = "googleAuthR",
  YOUR_STUFF  = "your API",
  PRODUCT     = "googleAuthR",
  API         = "Google API",
  PREFIX      = "gar"
)

#' Suspend authorization
#'
#' @eval gargle:::PREFIX_deauth_description_with_api_key(gargle_lookup_table)
#'
#' @family auth functions
#' @export
#' @examples
#' \dontrun{
#' gar_deauth()
#' }
gar_deauth <- function() {
  .auth$set_auth_active(FALSE)
  .auth$clear_cred()
  invisible()
}

#' Produce configured token
#'
#' @eval gargle:::PREFIX_token_description(gargle_lookup_table)
#' @eval gargle:::PREFIX_token_return()
#'
#' @family low-level API functions
#' @export
#' @examples
#' \dontrun{
#' req <- request_generate(
#'   "drive.files.get",
#'   list(fileId = "abc"),
#'   token = gar_token()
#' )
#' req
#' }
gar_token <- function() {
  if (isFALSE(.auth$auth_active)) {
    return(NULL)
  }
  if (!gar_has_token()) {
    gar_auth()
  }
  httr::config(token = .auth$cred)
}

#' Is there a token on hand?
#'
#' @eval gargle:::PREFIX_has_token_description(gargle_lookup_table)
#' @eval gargle:::PREFIX_has_token_return()
#'
#' @family low-level API functions
#' @export
#'
#' @examples
#' gar_has_token()
gar_has_token <- function() {
  inherits(.auth$cred, "Token2.0")
}

#' Edit and view auth configuration
#'
#' @eval gargle:::PREFIX_auth_configure_description(gargle_lookup_table)
#' @eval gargle:::PREFIX_auth_configure_return(gargle_lookup_table)
#'
#' @param app A Google OAuth client, presumably constructed via \link[gargle]{gargle_oauth_client_from_json}. Note, however, that it is preferred to specify the client with JSON, using the `path` argument.
#' @param path JSON downloaded from \href{https://console.cloud.google.com}{Google Cloud Console}, containing a client id and secret, in one of the forms supported for the \code{txt} argument of \code{\link[jsonlite]{fromJSON}} (typically, a file path or JSON string).
#' @param api_key API key.
#'
#' @family auth functions
#' @export
#' @examples
#' # see and store the current user-configured OAuth app (probaby `NULL`)
#' (original_app <- gar_oauth_app())
#'
#' # see and store the current user-configured API key (probaby `NULL`)
#' (original_api_key <- gar_api_key())
#'
#' if (require(httr)) {
#'   # bring your own app via client id (aka key) and secret
#'   google_app <- httr::oauth_app(
#'     "my-awesome-google-api-wrapping-package",
#'     key = "123456789.apps.googleusercontent.com",
#'     secret = "abcdefghijklmnopqrstuvwxyz"
#'   )
#'   google_key <- "the-key-I-got-for-a-google-API"
#'   gar_auth_configure(app = google_app, api_key = google_key)
#'
#'   # confirm the changes
#'   gar_oauth_app()
#'   gar_api_key()
#' }
#'
#' \dontrun{
#' ## bring your own app via JSON downloaded from Google Developers Console
#' gar_auth_configure(
#'   path = "/path/to/the/JSON/you/downloaded/from/google/dev/console.json"
#' )
#' }
#'
#' # restore original auth config
#' gar_auth_configure(app = original_app, api_key = original_api_key)
#' @importFrom rlang is_string
gar_auth_configure <- function(app, path, api_key) {
  if (!missing(app) && !missing(path)) {
    stop("Must supply exactly one of `app` and `path`", call. = FALSE)
  }
  stopifnot(missing(api_key) || is.null(api_key) || is_string(api_key))
  
  if (!missing(path)) {
    stopifnot(is_string(path))
    app <- gargle::oauth_app_from_json(path)
  }
  stopifnot(missing(app) || is.null(app) || inherits(app, "oauth_app"))
  
  if (!missing(app) || !missing(path)) {
    .auth$set_app(app)
  }
  
  if (!missing(api_key)) {
    .auth$set_api_key(api_key)
  }
  
  invisible(.auth)
}

#' @export
#' @rdname gar_auth_configure
gar_api_key <- function() .auth$api_key

#' @export
#' @rdname gar_auth_configure
gar_oauth_app <- function() .auth$app