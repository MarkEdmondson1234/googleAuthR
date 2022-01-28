#' Create a httr token from a js token
#' @keywords internal
#' @noRd
#' @importFrom httr oauth_app Token2.0 oauth_endpoints
gar_js_getToken <- function(token,
                            client.id     = getOption("googleAuthR.webapp.client_id"),
                            client.secret = getOption("googleAuthR.webapp.client_secret")){
  check_package_loaded("shiny")
  gar_app <- oauth_app("google", key = client.id, secret = client.secret)
  
  scope_list <- getOption("googleAuthR.scopes.selected")
  
  # Create a Token2.0 object consistent with the token obtained from gar_auth()
  token_formatted <-
    Token2.0$new(app = gar_app,
                 endpoint = oauth_endpoints("google"),
                 credentials = list(access_token = token$access_token,
                                    token_type = token$token_type,
                                    expires_in = token$expires_in,
                                    refresh_token = NULL),
                 params = list(scope = scope_list, type = NULL,
                               use_oob = FALSE, as_header = TRUE),
                 cache_path = FALSE)
  
  token_formatted
}