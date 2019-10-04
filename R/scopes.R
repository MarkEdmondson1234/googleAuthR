#' Create or add scopes to configuration
#' 
#' Helper for working with scopes
#' 
#' @param required_scopes character vector of scopes to add
#' 
#' @export
gar_scope_config <- function(required_scopes){
  assert_that(is.character(required_scopes))
  
  op <- getOption("googleAuthR.scopes.selected")
  if(is.null(op)){
    options(googleAuthR.scopes.selected = required_scopes)
  } else if(!any(op %in% required_scopes)){
    myMessage(paste("Adding ",paste(required_scopes, collapse = " ")," to scopes"), 
              level = 3)
    options(googleAuthR.scopes.selected = c(op, required_scopes))
  }
  
  getOption("googleAuthR.scopes.selected")
}