# library(googleAuthR)

#' Get a GTM account list
gtm_accounts_list <- function(){
  f <- googleAuth_fetch_generator("https://www.googleapis.com/tagmanager/v1/accounts",
                                  "GET",
                                  data_parse_function = function(x){
                                    x$accounts
                                  })
}