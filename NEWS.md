# News Updates

# v1.2.9000

* Remove scopes option as not used. 
* Added `googleAuthR.verbose` to control feedback. 0 = everything, 1 = debug, 2=normal, 3=important
* Allow customConfig to change all httr paramters, including encode, config etc.

# v1.2 - CRAN

* Added ability to add your own custom headers to requests via `customConfig` in `gar_api_generator`
* Add 'localhost' to shiny URL detection. 
* Google Service accounts now supported.  Authenticate via "Service Account Key" JSON.
* Exposed `gar_shiny_getUrl` and the authentication type (online/offline) in `renderLogin`
* `renderLogin` : logout now has option `revoke` to revoke authentication token
* Added option for `googleAuthR.jsonlite.simplifyVector` for content parsing for compatibility for some APIs
* Batch Google API requests now implemented.  See readme or `?gar_batch` and `?gar_batch_walk` for details.
* If data parsing fails, return the raw content so you can test and modify your data parsing function 
* Missed Jenny credit now corrected
* Add tip about using `!is.null(access_token())` to detect login state
* Add HTTP backoff for certain errors (#6) from Johann
* Remove possible NULL entries from path and pars argument lists
* Reduced some unnecessary message feedback
* moved `with_shiny` environment lookup to within generated function
* added gzip to headers

# v1.1 - CRAN

* Shiny compatibility
* Local authentication compatibility