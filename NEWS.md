# News Updates

# v0.3.1 - CRAN

* Add link to [example shiny app](https://mark.shinyapps.io/googleAuthRexample/)
* Add `option(googleAuthR.rawResponse)` - skip API checks on response - should now work
* A successfull request is now classed as all response codes matching ^20 e.g. 201, 204 etc.

# v0.3.0 

* Document default options in `?googleAuthR`
* Add `option(googleAuthR.rawResponse)` - skip API checks on response.
* Add an example Shiny app in `/inst/shiny/shiny-example.R`
* Add an [RStudio Addin](https://rstudio.github.io/rstudioaddins/) for easy authentication.  Run via menu or `googleAuthR:::gar_gadget()`
* Move simplifyVector option to be able to be passed in generated function, defaults to `getOption("googleAuthR.jsonlite.simplifyVector")`
* Remove scopes option as not used. 
* Added `googleAuthR.verbose` to control feedback. 0 = everything, 1 = debug, 2=normal, 3=important
* Make the retry kick in more often for every 5** and 429 status error
* Support non-JSON uploads (#28)
* Add option to force user consent screen on Shiny login
* Move specification of scope for `gar_auth_service` to param for more flexibility
* Migrated shiny functions to Shiny Modules (#27)

# v0.2 

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

# v0.1 - CRAN

* Shiny compatibility
* Local authentication compatibility
