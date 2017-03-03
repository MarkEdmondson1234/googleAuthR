# v0.5.0.9000


# v0.5.0

## Major changes

* Correct bug of incorrect redirect URL when no port (#45)
* Catch error for retry if no status response at all
* Fix bug where unnamed customConfigs were ignored
* Fixes to discovery API package creations, order of parameters and NULL parameters
* Add authentication option when using Google Compute Engine `gar_gce_auth()` (#52)
* Add a warning if the cached `.httr-oauth` token has different scopes to the ones specified at time of authentication (#53)
* Add debug body aid: if `option(googleAuthR.verbose = 0)` then a request with a body will write to a file `request_debug.rds` in working directory
* Passing in a file location token to `gar_auth("file-location.rds")` will only load the first element `[[1]]` if that token is a list of `Token2.0` class objects
* Add debug tool `gar_token_info()` which will report on current authentication. Available at `options(googleAuthR.verbose = 2)`

# v0.4.0

## Major changes

* Add client based authentication in JavaScript plus example app
* Add check to `gar_auth_service` to see if you have downloaded right JSON file
* Discovery API functions to get details on Google APIs added: `gar_discovery_apis_list` and `gar_discovery_api`
* Add `gar_create_package` that takes `gar_discovery_api` JSON and creates R package
* Change warnings() in batch to myMessage() level 2
* ensure batch requests only occur per second to help calculation of QPS limits
* Add 404 message if batch requests are not found.
* Fixed halt error if message can't parse body JSON, will now fail gracefully but carry on
* allow overwriting of default httr "encode" again (#28)
* Headers will contain up to date version number of package
* Add `gar_auto_auth` and `gar_attach_auto_auth` for auto-authentication upon a package load
* Fix bug where you couldn't pass in the file location of the ".httr-oauth" location to `gar_auth()`
* `gar_auth` now raises errors not NULL for passing incorrect token file locations of tokens
* `gar_auth` respects renamed `.httr-oauth` tokens now via `getOption("googleAuthR.httr_oauth_cache")`
* Add link to Github repo with auto-generated packages: `https://github.com/MarkEdmondson1234/autoGoogleAPI`

# v0.3.1

## Major changes

* Add link to [example shiny app](https://mark.shinyapps.io/googleAuthRexample/)
* Add `option(googleAuthR.rawResponse)` - skip API checks on response - should now work
* A successfull request is now classed as all response codes matching ^20 e.g. 201, 204 etc.

# v0.3.0 

## Major changes

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

## Major changes

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

## Major changes

* Shiny compatibility
* Local authentication compatibility
