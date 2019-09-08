# googleAuthR v1.1.1

* Allow to be used by R 3.3 via custom `isFALSE` function (#158 - thanks @matthijsvanderloos)
* Web JSON client id messaging
* Auto_auth works on startup now

# googleAuthR v1.1.0

* Add default scope of "https://www.googleapis.com/auth/cloud-platform" to `gar_gce_auth()`
* Improvements to stop `gar_auto_auth()` block library installation if auth files not correct.

# googleAuthR v1.0.0

* Implement `library(gargle)` as backend for authentication functions
* Auto-attempt authentication via `gar_auth()` if an API call has no auth set
* Remove explicit R6 dependency as imported via `gargle`
* Enable some customization of `googleSignInUI`'s logout button via `logout_name` and `logout_class` parameters
* Expose information whether user is signed in via `googleSignIn` module
* Auto-auth by specifying an email address in `gar_auto_auth()`

# googleAuthR v0.8.1

* Depend on usethis for `create_package()` and `use_github()` as they are
  removed in devtools 2.1.0 (@jimhester, #150).

# googleAuthR v0.8.0

* Change behaviour on API parse errors to write a diagnostic object to the working directory
* Add `gar_debug_parsing` to help debug API parsing errors.
* Fix batching that errored if body was exactly the same 
* Set environment args in `gar_set_client()` to help deployments on Shinyapps.io
* Support API body page requests in `gar_api_page()`
* Fix `gar_set_scopes()` to allow multiple scopes when setting with `web_json`
* Change required scopes in `gar_auto_auth` to only require one of the scopes specified, instead of all of them
* Fully remove `TRAVIS_AUTH` 
* Remove some message spam from startup
* Fix mocks failing with `httptest` if used with `with_mock_api()` vs `with_mock_API()`
* Add `option(googleAuthR.redirect)` to help publish Shiny apps to some platforms (#136)
* Add support for using gcloud application-default credentials on Jupyter notebooks via `gar_gce_auth_default()` (#147)

# googleAuthR v0.7.0

* Deprecate `https://www.googleapis.com/batch` batch endpoint fully as per https://developers.googleblog.com/2018/03/discontinuing-support-for-json-rpc-and.html
* Add `gar_api_page()` to help page through APIs (#121)
* Add a `url_override` argument to generated Google API functions to help with (#121)
* Add support for the Google Sign In JavaScript API via a Shiny module (#119 - thanks @dkulp2) - see `googleSignIn`
* Add support for new Shiny authentication flow where you load auth before main ui.R - see `gar_shiny_auth()`

# googleAuthR v0.6.3

* Another go at JavaScript approval prompt options
* Support web apps (Shiny) in `gar_set_client` (#107)
* Shiny auth will not attempt to create a `.httr-oauth` file (#84)
* Update docs for `gar_batch_walk()` with some examples (#175)
* Add bigQueryR and googleAnalyticsR to suggests for CRAN test checks. 

# googleAuthR v0.6.2

* Encoding fixed as it broke some functions downstream (#101)
* Fix batching with caching options (#106)
* Add `gar_set_client` to load client id/secret from JSON (Idea via @jennybc / @jimhester at `gargle`/`gmailr`)

# googleAuthR v0.6.1

* Fix bug with batching that didn't parse data (#103)
* If you specify a filename in `gar_auth(token = "blah")` that doesn't exist, will create new token there instead of an error
* If you specify a valid auth token filename, the scopes and client Id/secret options will now update to the settings within it
* Add encoding to URL parameters you pass in (#101)

# googleAuthR v0.6.0

## Major changes

* A Slack team set up for googleAuthR package support, sign up via https://goo.gl/forms/d541yrJoDFMrrSJp1
* Default demo project scopes now NULL, set your own Google Project via `options()` or by setting up environment arguments (#74):
   - `GAR_CLIENTID`
   - `GAR_CLIENT_SECRET`
   - `GAR_WEB_CLIENTID`
   - `GAR_WEB_CLIENT_SECRET`
   - `GAR_SCOPE`
* Add ability to return the email of the service account on a GCE instance, `gar_gce_auth_email`
* Add ability to cache API calls into memory or writing to local disk (#69 and #68)
* Make optional a trailing slash on URIs (#73)
* Improve header checks to be more RFC compliant (#78)
* Use `httr`s `RETRY` function for retries that handles handles better (#67)
* RStudio Addin now offers menu to prepopulate scopes for APIs from discovery API. 
* Add `googleAuthR.batch_endpoint` option for specific library batches.
* Remove TRAVIS support in environment arguments
* Add `gar_check_existing_token()` to improve user feedback on why a reauthentication occurs

## Bug fixes

* Fix `http2 framing layer error` that sometimes happens in misconfigured http2 servers (#87)
* Update minimum dependency to R `3.3.0` to support Shiny function bugs. 
 
# googleAuthR v0.5.1

## Major changes

* Fix bug in `gar_auto_auth` where it expects a file.path not a token object

# googleAuthR v0.5.0

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

# googleAuthR v0.4.0

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

# googleAuthR v0.3.1

## Major changes

* Add link to [example shiny app](https://mark.shinyapps.io/googleAuthRexample/)
* Add `option(googleAuthR.rawResponse)` - skip API checks on response - should now work
* A successfull request is now classed as all response codes matching ^20 e.g. 201, 204 etc.

# googleAuthR v0.3.0 

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

# googleAuthR v0.2 

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

# googleAuthR v0.1

## Major changes

* Shiny compatibility
* Local authentication compatibility
