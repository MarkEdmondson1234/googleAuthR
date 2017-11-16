## Test environments
* local OS X install, R 3.4.1
* ubuntu 12.04 (on travis-ci), R 3.4.2
* Windows Server 2008 R2 SP1, R-release, 32/64 bit (on r-hub), R 3.4.2

## Only one day since last submission

A bug was introduced that broke some downstream libraries that didn't come up
  in the tests. Sorry about that!
  
## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTE:

* Namespaces in Imports field not imported from:
     'R6'
     All declared Imports should be used.
     
R6 is a runtime dependency
  
## Downstream dependencies
I have also run R CMD check on downstream dependencies of googleAuthR 

* bigQueryR
* googleAnalyticsR
* searchConsoleR
* googleCloudStorageR
* googleComputeEngineR
* googleLanguageR

All packages passed.  googleLanguageR has a 503 error appear in its tests, but that is due
to its test suite which will be updated in its next update.
  
