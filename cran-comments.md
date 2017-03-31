## Test environments
* local OS X install, R 3.3.2
* ubuntu 12.04 (on travis-ci), R 3.3.2
* Windows Server 2008 R2 SP1, R-release, 32/64 bit (on r-hub), R 3.3.2 

## Only a month since last submission

Apologies, but I found a bug that would prevent downstream libraries loading in some edge cases.

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

All packages passed.
  
