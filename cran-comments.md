## Test environments
* local OS X install, R 3.3.2
* ubuntu 12.04 (on travis-ci), R 3.3.2
* Windows Server 2008 R2 SP1, R-release, 32/64 bit (on r-hub), R 3.3.2 

## CRAN submission 1 feedback

Found the following (possibly) invalid URLs:
 URL: http://cran.r-project.org/package=googleAuthR
   From: README.md
   CRAN URL not in canonical form
 URL: https://cran.r-project.org/web/packages/googlesheets/vignettes/managing-auth-tokens.html#tokens-for-testing
   From: man/gar_attach_auto_auth.Rd
   Status: 200
   Message: OK
   CRAN URL not in canonical form
 The canonical URL of the CRAN page for a package is 
   https://CRAN.R-project.org/package=pkgname
 Canonical CRAN.R-project.org URLs use https.
 
Now fixed

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
  
