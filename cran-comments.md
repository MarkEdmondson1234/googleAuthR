## Test environments
* local OS X install, R 3.5.0
* Ubuntu 14.04.5 LTS (on travis-ci), R 3.6.0
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit (on r-hub)
  
## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTE:

* Namespaces in Imports field not imported from:
     'R6'
     All declared Imports should be used.
     
R6 is a runtime dependency
  
## Downstream dependencies
I have also run R CMD check on downstream dependencies of googleAuthR

The results were:

* Checked bigQueryR           : 0 errors | 0 warnings | 0 notes
* Checked googleAnalyticsR    : 0 errors | 0 warnings | 0 notes
* Checked googleCloudStorageR : 0 errors | 0 warnings | 0 notes
* Checked googleComputeEngineR: 0 errors | 0 warnings | 0 notes
* Checked googleLanguageR     : 0 errors | 0 warnings | 0 notes
* Checked googlePrintr        : 0 errors | 0 warnings | 0 notes
* Checked searchConsoleR      : 0 errors | 0 warnings | 0 notes

All packages passed.
  
