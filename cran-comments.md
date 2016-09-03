## Test environments
* local OS X install, R 3.3.0
* ubuntu 12.04 (on travis-ci), R 3.3.0
* win-builder, R 3.3.0

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Mark Edmondson <m@sunholo.com>'

  License components with restrictions and base license permitting such:
    MIT + file LICENSE
  File 'LICENSE':
    YEAR: 2016
    COPYRIGHT HOLDER: Sunholo Ltd.

  Possibly mis-spelled words in DESCRIPTION:
    API (4:47)
    APIs (5:66)
    OAuth (4:40, 5:52)
  
  These are spelt as intended.
  
## Downstream dependencies
I have also run R CMD check on downstream dependencies of googleAuthR 

* bigQueryR
* googleAnalyticsR
* searchConsoleR
* googleCloudStorageR

All packages passed.
  
