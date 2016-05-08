## Test environments
* local OS X install, R 3.3.0
* ubuntu 12.04 (on travis-ci), R 3.3.0
* win-builder, R 3.3.0

## R CMD check results
There were no ERRORs or WARNINGs. 

There were 2 NOTEs:

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

* checking dependencies in R code ... NOTE
  Namespace in Imports field not imported from: 'R6'

  R6 is a build-time dependency.
  
