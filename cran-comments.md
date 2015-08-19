## Test environments
* local OS X install, R 3.2.2
* ubuntu 12.04 (on travis-ci), R 3.2.2
* win-builder, R 3.2.2

## R CMD check results
There were no ERRORs or WARNINGs. 

There were 3 NOTEs:

* Maintainer: ‘Mark Edmondson <m@sunholo.com>’
  New submission

  My first submission

* checking dependencies in R code ... NOTE
  Namespace in Imports field not imported from: 'R6'

  R6 is a build-time dependency.
  
* Found the following (possibly) invalid URLs:
  URL: http://127.0.0.1:4624
    From: inst/doc/googleAuthR.html
    Status: Error
    Message: libcurl error code 7
    	Failed to connect to 127.0.0.1 port 4624: Connection refused
    	
  
  This is a local testing URL for Shiny. 
  Users select their own port, in this example 4624 was used.