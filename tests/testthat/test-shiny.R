library(testthat)
library(googleAuthR)
library(shiny)

context("Shiny")

# runShinyAppTest <- function(){
#   if(file.exists("auth_success.rds")){
#     unlink("auth_success.rds")
#   }
#   runApp(shinyApp(
#     ui = fluidPage(title = "googleAuthR Test Login",
#                    googleAuthUI("loginButton")
#     ),
#     server = function(input, output, session){
#       
#       ## Create access token and render login button
#       access_token <- callModule(googleAuth, "loginButton", approval_prompt = "force")
#       
#       observe({
#         req(access_token())
#         
#         cat("\n## Shiny auth sucessful")
#         saveRDS(TRUE, "auth_success.rds")
#         stopApp(TRUE)
#       })
#       
#     }
#   ), port = 1221)
#   
# }

## run this in a seperate R process
## runShinyAppTest()

library(RSelenium)


if(Sys.getenv("TRAVIS_GAR_AUTH_FILE") == ""){
  cat("\n==Local so starting own server")
  CHROME_DRIVER <- "/Users/mark/dev/R/selenium/chromedriver"

  
  selServ <- startServer(args = c("-port 1221",
                                  paste0("-Dwebdriver.chrome.driver=",CHROME_DRIVER)),
                         log = FALSE)
  
  remDr <- remoteDriver(browserName = "chrome", port = 1221)
  on.exit(remDr$close())
} else {
  remDr <- remoteDriver(port = 1221)
}

remDr$open(silent = TRUE)
remDr$navigate("http://127.0.0.1:1221/")

do_auth_flow <- function(u = "test@sunholo.com",
                         pw = "test!!!!!"){
  login_xpath <- '//*[@id="loginButton-googleAuthUi"]/a[2]'
  button <- remDr$findElement(value = login_xpath)
  remDr$mouseMoveToLocation(webElement = button)
  remDr$click()
  username_xpath <- '//*[@id="Email"]'
  remDr$findElement(value = username_xpath)
  remDr$sendKeysToActiveElement(list(u, key = 'enter'))
  pass_xpath <- '//*[@id="Passwd"]'
  remDr$findElement(value = pass_xpath)
  remDr$sendKeysToActiveElement(list(pw, key = 'enter'))
  Sys.sleep(10)
  allow_xpath <- '//*[@id="submit_approve_access"]'
  remDr$findElement(value = allow_xpath)
  remDr$sendKeysToActiveElement(list(key = "enter"))
  Sys.sleep(5)
  remDr$quit()
}



test_that("Can authenticate in Shiny", {  
  skip_on_cran()
  do_auth_flow()
  
  expect_true(file.exists("auth_success.rds"), info = "The Shiny Auth failed")
})

