library(RSelenium)
cat("\n==Local so starting own server")
CHROME_DRIVER <- "/Users/mark/dev/R/selenium/chromedriver"
u <- "test@sunholo.com"
pw <- "test!!!!!"

selServ <- startServer(args = c("-port 1221",
                                paste0("-Dwebdriver.chrome.driver=",CHROME_DRIVER)),
                       log = FALSE)

remDr <- remoteDriver(browserName = "chrome", port = 1221)
on.exit(remDr$close())
remDr$open(silent = TRUE)
remDr$navigate("http://127.0.0.1:1221/")

do_auth_flow <- function(){
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
  remDr$quit()
}

do_auth_flow()

