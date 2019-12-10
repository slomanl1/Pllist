### FRONT MATTER ####
shell('del c:\\users\\larry\\downloads\\*.csv')
library(devtools)
library(RSelenium)
library(XML)
library(plyr)
library(httr)
if(exists('rD'))
  rD[["server"]]$stop()
######################

## This block will open the Chrome browser, which is linked to R
rD <- rsDriver(browser = 'chrome')
remDr <- rD[["client"]]
remDr$navigate('https://eyeonwater.com/account/signin?')

#// Start Login

user <- "pjrice58@yahoo.com" 
pass <- "Fastone19@"
remDr$findElement("id", "username_entry")$sendKeysToElement(list(user))
remDr$findElement("id", "password")$sendKeysToElement(list(pass))
remDr$findElement("id", "eow-sign-in")$clickElement()
xx=remDr$findElement('css selector','*')
Sys.sleep(2)
xz=xx$getElementText()[[1]]
writeLines(xz,'~/xx.txt')
remDr$findElement("id", "export-data-button")$clickElement()
dstart=strftime(Sys.Date()-1,"%m/%d/%Y")
dend=strftime(Sys.Date(),"%m/%d/%Y")
Sys.sleep(2)
remDr$findElement("id", "data-export-start-datepicker")$sendKeysToElement(list(dstart))
remDr$findElement("id", "data-export-end-datepicker")$sendKeysToElement(list(dend))
remDr$findElement("class", "select2-container")$clickElement()
remDr$findElement(using = 'xpath', "//*/option[@value ='Hourly']")$clickElement()
remDr$findElement("id", "btn-export-data")$clickElement()
Sys.sleep(3)
remDr$findElement('id','export_result_url')$clickElement()
while(!file.exists('C:/Users/Larry/Downloads/export.csv')){}
rD[["server"]]$stop()
dd=readLines('~/xx.txt')

