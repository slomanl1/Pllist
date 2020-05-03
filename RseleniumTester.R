### FRONT MATTER ####

library(devtools)
library(RSelenium)
library(XML)
library(plyr)
library(httr)

######################

## This block will open the Chrome browser, which is linked to R
rD <-rsDriver(chromever = "81.0.4044.69")
remDr <- rD[["client"]]
remDr$navigate('https://my.uclahealth.org/MyChart/')

#// Start Login

user <- "slomanl1"; pass <- "fastsix2013"
remDr$findElement("id", "Login")$sendKeysToElement(list(user))
remDr$findElement("id", "Password")$sendKeysToElement(list(pass))
remDr$findElement("id", "submit")$clickElement()

nn=1
for(jj in 1:10){
  print(paste('jj=',jj))
  remDr$navigate(sprintf('https://my.uclahealth.org/MyChart/inside.asp?mode=labs&RESULTS_results_lmPgNum=%s',jj))
  
  while(TRUE){
    print(paste('nn=',nn))
    remDr$navigate(sprintf('https://my.uclahealth.org/MyChart/inside.asp?mode=labdetail&orderid=%s&printmode=true',nn))
    #remDr$screenshot(file=sprintf('~/pngs/test%s%s.png',jj,nn))
    xx=remDr$findElement('css selector','*')
    xz=xx$getElementText()[[1]]
    writeLines(xz,sprintf('~/pngs/ScreenText%s%s.txt',jj,nn))
    nn=nn+1
    if(len(remDr$findElements('class','rz_11'))==0)
      break
  }
}

xx=remDr$findElement('css selector','*')
xz=xx$getElementText()[[1]]
writeLines(xz,'~/xx.txt')
rD[["server"]]$stop()



