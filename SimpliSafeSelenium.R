### FRONT MATTER ####

library(devtools)
library(RSelenium)
library(XML)
library(plyr)
library(httr)

ycred='https://login.yahoo.com/?.src=ym&.lang=en-US&.intl=us&.done=https%3A%2F%2Fmail.yahoo.com%2Fd%3F.src%3Dfp'
user <- "LarrySloman@yahoo.com"
pass <- "Fastsix123"
######################

## This block will open the Chrome browser, which is linked to R
rD <-rsDriver(chromever = "81.0.4044.69")
remDr <- rD[["client"]]

#// Start Login
while(TRUE){
  remDr$navigate('https://webapp.simplisafe.com/#/login/')
  Sys.sleep(1)
  err=list(value=0,warning=0)
  while(!is.null(err$value)){
    evinfo=remDr$findElements(using = 'name', "email")
    err=tryCatch.W.E(
      evinfo[[1]]$sendKeysToElement(list(user))
    )
  }
  evinfo=remDr$findElements(using = 'name', "password")
  evinfo[[1]]$sendKeysToElement(list(pass))
  evinfo=remDr$findElement(using = "class", "ss-standard-button") 
  evinfo$clickElement()
  Sys.sleep(2)
  evinfo=remDr$findElement(using = "class", "ss-inline-text-action")
  rsp=evinfo$getElementText()
  if(grepl("Resend E-mail",rsp)){
    print('Login Waiting for Verify')
    remDr$navigate(ycred)
    Sys.sleep(3)
    zz=remDr$findElements(using = 'id', "login-username")
    usern=paste(user,'\n',sep='')
    zz[[1]]$sendKeysToElement(list(usern))
    ################# sendKeysToElement(list(key = "enter")) ##################
    Sys.sleep(3)
    zz=remDr$findElements(using = 'id', "login-passwd")
    Sys.sleep(4)
    passn=paste(pass,'\n',sep='')
    zz[[1]]$sendKeysToElement(list(passn))
    zz=remDr$findElements(using = "class", "en_N")
    txts=sapply(1:len(zz), function(x) zz[[x]]$getElementText())
    idxs=which(grepl("SimpliSafe Account New Device Login",txts))
    zz[[idxs[1]]]$clickElement() # click on newest very email message
    zz=remDr$findElement(using = "partial link text",value="Verify Device")
    Sys.sleep(4)
    zz$clickElement() # click Verify Device
  }else{
    break
  }
}
print('SimpliSafe Login Successful')
remDr$navigate('https://webapp.simplisafe.com/new/#/timeline')
Sys.sleep(3)
evtxts=remDr$findElements(using = 'class', "event-text")
evinfo=remDr$findElements(using = 'class', "event-info")
for(jj in 1:50)
  print(evinfo[[jj]]$getElementText())

remDr$close()
rD[["server"]]$stop()
system("taskkill /im java.exe /f > nul 2>&1", intern = FALSE, ignore.stdout=TRUE, ignore.stderr=TRUE)
rm(rD)
gc()




