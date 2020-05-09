### FRONT MATTER ####

library(devtools)
library(RSelenium)
library(XML)
library(plyr)
library(httr)
library(pushoverr)
set_pushover_user("uv7t4utsqjm6g6eorqytdaujw74ovk")
set_pushover_app(token = "ae39raeannooa3jwv4zqiede4oo2bo")

#' @name selKeys
#' @title Selenium key mappings
#' @description This data set contains a list of selenium key mappings.
#' selKeys is used when a sendKeys variable is needed.
#' sendKeys is defined as a list.
#' If an entry is needed from selKeys it is denoted by key.
#' @docType data
#' @usage selKeys
#' @export selKeys
#' @format A named list. The names are the descriptions of the keys. The
#'    values are the "UTF-8" character representations.
#' @source https://github.com/SeleniumHQ/selenium/wiki/JsonWireProtocol#sessionsessionidelementidvalue
selKeys <- list(
  null = "\uE000",
  cancel = "\uE001",
  help = "\uE002",
  backspace = "\uE003",
  tab = "\uE004",
  clear = "\uE005",
  return = "\uE006",
  enter = "\uE007",
  shift = "\uE008",
  control = "\uE009",
  alt = "\uE00A",
  pause = "\uE00B",
  escape = "\uE00C",
  space = "\uE00D",
  page_up = "\uE00E",
  page_down = "\uE00F",
  end = "\uE010",
  home = "\uE011",
  left_arrow = "\uE012",
  up_arrow = "\uE013",
  right_arrow = "\uE014",
  down_arrow = "\uE015",
  insert = "\uE016",
  delete = "\uE017",
  semicolon = "\uE018",
  equals = "\uE019",
  numpad_0 = "\uE01A",
  numpad_1 = "\uE01B",
  numpad_2 = "\uE01C",
  numpad_3 = "\uE01D",
  numpad_4 = "\uE01E",
  numpad_5 = "\uE01F",
  numpad_6 = "\uE020",
  numpad_7 = "\uE021",
  numpad_8 = "\uE022",
  numpad_9 = "\uE023",
  multiply = "\uE024",
  add = "\uE025",
  separator = "\uE026",
  subtract = "\uE027",
  decimal = "\uE028",
  divide = "\uE029",
  f1 = "\uE031",
  f2 = "\uE032",
  f3 = "\uE033",
  f4 = "\uE034",
  f5 = "\uE035",
  f6 = "\uE036",
  f7 = "\uE037",
  f8 = "\uE038",
  f9 = "\uE039",
  f10 = "\uE03A",
  f11 = "\uE03B",
  f12 = "\uE03C",
  command_meta = "\uE03D"
)

ycred='https://login.yahoo.com/?.src=ym&.lang=en-US&.intl=us&.done=https%3A%2F%2Fmail.yahoo.com%2Fd%3F.src%3Dfp'
user <- "LarrySloman@yahoo.com"
pass <- "Fastsix123"
###################### findElement(By.cssSelector("body")).sendKeys(Keys.CONTROL, Keys.PAGE_DOWN);

## This block will open the Chrome browser, which is linked to R
rD <-rsDriver(chromever = "81.0.4044.69")
remDr <- rD[["client"]]

#// Start Login
while(TRUE){
  pushover(paste('LOGGING INTO SIMPLISAFE',Sys.time()))
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
  Sys.sleep(3)
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
    zz[[idxs[1]]]$clickElement() # click on newest verify email message
    zz=remDr$findElement(using = "partial link text",value="Verify Device")
    Sys.sleep(4)
    zz$clickElement() # click Verify Device
    print('Verify Button Clicked')
    next
  }else{
    print('SimpliSafe Login Successful')
  }
  Sys.sleep(4)
  
  while(TRUE){
    remDr$navigate('https://webapp.simplisafe.com/new/#/timeline')
    Sys.sleep(4)
#    evtxts=remDr$findElements(using = 'class', "event-text")
    evinfo=remDr$findElements(using = 'class', "event-info")
    if(len(evinfo)==0)
      break # indlcates logged out
    for(jj in 1:5){
      print(evinfo[[jj]]$getElementText())
    }
    print(Sys.time())
    Sys.sleep(30)
  }
}
remDr$close()
remDr$closeServer()
rD[["server"]]$stop()
rm(rD)
gc()




