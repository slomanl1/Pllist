### FRONT MATTER ####

library(devtools)
library(RSelenium)
library(XML)
library(plyr)
library(httr)

######################
rD <-rsDriver(chromever = "81.0.4044.69")
remDr <- rD[["client"]]
remDr$navigate('https://webapp.simplisafe.com/#/login')
user <- "slomanl75@gmail.com"; pass <- "Fastsix123"

rD[["server"]]$stop()
