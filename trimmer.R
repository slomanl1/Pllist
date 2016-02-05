len=function(x) length(x)
source('~/pllist.git/EnterStartStop.R')
source('~/pllist.git/StartMyGuiTrimmer.R')

ToEnd=FALSE
.GlobalEnv$convert=FALSE
.GlobalEnv$bOK=FALSE
.GlobalEnv$Fdate=FALSE
print('Trimmer hello')

shell('nircmd win min process rscript.exe')
options("guiToolkit"="RGtk2")
library(RGtk2)
library(gWidgets)
library(gWidgetsRGtk2)
library(tools)
require(gdata) # required for standalone version
args <- commandArgs(TRUE)
save(args,file='~/args.RData')
svt=args[1]
if(is.na(svt))
  svt=file.choose()
print(paste('svt=',svt))
StartMyGUI()
