library(gWidgets2)
options(guiToolkit = "RGtk2") 
source('~/pllist.git/StartMyGuiTrimmer.R')
source('~/pllist.git/ProtoConvertH265Func.R')
source('~/pllist.git/EnterStartStop.R') # for galert()
source('~/pllist.git/addStudioToDmfnfo.R') # wrteStudio, getwplsxx function source
source('~/pllist.git/cleanwplsfn.R') # cleanwplsfn
source('~/pllist.git/Buildmfnfo.R')
scriptStatsRemoveAll <- "~/Pllist.git/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones

setwd('D:/PNMTALL/RPDNClips')
load('~/mfnfo.RData')

pp=subset(mfnfo,nchar(cmt)==0)
pp$cmt="Added to RPDNClips by makelast"
wrStud(pp$lsst,pp$studio,pp$cmt)
