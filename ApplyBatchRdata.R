library(gWidgets2)
options(guiToolkit = "RGtk2") 
source('~/pllist.git/StartMyGuiTrimmer.R')
source('~/pllist.git/ProtoConvertH265Func.R')
source('~/pllist.git/EnterStartStop.R') # for galert()
source('~/pllist.git/addStudioToDmfnfo.R') # wrteStudio, getwplsxx function source
source('~/pllist.git/cleanwplsfn.R') # cleanwplsfn
source('~/pllist.git/WriteDate.R')
source('~/pllist.git/addStudioToDmfnfo.R') # writeStudio, getwplsxx function source
source('~/pllist.git/rmmovname.R')

load('~/batch.RData')
wrStud(Batch$filename,Batch$studio,Batch$Comment)
