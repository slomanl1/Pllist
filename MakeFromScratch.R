options("guiToolkit"="RGtk2")
library(RGtk2)
library(gWidgets2)
xx=gconfirm('Are you sure you want to build from scratch')
if(xx){
  sfname = '~/PNMTALL.RDATA'
  unlink(sfname)
  unlink('dfltsave.RData') # clear search selections
  unlink('~/gdframe.RData')
  unlink('~/dfan.RData') # deal with this in dfg merge code below
  source('~/pllist.git/MakeDfanFromAM.R')
}
