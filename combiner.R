scriptStatsRemoveAll <- "~/Pllist.git/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
print('Combiner')
source('~/Local.R') #get drive
setwd(paste(pldrive,'My Playlists',sep=""))

source('~/Local.R') #get drive
gg=matrix(c('ussf.wpl','ussfd.wpl','sfall.WPL','ussf.WPL','swa.WPL','uwa.WPL','ah.WPL','blah.WPL','stp.WPL',
            'utp.WPL','bl.WPL','blah.WPL','b.wpl','bfa.wpl','wa.wpl','utp.wpl','ussf.wpl','ussfa.wpl','uwa.wpl','vva.wpl'),10,2,byrow=TRUE)
lsa=''
lim=dim(gg)[1]
for (i in 1:lim){
  lss=readLines(gg[i,1])
  lss1=readLines(gg[i,2])
  lss2=c(lss[1:(length(lss)-3)],lss1[16:length(lss1)])
  writeLines(unique(lss2),gg[i,1])
}


