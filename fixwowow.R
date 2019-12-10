scriptStatsRemoveAll <- "~/Pllist.git/RemoveAllExceptFuncs.R"
source(scriptStatsRemoveAll) #clear bones
source("~/Local.R")
source('~/pllist.git/addStudioToDmfnfo.R') # wrteStudio, getwplsxx function source
setwd(paste(drive,'PNMTALL/RPDNClips',sep=""))
load('~/dfan.rdata')
ix=which(grepl('WOWOWO',dfan$DMComment)&grepl('RPDNC',dfan$filename))
if(len(ix)>0){
  xx=dfan[ix,'DMComment']
  ss=strsplit(xx,' ')
  ll=sapply(1:len(xx), function(x) which(grepl('WOW',ss[[x]])))
  ln=sapply(1:len(xx), function(x) len(ss[[x]]))
  sq=sapply(1:len(xx), function(x) {
    tt=1:ln[x]
    tn=tt[!tt %in% ll[x]]
    return(tn)
  })
  
  mm=unlist(sapply(1:len(xx), function(x) capture.output(cat(ss[[x]][sq[[x]]],'WOW'))))
  wrStud(dfan[ix,1],dfan[ix,'studio'],mm)
}else{
  galert('NONE FOUND')
}
