cd('c:/myvideos/rpdnclips')
shell('mediainfo *.* | findstr "HEVC Complete" > c:\\Users\\Larry\\Documents\\hevc.txt')
hvc=readLines('~/hevc.txt')
hvc=data.frame(hvc,fc=NA)
hvc$fc=as.numeric(as.factor(substr(hvc$hvc,1,10)))
hx=c(diff(hvc$fc),NA)
hvc$df=hx
hevcs=hvc[grepl('Complete',hvc$hvc) & (hvc$df!=0),'hvc']
hevcs=sub('Complete name                            : ','',hevcs)
hevcs=normalizePath(hevcs,winslash = '/')
bds=data.frame(fname=hevcs,errorC='Already HEVC',md5s=md5sum(hevcs))
save(hvc,hevcs,bds,file='~/hvc.rdata')
load('~/Bads.RData')
bads=bads[!grepl('Clips',bads$fname,ignore.case = TRUE),]
bads=rbind(bads,bds)
bads=bads[!duplicated(bads$fname),]
save(bads, file = "~/bads.RData")

