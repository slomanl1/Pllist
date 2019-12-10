cd('D:/pnmtall/RPDNClips')
shell('exiftool * -CompressorID > c:/users/Larry/Documents/studios.txt')

lns=readLines('~/studios.txt')
lns2=sub('Compressor','',lns)
lns1=gsub(' ','',lns2)

fac=as.numeric(as.factor(substr(lns1,1,8)))
dfx=diff(fac)

df=data.frame(lns1,fac,dff=c(dfx,NA),stringsAsFactors = FALSE)
hevc1=substr(df[df$dff==3,1],9,1000)
hevc=subset(hevc1,!is.na(hevc1))
hevcs=normalizePath(hevc,winslash = '/')
load('~/bads.RData')

baad=data.frame(fname=hevcs,errorC='Already HEVC',md5s='',stringsAsFactors = FALSE)

bds1=rbind(bads,baad)
bds2=subset(bds1,file.exists(as.character(bds1$fname)))
bads=subset(bds2,!duplicated(bds2$fname))
save(bads,file='~/bads.RData')

