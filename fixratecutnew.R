source('~/pllist.git/isVC1HEVC.R')
cd('D:/pnmtall/newdownloads')
dd=dir()
dx=sapply(dd,isHevc)
dcc=data.frame(attributes(dx),dx,stringsAsFactors = FALSE)
ndd=subset(dcc,!grepl('RATE_cut_New.mp4',names)&dx)
dxx=strsplit(ndd,'RATE')
ofn=ndd
nfn=sapply(1:len(dxx), function(x) paste(dxx[[x]][1],'RATE_cut_New.mp4',sep=''))
file.rename(ofn,nfn)

