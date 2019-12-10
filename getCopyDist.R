load('~/dfan.RData')
zz=subset(dfan,grepl('COPY',DMComment))
pos=unlist(gregexpr('_',zz$filename))
xx=substr(zz$filename,27,pos)
cc=as.data.frame(table(xx))
print(cc[order(cc$Freq,decreasing=TRUE),])

