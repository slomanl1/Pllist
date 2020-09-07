load('~/mfnfo.RData')
dx=data.frame(bn=sort(as.integer(mfnfo$bn)))
dx$df=c(diff(dx$bn),NA)
print(dx$bn[which(dx$df!=1)]+1)

