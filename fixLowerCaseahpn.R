load('~/dfan.RData')
source('~/pllist.git/addStudioToDmfnfo.R')
setwd('D:/PNMTALL/RPDNClips')
ndc=subset(dfan,grepl('best ever|Yom',DMComment) & grepl(' ah\\+| hb\\+| pn\\+| cu\\+| dg\\+',DMComment))
ndc$DMComment=sub(' ah\\+',' AH\\+',ndc$DMComment)
ndc$DMComment=sub(' hb\\+',' HB\\+',ndc$DMComment)
ndc$DMComment=sub(' pn\\+',' PN\\+',ndc$DMComment)
ndc$DMComment=sub(' cu\\+',' CU\\+',ndc$DMComment)
ndc$DMComment=sub(' dg\\+',' DG\\+',ndc$DMComment)

wrStud(as.character(ndc$filename),as.character(ndc$studio),as.character(ndc$DMComment))


