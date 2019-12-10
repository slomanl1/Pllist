datestr = function(s){
paste(floor(floor(floor(s/60)/60)/24)%%24,"DAYS",
    floor(floor(s/60)/60)%%60,"HOURS",
    floor(s/60)%%60,"MINUTES",
    s%%60,"SECONDS"  ) 
}

shell('exiftool -r D:\\pnmtall\\* | findstr "File.Name Duration" >\\users\\larry\\ddur.txt')
shell('exiftool -r C:\\pnmtall\\* | findstr "File.Name Duration" >\\users\\larry\\cdur.txt')
dd=readLines('c:/users/Larry/ddur.txt')
dd=c(dd,readLines('c:/users/Larry/cdur.txt'))
dx=subset(dd,grepl('^Duration|File|Play Duration',dd)&!grepl('Duration S',dd)&!grepl('Duration V',dd))
dy=c(dx[which(diff(as.numeric(as.factor(substr(dx,1,10))))!=0)],dx[len(dx)])
ddf=data.frame(fn=subset(dy,grepl('File Name',dy)),duration=subset(dy,grepl('Duration',dy)))
ddf$duration=sub('Play',' ',ddf$duration)
gg=strsplit(ddf$duration,':|s')
durs=sapply(1:nrow(ddf),function(x) {return(sum(as.numeric(gg[[x]][2]),as.numeric(gg[[x]][3]),as.numeric(gg[[x]][4])*60,na.rm = TRUE))})
print(datestr(sum(durs)))

