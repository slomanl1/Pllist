lst=ls()
ln=length(lst)
for(i in 1:ln){
if(typeof(eval(parse(text=lst[i]))) != 'closure' & lst[i]!='i' & lst[i]!='ln' & lst[i]!='lst' ){
	#print(paste('rm(',lst[i],')'))
	eval(parse(text=paste('rm(',lst[i],')')))
	}
}
rm(i,lst,ln)
graphics.off()
clearWarnings= function() assign("last.warning", NULL, envir = baseenv()) #clear warnings
clearWarnings()
if(exists('pb'))
	close(pb)
while(sink.number()>0)
	sink()
options(width=250)
while(dev.cur() >1) dev.off() # close pdf
cd() # set pwd






