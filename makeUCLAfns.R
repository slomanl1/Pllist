load('~/ddUCLA.RData')
k=1
rr=which(dd$fc==123)
x=''
for(i in 1:len(rr)){
  x[i]=''
  for(j in 0:2) {
    x[i]=paste(x[i],trim(dd[rr[i]+j,'ec']),sep='-')
    }
}


y=gsub('Encounter Details','',x)
y=gsub('   ','',y)
y=gsub('--','',y)
y=gsub('.','',y,fixed=TRUE)
y=gsub('CARE','IM CARE',y)
y=gsub('Visit','',y,ignore.case = TRUE)
y=gsub('IM CARE UCLA CENTURY CITY-IM CARE UCLA CENTURY CITY','IM CARE UCLA CENTURY CITY',y,fixed=TRUE)
y=gsub('Stein Eye Institute,-','',y,fixed=TRUE)
y=gsub('OFFICE  ','OFFICE',y,ignore.case = TRUE)

print(y)
