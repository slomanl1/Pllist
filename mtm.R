############# Prints file modification time and time zone for current directory
mtm=function(x){
  dts=shell('xdir "/Form=*F *d *t"',intern = TRUE)
  poss=regexpr('/',dts)
  return(strptime(substr(dts,poss-2,1000),format = '%d/%m/%Y %H:%M:%S'))
}
