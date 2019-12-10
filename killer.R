xx=shell('handle /P rscript.exe',intern=TRUE)
ss=fi('pid',xx)
if(len(ss)>0){
  zz=fi('rpurger.R',xx)
  if(len(zz)){
    pid=as.numeric(strsplit(ss,' ')[[1]][3])
    cmd=sprintf('taskkill /F /IM %s',pid)
    shell(cmd)
  }else{
    print('Rpurger.R not found')
  }

}else{
  print('NONE FOUND')
}


