print('RMZIPS.R')
xxx=shell('curl -l ftp://192.168.0.54/backup/ --user hassio:fastsix123ftp',intern = TRUE)
fnc=xxx
fns=fnc[file_ext(fnc)=='zip' & !is.na(fnc)]
if(len(fns)>0){
  cmdv="shell('curl -v ftp://192.168.0.54/backup/ --user hassio:fastsix123ftp -Q\"DELE /backup/%s\"')" 
  joe=eval(parse(text=sprintf(cmdv,fns)))
}else{
  print('NO ZIPS FOUND')
}
