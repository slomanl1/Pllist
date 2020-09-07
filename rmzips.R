print('RMZIPS.R')
xxx=shell('curl -l ftp://192.168.4.52/usr/share/hassio/homeassistant/ --user pi:fastsix123',intern = TRUE)
fnc=xxx
fns=fnc[file_ext(fnc)=='zip' & !is.na(fnc)]
if(len(fns)>0){
  cmdv="shell('curl -v ftp://192.168.4.52/config/ --user hassio:fastsix123ftp -Q\"DELE /backup/%s\"')" 
  joe=eval(parse(text=sprintf(cmdv,fns)))
}else{
  print('NO ZIPS FOUND')
}
