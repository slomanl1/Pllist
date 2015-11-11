EnterStartStop = function(x="Enter Start Time (secs) or (mm:ss)\n"){
  while(TRUE){
    startt <- dlgInput(x)$res;
    if(len(startt)>0){
      if(!is.na(as.integer(startt))){
        break # good integer
      }else{
        cpos=regexpr(':',startt)
        if(cpos>0){
          f1=as.integer(substr(startt,1,cpos-1))
          f2=as.integer(substr(startt,cpos+1,nchar(startt)))
          if (f1>=0 & f1<60 & f2>=0 & f2<60){
            break # good mm:ss
          }
        }
        
      }
    }else{
      break # bad integer
    }
  }
  return(startt)
}