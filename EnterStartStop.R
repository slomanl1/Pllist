source('~/pllist.git/testnewginput.R')
EnterStartStop = function(x="Enter Start Time (secs) or (mm:ss)\n",allowEnter=FALSE){
  .GlobalEnv$ss=NULL
  while(TRUE){
#     ginput(x, icon="question", title=.GlobalEnv$svt, handler = function(h,...) 
#       .GlobalEnv$ss=h$input)
    ALTGinput(x,allowEnter)
    startt= .GlobalEnv$ss   
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
        }else{
              if(allowEnter){
                startt=10000
                break;
              }

        }
      }
    }else{
      break # bad integer
    }
  }
  return(startt)
}
