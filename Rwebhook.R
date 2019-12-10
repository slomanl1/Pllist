library(gmailr)
require(tools)
len=function(x) length(x)
tryCatch.W.E=function(expr) # from demo(error.catching)
{
  W <- NULL
  w.handler <- function(w){ # warning handler
    W <<- w
    invokeRestart("muffleWarning")
  }
  eere=list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
                                        warning = w.handler), warning = W)
  if(exists('eere')){
    if(!is.null(attributes(eere$value))&!is.atomic(eere$value)){
      em=eere$value$message
      if(!is.null(em)){
        print('ERROR')
        print(str(eere))
        if(grepl('recursive',em))
          browser()
        load('/home/pi/errLog.RData')
        
        xx=as.character(eere$value$call)
        ec=capture.output(cat(xx))
        if(!len(ec))
          ec='None'
        ew=ifelse(is.null(eere$warning),'',eere$warning)
        ts=Sys.time()
        ET=data.frame(em=em,ec=ec,ew=ew,ts=ts)
        if(identical(names(ET),names(errLog))){
          errLog=rbind(errLog,ET)
          errLog=errLog[!duplicated(errLog$ts),]
          save(errLog,file='/home/pi/errLog.RData')
        }else{
          galert('TryCatch.W.E Internal Error')
        }
        
      }
    }
    return(eere)
  }
}

#gmail_auth(secret_file='/home/pi/Desktop/client_secret_606060696458-1ui182cjfr9v4avg4inv1dl6trp17je3.apps.googleusercontent.com.json')
tryCatch.W.E(gm_auth_configure(path="c:/users/larry/Desktop/client_secret_277762207508-2bailcqk4fm0ru6r9t8ia9q0kmg5l6ub.apps.googleusercontent.com.json"))
while(TRUE){
  tryCatch.W.E({zz=gm_messages(include_spam_trash=TRUE)})
  tryCatch.W.E({dd=gm_id(zz)})
  if(len(dd)>0){
    tryCatch.W.E({msgg=capture.output(gm_message(tail(dd,1)))})
    if(any(grepl('ON',msgg)))
      tryCatch.W.E(system('curl -X POST http://192.168.0.77:8123/api/webhook/3312548')) # Motion On
    if(any(grepl('OFF',msgg)))
      tryCatch.W.E(system('curl -X POST http://192.168.0.77:8123/api/webhook/3312549')) # Motion Off
    for(i in 1:len(dd)){
      tryCatch.W.E(print(gm_message(dd[i])))
      errx=tryCatch.W.E(gm_delete_message(dd[i]))
    }
  }
Sys.sleep(.5)
}
while(TRUE){
  if(file.exists('/home/pi/dashfile.txt')){
    file.remove('/home/pi/dashfile.txt')
    gm_auth_configure(path="c:/users/larry/Desktop/client_secret_277762207508-2bailcqk4fm0ru6r9t8ia9q0kmg5l6ub.apps.googleusercontent.com.json")
    dd=id(gm_messages(include_spam_trash=TRUE))
    mn=readLines('/home/pi/purgerlog.txt')
    if(len(mn)>1000)
      mn=''
    writeLines(c(mn,sprintf("Date: %s found %d messages",date(),len(dd))),"/home/pi/purgerlog.txt")
    if(len(dd)>0){
      for(i in 1:len(dd)){
        print(gm_message(dd[i]))
        errx=tryCatch.W.E(gm_delete_message(dd[i]))
      }
    }else{
      print('RAPSBERRY PI MAILBOX IS EMPTY')
    }
    system('sudo cp /home/pi/purgerlog.txt /usr/share/hassio/share/pp.txt')
  }
  Sys.sleep(10)
}
#slomanl75 - gmail_auth(secret_file="/home/pi/Desktop/client_secret_277762207508-2bailcqk4fm0ru6r9t8ia9q0kmg5l6ub.apps.googleusercontent.com.json")
#raspberrypi275 - gmail_auth(secret_file="/home/pi/Desktop/client_secret_606060696458-1ui182cjfr9v4avg4inv1dl6trp17je3.apps.googleusercontent.com.json"



