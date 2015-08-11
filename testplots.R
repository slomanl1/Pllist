library(gWidgets)
options(guiToolkit = "RGtk2")                     # avoid question if more than one is installed
# w <- gwindow("Hello world example")             # top level window
# g <- ggroup(cont=w, horizontal=FALSE)           # a box container, added to w
# b <- gbutton("Click me for a message", cont=g)  # add button to container g
# addHandlerClicked(b, handler=function(h,...) {  # add interactivity through a handler
#   galert("Hello world", parent=h$obj)
# })
testplots = function(fnames) {
  idxs=NULL # indices of fnames in an[ttl]
  
  for(x in 1:len(fnames)) idxs=c(idxs,which(grepl(fnames[x],an[ttl],fixed=TRUE)))
  
  if (!.GlobalEnv$tpexist) {
    .GlobalEnv$avail = FALSE
    .GlobalEnv$renamed = FALSE
    .GlobalEnv$ssv = NULL
    heit=min((100+len(fnames)*170),1900)
    w <- gwindow(paste(liner,"Choose One or More Files\n"),width = 1900,height=heit,parent = c(0,0))
    gp <- ggroup(horizontal = FALSE, container = w)
    tab <- gtable(fnames, container = gp, expand = TRUE,multiple = TRUE,
                  handler = function(h,...) {
                    print(svalue(h$obj))
                    .GlobalEnv$ssv = as.character(svalue(h$obj))
                    .GlobalEnv$avail = TRUE
                    .GlobalEnv$ww=w # pass window to main for dispose()
                  }
    )
    addHandlerRightclick(
      tab, handler = function(h,...) {
        if ((length(svalue(h$obj) > 0)) & !.GlobalEnv$gdfopen) {
          idx=which(grepl(basename(as.character(svalue(h$obj)))[1],gdframe$fnx,fixed = TRUE))
          print(paste('idx=',idx))
          msgg = an[ttl][idx]
          msgg = substr(msgg,10,255)
          svalue(w) <- as.character(msgg)
          ofnx=gdframe[idx,]
          nfn=NULL
          .GlobalEnv$gdfopen=TRUE
          if(exists('fw'))
            rm(fw)
          fwind=gdf(ofnx, container=gwindow("Edit File Details",width=1900,height = 20))
          addHandlerDestroy(
            fwind, handler = function(h,...) {
              .GlobalEnv$gdfopen=FALSE
            }
          )
          addhandlerchanged(fwind, handler = function(h,...) 
          {.GlobalEnv$fw=fwind})
          while(!exists('fw'))
          {};
          nfnx=fw[,]
          nfn=nfnx$fnx
          ofn=ofnx$fnx
          print(paste('nfn=',nfn))
          
          if(dirname(nfn)!=dirname(ofn)){
            print(paste('New dir name',dirname(nfn),'Not equal to old',dirname(ofn)))
            dispose(fwind)
          }else{
            if(length(nfn)>0){
              if(nfn!=ofn){
                if(file.rename(ofn,nfn)){
                  print(paste("file rename successful",ofn,nfn))
                  an[ttl][idx]=sub(ofn,nfn,an[ttl][idx],fixed=TRUE) # replace old filename with new filename
                  print(paste('new anttlidx=',an[ttl][idx]))
                  save(an,file='AN.RData')
                  .GlobalEnv$renamed = TRUE
                  fnx1 = an[ttl][idxs]
                  ttls = unlist(regexpr(EOFN,fnx1))
                  ttls[ttls < 0] = 500
                  fnx= substr(fnx1,10,ttls - 2)
                  print(paste('fnx=',fnx))
                  comments=substr(fnx1,ttls,nchar(pnoln))
                  print(get_list_content(fnx,comments))
                  tab[] <- get_list_content(fnx,comments) # refresh gtable(write updated table to tab)
                  
                }else{
                  print(paste("file rename FAILED",ofn,nfn))}
              }
            }
          }
        }
      }
    )
    
    bg <- ggroup(container = gp)
    addSpring(bg)
    gbutton("dismiss", container = bg, handler = function(h,...) {
      .GlobalEnv$tpexist <- FALSE
      .GlobalEnv$avail = TRUE
      dispose(w)
      if(exists('fw', envir = .GlobalEnv)){
        if(isExtant(.GlobalEnv$fw))
          dispose(.GlobalEnv$fw)} # fw is gdf window
      .GlobalEnv$gdfopen=FALSE
    }
    )
    addHandlerDestroy(
      tab, handler = function(h,...) {
        .GlobalEnv$ssv = NULL
        .GlobalEnv$avail = TRUE
        .GlobalEnv$tpexist <- FALSE
        if(exists('fw', envir = .GlobalEnv)){
          if(isExtant(.GlobalEnv$fw))
            dispose(.GlobalEnv$fw)} # fw is gdf window
        .GlobalEnv$gdfopen=FALSE
      }
    )
    .GlobalEnv$tpexist <- TRUE
  }
}