testplots = function(fnames) { #fnames is a data frame
  idxs=NULL
  for(x in 1:nrow(fnames)) 
    idxs=c(idxs,which(grepl(fnames[x,'fnx'],an[ttl],fixed=TRUE)))
  print(.GlobalEnv$changed)
  if (!.GlobalEnv$tpexist) {
    .GlobalEnv$avail = FALSE
    .GlobalEnv$renamed = FALSE
    .GlobalEnv$ssv = NULL
    .GlobalEnv$changed = TRUE
    heit=min((100+nrow(fnames)*25),1900)
    w <- gwindow(paste(liner,"Choose One or More Files, Click Right to Edit Filename and Comments\n"),width = 1900,height=heit,parent = c(0,0))
    gp <- ggroup(horizontal = FALSE, container = w)
    .GlobalEnv$tpexist <- TRUE
    .GlobalEnv$ww <- w
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
          .GlobalEnv$idx=which(grepl(basename(as.character(svalue(h$obj)))[1],fnames$fnx,fixed = TRUE))
          print(paste('idx=',idx))
          .GlobalEnv$ofnx=fnames[idx,]
          ofnxa=fnames[idx,]
          nfn=NULL
          .GlobalEnv$gdfopen=TRUE
          enabled(w) <- FALSE
          fwind=gdf(ofnxa, container=gwindow("Edit File Details",width=1900,height = 20))
          .GlobalEnv$fw=fwind
          addHandlerDestroy(
            fwind, handler = function(h,...) {
              .GlobalEnv$gdfopen=FALSE
              enabled(w) <- TRUE
            }
          )
          addhandlerchanged(fwind, handler = function(h,...) 
          { print('changed handler (fwind)')
            .GlobalEnv$changed=TRUE
            })
        }
      }
    )
    addHandlerDestroy(
      tab, handler = function(h,...) {
        .GlobalEnv$ssv = NULL
        .GlobalEnv$avail = TRUE
        .GlobalEnv$tpexist <- FALSE
        print('tab destroyed handler')
        if(exists('fw', envir = .GlobalEnv)){
          print('fw exists')
          if(isExtant(.GlobalEnv$fw))
            dispose(.GlobalEnv$fw)} # fw is gdf window
        .GlobalEnv$gdfopen=FALSE
      }
    )
    print('bghello')
    bg <- ggroup(container = gp)
    .GlobalEnv$tab <- tab
    addSpring(bg)
    gbutton("dismiss", container = bg, handler = function(h,...) {
      .GlobalEnv$tpexist <- FALSE
      .GlobalEnv$avail = TRUE
      wx=.GlobalEnv$ww
      #dispose(wx)
      visible(w) <- FALSE
      .GlobalEnv$gdfopen=FALSE
    }
    )
  }
  print('enter sub while')
  while(!.GlobalEnv$changed)
  {};
  print('changed handler (fw)')
  .GlobalEnv$changed=FALSE
  ofnx=.GlobalEnv$ofnx
  if(len(ofnx)>0){
    print('changed handler (fwofnx>0)')
    fw=.GlobalEnv$fw
    nfnx=fw[,]
    print('changed handler (nfnx created OK')
    nfn=nfnx$fnx
    ofn=ofnx$fnx
    ofc=ofnx$comments
    nfc=nfnx$comments
    idx=.GlobalEnv$idx
    print(paste('ofnx,nfnx',ofnx,nfnx)) ######### debug only
    
    if(dirname(nfn)!=dirname(ofn)){
      print(paste('New dir name',dirname(nfn),'Not equal to old',dirname(ofn)))
      dispose(fwind)
    }else{
      if(length(nfn)>0){
        if(any(nfnx!=ofnx)){ # all fields in DF compared
          if(file.rename(ofn,nfn)){
            print(paste("file rename successful,old an[ttl][idxs][idx]=",ofn,nfn,an[ttl][idxs][idx]))
            an[ttl][idxs][idx]=sub(ofn,nfn,an[ttl][idxs][idx],fixed=TRUE) # replace old filename with new filename
            an[ttl][idxs][idx]=sub(ofc,nfc,an[ttl][idxs][idx],fixed=TRUE) # replace old comments with new comments
            save(an,file='AN.RData')
            .GlobalEnv$renamed = TRUE
            fnx1 = an[ttl][idxs]
            ttls = unlist(regexpr(EOFN,fnx1))
            ttls[ttls < 0] = 500
            fnx= substr(fnx1,10,ttls - 2)
            comments=substr(fnx1,ttls,nchar(fnx1))
            tab=.GlobalEnv$tab
            tab[,] <- get_list_content(fnx,comments) # refresh gtable(write updated table to tab)
          }else{
            print(paste("file rename FAILED",ofn,nfn))}
        }
      }
    }
  }  
  # end of rightclickhandler for gtable (tab)

}

#exiftool  -Title=[test] 