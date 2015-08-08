library(gWidgets)
options(guiToolkit = "RGtk2")                     # avoid question if more than one is installed
# w <- gwindow("Hello world example")             # top level window
# g <- ggroup(cont=w, horizontal=FALSE)           # a box container, added to w
# b <- gbutton("Click me for a message", cont=g)  # add button to container g
# addHandlerClicked(b, handler=function(h,...) {  # add interactivity through a handler
#   galert("Hello world", parent=h$obj)
# })
testplots = function(fnames) {
  get_list_content <- function (fnx) data.frame(fnx,cdts=as.character(file.mtime(fnx)),stringsAsFactors =FALSE)
  if (!.GlobalEnv$tpexist) {
    .GlobalEnv$avail = FALSE
    .GlobalEnv$renamed = FALSE
    .GlobalEnv$ssv = NULL
    
    w <- gwindow(paste(liner,"Choose One or More Files\n"),width = 1024,parent = c(0,0))
    gp <- ggroup(horizontal = FALSE, container = w)
    tab <- gtable(get_list_content(fnames), container = gp, expand = TRUE,multiple = TRUE,
      handler = function(h,...) {
        print(svalue(h$obj))
        .GlobalEnv$ssv = as.character(svalue(h$obj))
        .GlobalEnv$avail = TRUE
      }
    )
    addHandlerRightclick(
      tab, handler = function(h,...) {
        if (length(svalue(h$obj)) > 0) {
          msgg = an[ttl][which(grepl(basename(as.character(svalue(h$obj))),an[ttl],fixed = TRUE))]
          msgg = substr(msgg,10,255)
          svalue(w) <- as.character(msgg)
          ofn=as.character(svalue(h$obj)[1])
          dirnm=dirname(ofn)
          nfn=NULL
          if(okCancelBox(paste(substr(msgg,1,230),'Edit Filename ?')))
            nfn=dlgInput('Edit filename',default = basename(ofn))$res

          if(length(nfn)>0){
            nfn=paste(dirnm,nfn,sep='/')
            if(nfn!=ofn){
              if(file.rename(ofn,nfn)){
                print(paste("file rename successful",ofn,nfn))
                idxs=which(grepl(basename(as.character(svalue(h$obj))),
                            an[ttl],fixed = TRUE))
                an[ttl][idxs][1]=paste("========",nfn) # replace with new filename
                .GlobalEnv$renamed = TRUE
                save(an,file='AN.RData')
                fnx1 = substr(an[ttl][idxs],10,1000)
                print(paste('fnx1=',fnx1))
                print(get_list_content(fnx1))
                tab[] <- get_list_content(fnx1)

              }else{
                print(paste("file rename FAILED",ofn,nfn))}
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
      }
    )
    addHandlerDestroy(
      tab, handler = function(h,...) {
        .GlobalEnv$ssv = NULL
        .GlobalEnv$avail = TRUE
        .GlobalEnv$tpexist <- FALSE
      }
    )
    .GlobalEnv$tpexist <- TRUE
  }
}