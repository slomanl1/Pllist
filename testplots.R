library(gWidgets)
options(guiToolkit = "RGtk2")                     # avoid question if more than one is installed
# w <- gwindow("Hello world example")             # top level window
# g <- ggroup(cont=w, horizontal=FALSE)           # a box container, added to w
# b <- gbutton("Click me for a message", cont=g)  # add button to container g
# addHandlerClicked(b, handler=function(h,...) {  # add interactivity through a handler
#   galert("Hello world", parent=h$obj)
# })
testplots = function(fnames) {
  w <- gwindow(paste(liner,"Choose One or More Files\n"),width = 600)
  gp <- ggroup(horizontal = FALSE, container = w)
  tab <- gtable(
    fnames, container = gp, expand = TRUE,multiple = TRUE,
    handler = function(h,...)
      print(svalue(h$obj))
  )
  addHandlerRightclick(
    tab, handler = function(h,...) {
      if (length(svalue(h$obj)) > 0) {
        msgg = an[ttl][which(grepl(basename(svalue(h$obj)),an[ttl],fixed = TRUE))]
        msgg = substr(msgg,10,255)
        msgBox(msgg)
      }
    }
  )
  
  bg <- ggroup(container = gp)
  addSpring(bg)
  gbutton(
    "dismiss", container = bg, handler = function(h,...)
      dispose(w)
  )
}
