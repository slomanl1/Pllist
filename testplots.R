library(gWidgets)
options(guiToolkit = "RGtk2")                     # avoid question if more than one is installed
# w <- gwindow("Hello world example")             # top level window
# g <- ggroup(cont=w, horizontal=FALSE)           # a box container, added to w
# b <- gbutton("Click me for a message", cont=g)  # add button to container g
# addHandlerClicked(b, handler=function(h,...) {  # add interactivity through a handler
#   galert("Hello world", parent=h$obj)
# })
testplots = function(fnames) {
  if (!.GlobalEnv$tpexist) {
    .GlobalEnv$avail = FALSE
    w <-
      gwindow(
        paste(liner,"Choose One or More Files\n"),width = 800,parent = c(0,0)
      )
    gp <- ggroup(horizontal = FALSE, container = w)
    tab <- gtable(
      fnames, container = gp, expand = TRUE,multiple = TRUE,
      handler = function(h,...) {
        print(svalue(h$obj))
        .GlobalEnv$ssv = svalue(h$obj)
        .GlobalEnv$avail = TRUE
      }
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