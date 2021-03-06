flavors <- c("Choose","NO","YES")
avail=FALSE
vall='NO'
flagg=TRUE
f <- function(h,...) {
  .GlobalEnv$avail=TRUE
  .GlobalEnv$vall=svalue(h$obj)
  .GlobalEnv$flagg=FALSE # if TRUE, indicates xw was destroyed with user close
}

xw <- gwindow("Delete sfname and rebuild?",height = 20)
gp <- ggroup(container=xw)

yb=gbutton("YES", container=gp, handler = f)
font(yb) <- c(color="red" , weight="bold")
cb=gbutton("NO", container=gp, handler = f)
font(cb) <- c(color="springgreen4" , weight="bold")
addHandlerKeystroke(xw, f)

addHandlerDestroy(xw, handler = function(h,...) {
  .GlobalEnv$destroyed=.GlobalEnv$flagg
  .GlobalEnv$avail=TRUE
})
cntt=7
Sys.sleep(1)
shell('nircmd win activate title "Delete sfname and rebuild?"')
 
while(!avail)
{
  svalue(xw)=paste("Delete sfname and rebuild?",cntt)
  Sys.sleep(1)
  cntt=cntt-1
  if(cntt==0){
    flagg=FALSE
    break
    }
}
if(isExtant(xw))
  dispose(xw)


