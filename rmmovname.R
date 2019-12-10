source("~/Local.R")
library(tools)
source('~/pllist.git/addStudioToDmfnfo.R') # writeStudio, getwplsxx function source
source('~/pllist.git/EnterStartStop.R') # for galert()
source('~/pllist.git/cleanwplsfn.R') # cleanwplsfn
source('~/pllist.git/getMetadata.R')
options("guiToolkit"="RGtk2")
library(RGtk2)
library(gWidgets2)
rmmovname = function(ifile='',warn=FALSE,ofile=ifile){
  OK=FALSE
  ifile=normalizePath(ifile,winslash = '/',mustWork=FALSE)
  if(!file.exists(ifile)){
    print(paste('ERROR Input file',ifile, 'Does not exist'))
    ofile=NULL  # return value
  }else{
    OK=TRUE
    ofile=normalizePath(ofile,winslash = '/',mustWork=FALSE)
    if(file.exists(ofile)){
      if(warn){
        print(paste('Warning: Output file already exists',ofile))
        OK=gconfirm('Overwrite?')
      }
    }
    if(OK){
      load('~/dirtbl.RData')
      galert('Removing Movie Name')
      print('Removing Movie Name')      
      dtt=file.mtime(ifile)
      zzx=getMetadata(ifile, FALSE) # do not save result to prevent destroy of saved metadata
      cmdd='shell("ffmpeg -i %s -map_metadata -1 -c:v copy -c:a copy %s",mustWork=NA,translate=TRUE)'
      if(ofile!=ifile){
        cmdd=sprintf(cmdd,ifile,ofile)
        eval(parse(text=cmdd))
      }else{
        suppressWarnings(file.remove('temp.mp4'))
        cmdd=sprintf(cmdd,ifile,'temp.mp4')
        eval(parse(text=cmdd))
        zzy=getMetadata('temp.mp4',FALSE)
        #if(file.size('temp.mp4')==file.size(ofile)){
        if(zzx$format[1]==zzy$format[1] & zzx$ImageSize==zzy$ImageSize){
          suppressWarnings(file.remove(ofile))
          file.copy('temp.mp4',ofile)
        }else{
          galert('Removal of Movie Name FAILED')
          return()
        }
      }
      if(!zzx$studio %in% dirtbl$Var){
        galert('rmmovname: Invalid Studio')
        zzx$studio=''
      }
      wrStud(ofile,zzx$studio,zzx$cmt,zzx$title,zzx$subtitle)
      if(file.exists(ofile)){
        WriteDate(ofile,dtt)
      }else{
        file.copy('temp.mp4',ofile)
        print('File restored after wrStud ERROR')
      }
      if(file.exists(ofile)){
        file.remove('temp.mp4')
        print('Movie Name Removed')
      }else{
        file.copy('temp.mp4',ofile)
        print('File restored after writeDate ERROR')
        file.remove('temp.mp4')
      }
      
      
    }
  }
  return(ofile)
}

hasMovieName = function(fn){
  xx=shell(paste('c:/Users/Larry/Documents/hexDump/bin/medi.bat "',
                 fn,'" ' ,sep=''),translate = TRUE, intern = TRUE)
  return(any(grepl('Movie name',xx)))
}
