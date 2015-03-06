#' @title save.ANTsR
#' @description Save and load ANTsR sessions. 
#' @usage save.ANTsR(filename="./ANTsRsession", objects=NA, ...)
#' @param filename Prefix for folder to store data. 
#' @param objects Vector of character names of objects to store.  Can be antsImages.
#' @param ... Additional arguments to pass to \code{save}.
#' @examples
#' a <- 1 
#' b <- c(2,3,4)
#' img <- antsImageRead(getANTsRData('r16'), 2)
#' save.ANTsR(objects=c('b', 'img'))
#' load.ANTsR("./ANTsRsession")
#' @name save.ANTsR 

save.ANTsR <- function(filename="./ANTsRsession", objects=NA, ...){
  if(all(is.na(objects))) objects <- ls(envir = .GlobalEnv) 
  myimgs <- rep(FALSE, length(objects))
  for(ii in 1:length(objects)) {
    if(is.antsImage(eval(as.name(objects[ii])))){
      myimgs[ii] <- TRUE
    }
  }
  dir.create(filename)
  rdatfile <- file.path(filename, ".RData")
  ANTsRimgnames <- objects[myimgs]
  ANTsRImageData <- data.frame(names=ANTsRimgnames, 
    dims=rep(NA, length(ANTsRimgnames)), pixeltypes=rep(NA, length(ANTsRimgnames)))
  if(length(ANTsRimgnames) >= 1) {
    for(ii in 1:length(ANTsRimgnames)){
      antsImageWrite(eval(as.name(ANTsRimgnames[ii])), file.path(filename, 
        paste(ANTsRimgnames[ii], ".nii.gz", sep="")))
      ANTsRImageData[ii, "pixeltypes"] <- (eval(as.name(ANTsRimgnames[ii])))@pixeltype
      ANTsRImageData[ii, "dims"] <- (eval(as.name(ANTsRimgnames[ii])))@dimension
    }
  }
  
  save(list=c(objects[!myimgs], "ANTsRImageData"), file=rdatfile, ...)
}

#' @usage load.ANTsR(filename)
#' @describeIn save.ANTsR Load saved ANTsR session.
load.ANTsR <- function(filename){
  load(file.path(filename, ".RData"), envir=globalenv())
  #need images accessible within local function environment too
  load(file.path(filename, ".RData"))
  for(ii in 1:nrow(ANTsRImageData)){
    imgname <- ANTsRImageData$names[ii]  
    assign(as.character(ANTsRImageData$names[ii]),  antsImageRead(
      file.path(filename, paste(as.character(ANTsRImageData$names[ii], 
        ".nii.gz", sep=""))), 
      ANTsRImageData$dims[ii], 
      ANTsRImageData$pixeltypes[ii]), envir=.GlobalEnv) 
  }
  rm(ANTsRImageData, envir=.GlobalEnv)
}
