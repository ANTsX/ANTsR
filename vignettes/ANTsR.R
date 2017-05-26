## ----global options, include=FALSE---------------------------------------
library(knitr)
library(ANTsR)
runLongExamples<-FALSE

## ----basic read,message=FALSE,warnings=FALSE-----------------------------
img<-antsImageRead( getANTsRData("r16"), 2 ) # built in image
img

## ----basic_plot,message=FALSE,warnings=FALSE,echo=FALSE------------------
img2<-antsImageRead( getANTsRData("r64"), 2 ) # built in image
invisible(plot(img2))

## ----physical space------------------------------------------------------
mnifilename<-getANTsRData("r27")
img<-antsImageRead(mnifilename, pixeltype="unsigned char")
img
retval<-antsImageWrite(img,mnifilename)
antsGetSpacing(img)
antsGetDirection(img)
antsGetOrigin(img)
print(img[120,122]) # same type of thing in 3 or 4D
print(max(img))

