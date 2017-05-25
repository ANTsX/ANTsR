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

## ----indexing------------------------------------------------------------
gaussimg<-array( data=rnorm(125), dim=c(5,5,5) )
arrayimg<-array( data=(1:125), dim=c(5,5,5) )
img<-as.antsImage( arrayimg )
print( max(img) )
print( mean(img[ img > 50  ]))
print( max(img[ img >= 50 & img <= 99  ]))
# if using SUBSET using an antsImage, you must be explicit
sub = as.array(img >= 50 & img <= 99) > 0
print( mean( gaussimg[ sub  ]) )

## ----fourd---------------------------------------------------------------
gaussimg<-makeImage(c(5,5,5,10), voxval = rnorm(125*10)  )
print(dim(gaussimg))
avg3d<-getAverageOfTimeSeries( gaussimg )
voxelselect <- avg3d < 0.25
mask<-antsImageClone( avg3d )
mask[ voxelselect  ]<-0
mask[ !voxelselect  ]<-1
gmat<-timeseries2matrix( gaussimg, mask )
print(dim(gmat))

## ----makeimage-----------------------------------------------------------
newimg<-makeImage( mask, mean(avg3d) )    # from scalar
newimg<-makeImage( mask, colMeans(gmat) ) # from vector

