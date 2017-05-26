## ----simfun--------------------------------------------------------------
library(ANTsR)
simLesion<-function(  img, s , w, thresh=0.01, mask=NA, myseed )
{
  set.seed(myseed)
  img<-iMath(img,"Normalize")
  if ( is.na(mask) ) mask<-getMask(img)
  i<-makeImage( dim(img) , rnorm( length(as.array(img))  ) )
  i[ mask==0 ]<-0
  ni<-smoothImage(i,s)
  ni[mask==0]<-0
  i<-thresholdImage(ni,thresh,Inf)
  i<-iMath(i,"GetLargestComponent")
  ti<-antsImageClone(i)
  i[i>0]<-ti[i>0]
  i<-smoothImage(i,w)
  i[ mask != 1  ] <- 0
  i[ 1:(dim(img)[1]/2), 1:(dim(img)[2]-1) ]<-0
  limg<-( antsImageClone(img) * (-i)  %>% iMath("Normalize") )
  return( list(limg=limg, lesion=i ) )
}

## ----testsub-------------------------------------------------------------
ti<-antsImageRead( getANTsRData("r27") )
timask=getMask(ti)
seg2<-kmeansSegmentation( ti, 3 )$segmentation
ll2<-simLesion( ti, 10, 6, myseed=919 ) # different sized lesion
seg2[ ll2$lesion > 0.5 & seg2 > 0.5 ]<-4

## ----testsub2,echo=FALSE-------------------------------------------------
invisible( plot(ll2$limg) )

## ----runsim--------------------------------------------------------------
img<-antsImageRead( getANTsRData("r16") )
seg<-kmeansSegmentation( img, 3 )$segmentation
ll<-simLesion( img, 12, 5, myseed=1 )
seg[ ll$lesion > 0.5 & seg > 0.5 ]<-4

## ----runsim2,echo=FALSE--------------------------------------------------
invisible( plot(ll$limg) )

## ----runsim1-------------------------------------------------------------
img<-antsImageRead( getANTsRData("r30") , 2  )
seg1<-kmeansSegmentation( img, 3 )$segmentation
ll1<-simLesion( img, 9, 5,  myseed=2 ) # different sized lesion
seg1[ ll1$lesion > 0.5 & seg1 > 0.5 ]<-4

## ----runsim3,echo=FALSE--------------------------------------------------
invisible( plot( ll1$limg ) )

