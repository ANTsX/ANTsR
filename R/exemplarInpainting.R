#' @name exemplarInpainting
#' @title Uses example images to inpaint or approximate an existing image.
#' @description  Employs a robust regression approach to learn the relationship between a sample image and a list of images that are mapped to the same space as the sample image.  The regression uses data from an image neighborhood.
#' @usage  approximg<-exemplarInpainting( img, paintMask, list(img1,img2))
#' @param img - antsImage to be approximated / painted
#' @param painting mask with values 1 or values 1 and 2 - if there is a 2 then it will learn from label 1 to paint label 2
#' @param a list containing antsImages
#' @param featureRadius - radius of image neighborhood e.g. 2
#' @param putbackoriginaldata - put values in label 1 in approximated image
#' @param sharpen - sharpen the approximated image
#' @return inpainted image
#' @author Brian B. Avants
#' @keywords inpainting template
#' @examples
#' fi<-replicate(100, rnorm(100))
#' fi[1:10,]<-fi[,1:10]<-fi[91:100,]<-fi[,91:100]<-0
#' fi<-as.antsImage( fi , 'float' )
#' SmoothImage(2,fi,1.5,fi)
#' mo<-as.antsImage( replicate(100, rnorm(100))  )
#' mo2<-as.antsImage( replicate(100, rnorm(100))  )
#' mask<-getMask(fi,mean(fi),Inf,TRUE)
#' ilist<-list(mo,mo2)
#' painted<-exemplarInpainting(fi,mask,ilist)
exemplarInpainting<-function( img, paintMask,
  imageList, featureRadius=2,
  putbackoriginaldata=FALSE, sharpen=FALSE )
{
mask<-antsImageClone( paintMask )
mask[ paintMask != 1 ]<-0 # dont use the lesion
inpaintLesion<-FALSE
if ( max( paintMask ) == 2 ) inpaintLesion<-TRUE
if ( inpaintLesion )
  {
  lmask<-antsImageClone( paintMask )
  lmask[ paintMask != 2 ]<-0 # use the lesion
  }
targetvoxels<-img[ mask == 1 ]
radius <- rep(featureRadius,img@dimension)
nmat<-matrix()
lmat<-matrix()
for ( i in ilist )
  {
  mat<-(antsGetNeighborhoodMatrix( i, mask, radius,
    boundary.condition='mean'))
  if ( all(dim(nmat)==1) ) nmat<-t(mat) else nmat<-cbind(nmat,t(mat))
  if ( inpaintLesion )
    {
    mat<-(antsGetNeighborhoodMatrix(i,lmask,radius,
      boundary.condition='mean'))
    if ( all(dim(lmat)==1) ) lmat<-t(mat) else lmat<-cbind(lmat,t(mat))
    }
  }
return(list(img,mask))
nmatdf<-data.frame(nmat)
print(dim(nmatdf))
print(length(targetvoxels))
mdl<-rlm( targetvoxels ~ ., data=nmatdf )
if ( inpaintLesion )
  {
  lmatdf<-data.frame(lmat)
  lesvox<-predict(mdl,newdata=lmatdf)
  }
pvox<-predict(mdl,type='response')
if (  putbackoriginaldata )
 pvox<-img[ mask == 1 ]
predimg<-makeImage( mask, pvox )
predimg[ mask == 1 ]   <- pvox
if ( inpaintLesion )
  predimg[ paintMask == 2  ] <- lesvox
if ( sharpen )
  ImageMath(img@dimension,predimg,'Sharpen',predimg)
if (  putbackoriginaldata )
 predimg[ mask == 1 ]<-img[ mask == 1 ]
return(predimg)
# for bayesian regression - amazingly fast!
# W2 = invcov.shrink(t(nmat), 0.1)
}
