#' @name exemplarInpainting
#' @title Uses example images to inpaint or approximate an existing image.
#' @description  Employs a robust regression approach to learn the relationship between a sample image and a list of images that are mapped to the same space as the sample image.  The regression uses data from an image neighborhood.
#' @usage  approximg<-exemplarInpainting( img, paintMask, list(img1,img2))
#' @param img - antsImage to be approximated / painted
#' @param painting mask with values 1 or values 1 and 2 - if there is a 2 then it will learn from label 1 to paint label 2
#' @param a list containing antsImages
#' @param featureRadius - radius of image neighborhood e.g. 2
#' @param sharpen - sharpen the approximated image
#' @param feather - value (e.g. 1) that helps feather the mask for smooth blending
#' @return inpainted image
#' @author Brian B. Avants
#' @keywords inpainting template
#' @examples
#' set.seed(123)
#' fi<-abs(replicate(100, rnorm(100)))
#' fi[1:10,]<-fi[,1:10]<-fi[91:100,]<-fi[,91:100]<-0
#' mask<-fi
#' mask[ mask > 0 ]<-1
#' mask<-as.antsImage( mask )
#' fi<-as.antsImage( fi , 'float' )
#' SmoothImage(2,fi,3,fi)
#' mo<-as.antsImage( replicate(100, rnorm(100))  )
#' mo2<-as.antsImage( replicate(100, rnorm(100))  )
#' ilist<-list(mo,mo2)
#' painted<-exemplarInpainting(fi,mask,ilist)
exemplarInpainting<-function( img, paintMask,
  imageList, featureRadius=2,
  sharpen=FALSE, feather=1 )
{
mask<-antsImageClone( paintMask )
mask[ paintMask != 1 ]<-0 # dont use the lesion
inpaintLesion<-FALSE
if ( max( paintMask ) == 2 ) inpaintLesion<-TRUE
if ( inpaintLesion )
  {
  lmask<-antsImageClone( paintMask )
  lmask[ paintMask != 2 ]<-0 # use the lesion
  lmask[ paintMask == 2 ]<-1 # use the lesion
  fmask<-antsImageClone( paintMask )
  fmask[ paintMask < 0.5 ]<-0  # full mask
  fmask[ paintMask >= 0.5 ]<-1 #
  featherMask<-antsImageClone(lmask)
  SmoothImage(img@dimension,featherMask,feather,featherMask)
  featherMask2<-antsImageClone(lmask)
  featherMask2[ featherMask2 >= 0 ]<-1.0
  featherMask2[ featherMask > 0 ]<-1.0-featherMask[ featherMask > 0 ]
  }
targetvoxels<-img[ mask == 1 ]
radius <- rep(featureRadius,img@dimension)
nmat<-matrix()
lmat<-matrix()
fmat<-matrix()
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
    mat<-(antsGetNeighborhoodMatrix(i,fmask,radius,
      boundary.condition='mean'))
    if ( all(dim(fmat)==1) ) fmat<-t(mat) else fmat<-cbind(fmat,t(mat))
    }
  }
nmatdf<-data.frame(nmat)
if (  nrow(nmatdf) != length(targetvoxels) )
  {
  print("nrow(nmatdf) != length(targetvoxels)")
  return( mask )
  }
mdl<-rlm( targetvoxels ~ ., data=nmatdf )
if ( inpaintLesion == FALSE )
  {
  pvox<-predict(mdl,type='response')
  predimg<-makeImage( mask, pvox )
  return(predimg)
  }
# otherwise predict from full mat and feather-combine
lmatdf<-data.frame(fmat)
lesvox<-predict(mdl,newdata=lmatdf)
predimg<-makeImage( fmask, lesvox )
if ( sharpen )
  ImageMath(img@dimension,predimg,'Sharpen',predimg)
# now make two vectors - one for the lesion and
# one for the original image
vec1<-img[ fmask == 1 ]*featherMask2[fmask==1]
vec2<-predimg[ fmask == 1 ]*featherMask[fmask==1]
predimg[fmask==1]<-vec2+vec1
return( predimg )
# for bayesian regression - amazingly fast!
# W2 = invcov.shrink(t(nmat), 0.1)
}
