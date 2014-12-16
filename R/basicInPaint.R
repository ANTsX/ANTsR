#' @name basicInpaint
#' @title Inpaints missing imaging data from boundary data
#' @description  Smooths data along the boundary into the missing region.
#' @usage  approximg<-basicInpaint( img, paintMask )
#' @param img antsImage to be approximated / painted
#' @param paintMask painting mask with values 1 or
#' values 1 and 2 - if there is a 2 then it will learn
#' from label 1 to paint label 2.  should cover the brain.
#' @param smoothParam - larger means smoother
#' @param its - number of iterations
#' @return inpainted image
#' @author Brian B. Avants
#' @keywords inpainting template
#' @examples
#' set.seed(123)
#' fi<-abs(replicate(100, rnorm(100)))
#' fi[1:10,]<-fi[,1:10]<-fi[91:100,]<-fi[,91:100]<-0
#' mask<-fi
#' mask[ mask > 0 ]<-1
#' mask2<-mask
#' mask2[11:20,11:20]<-2
#' mask<-as.antsImage( mask , 'float' )
#' fi<-as.antsImage( fi , 'float' )
#' SmoothImage(2,fi,3,fi)
#' painted<-basicInpaint(fi,mask)
basicInPaint<-function( img, paintMask, smoothParam=1, its=100 )
{
  if (nargs() == 0) {
    print(args(basicInPaint))
    return(1)
  }
  inpainted<-antsImageClone(img)
  lesmask<-antsImageClone(paintMask)
  lesbound<-antsImageClone(paintMask)
  lesmask[ lesmask >=0 ]<-0
  lesmask[ paintMask == 2 ]<-1
  ImageMath(lesmask@dimension,lesbound,"MD",lesmask,1)
  lesbound[ lesmask == 1 & paintMask == 2 ]<-0
  inpainted[ lesmask == 1 ]<-mean( img[ lesbound == 1 ]  )
  # ImageMath 2 temp.nii.gz PropagateLabelsThroughMask lesmask.nii.gz r16slice_lesion_thresh.nii.gz 200000 0
  timg<-antsImageClone(img)
  timg[lesmask==1]<-0
  ImageMath(inpainted@dimension,lesmask,"MD",lesmask,1)
  ImageMath(inpainted@dimension,inpainted,"FastMarchingExtension",
    lesmask, timg, 20000 , 0 )
  return(inpainted)
  for ( i in 1:its ) {
    inpaintedTemp<-antsImageClone(inpainted)
    SmoothImage(inpainted@dimension,inpaintedTemp,smoothParam,inpaintedTemp)
    inpainted[ lesmask == 1 ]<-inpaintedTemp[ lesmask == 1 ]
  }
  return(inpainted)
}
