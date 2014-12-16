#' @name basicInpaint
#' @title Inpaints missing imaging data from boundary data
#' @description  Smooths data along the boundary into the missing region.
#' @usage  approximg<-basicInpaint( img, paintMask )
#' @param img antsImage to be approximated / painted
#' @param paintMask painting mask with values 1 or
#' values 1 and 2 - if there is a 2 then it will learn
#' from label 1 to paint label 2.  should cover the brain.
#' @param speedimage - larger means faster/better
#' @param its - iterations of graddescent
#' @param gparam - graddescent param e.g. 0.05
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
#' mask<-as.antsImage( mask2 , 'float' )
#' fi<-as.antsImage( fi , 'float' )
#' SmoothImage(2,fi,3,fi)
#' painted<-basicInPaint(fi,mask)
#' \dontrun{
#' lmask<-antsImageRead("brainmask.nii.gz",2)
#' limg<-antsImageRead("r16slice_lesion.nii.gz",2)
#' mm<-basicInPaint(limg,lmask)
#' plotANTsImage(mm)
#' }
basicInPaint<-function( img, paintMask, speedimage=NA, its=0,
  gparam=0.05 )
{
  if (nargs() == 0) {
    print(args(basicInPaint))
    return(1)
  }
  inpainted<-antsImageClone(img)
  healthymask<-antsImageClone(paintMask)
  healthymask[ paintMask == 2 ]<-0
  if ( is.na(speedimage) ) {
    speedimage<-antsImageClone(img)
    upit<-mean( img[ paintMask == 2 ] )
    speedimage[ paintMask == 2 ]<-speedimage[ paintMask == 2 ]+upit
  }
  ImageMath(inpainted@dimension,inpainted,"FastMarchingExtension",
    speedimage, healthymask, img  )
  outimg<-antsImageClone(img)
  outimg[ paintMask == 2 ]<-inpainted[ paintMask == 2 ]
  if ( its > 0 )
    {
    for ( i in 1:its )
      {
      soutimg<-antsImageClone(outimg)
      SmoothImage(inpainted@dimension,outimg,1.5,soutimg)
      outimg[ paintMask == 2 ]<-outimg[ paintMask == 2 ]*(1.0-gparam)+
        soutimg[ paintMask == 2 ]*gparam
      }
    }
  return(outimg)
}
