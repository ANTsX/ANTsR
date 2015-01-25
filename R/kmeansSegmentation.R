#' k means image segmentation.
#' 
#' k means image segmentation.
#' 
#' 
#' @param k integer number of classes
#' @return segmentation and probability images
#' @author Brian B. Avants
#' @examples
#' 
#' fi<-antsImageRead( getANTsRData('r16') ,2)
#' N3BiasFieldCorrection(2,fi,fi,4)
#' seg<-kmeansSegmentation( fi, 3 )
#' 
#' @export kmeansSegmentation
kmeansSegmentation <- function(img,k,kmask=NA,mrf=0.1){
  kmimg<-antsImageClone(img)
  dim<-img@dimension
  ImageMath(dim,kmimg,"Normalize", kmimg )
  if ( is.na(kmask) ) kmask<-getMask(kmimg,0.01,1,cleanup = 2 )
  ImageMath(dim,kmask,"FillHoles",kmask)
  nhood<-paste(rep(1,dim),collapse='x')
  mrf<-paste("[",mrf,",",nhood,"]")
  kmimg<-Atropos( d = dim, a = kmimg, m = mrf,c = "[5,0]",
      i = paste("kmeans[",k,"]",sep=''), x = kmask)
  kmimg$segmentation<-antsImageClone(kmimg$segmentation, img@pixeltype )
  return( kmimg )
}
