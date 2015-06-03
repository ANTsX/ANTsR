#' jointIntensityFusion3D
#'
#' Estimates an image/labelset from another set of 3D images
#'
#' intensity generalization of joint label fusion, still supports segmentation.
#' this version is more efficient, memory-wise, for 3D images. it is a thin
#' wrapper that goes slice-by-slice but produces the same results.
#'
#'
#' @param targetI antsImage to be approximated
#' @param targetIMask mask with value 1
#' @param atlasList list containing antsImages
#' @param beta weight sharpness, default to 2
#' @param rad neighborhood radius, default to 4
#' @param labelList list containing antsImages
#' @param doscale scale neighborhood intensities
#' @param doNormalize normalize each image range to 0, 1
#' @param maxAtlasAtVoxel min/max n atlases to use at each voxel
#' @param rho ridge penalty increases robustness to outliers but also
#'   makes image converge to average
#' @param useSaferComputation slower but more error checking
#' @param usecor employ correlation as local similarity
#' @param rSearch radius of search, default is 2
#' @param slices vector defining slices to use (speeds parameter selection)
#' @param includezero boolean - try to predict the zero label
#' @return approximated image, segmentation and probabilities
#' (latter are WIP, might be done by the time your read this ) ...
#' @param computeProbs boolean - requires more memory
#' @author Brian B. Avants, Hongzhi Wang, Paul Yushkevich
#' @keywords fusion, template
#' @examples
#'
#' # see jointIntensityFusion for a detailed example
#' # defaults for this function are current recommended parameters
#'
#' @export jointIntensityFusion3D
jointIntensityFusion3D <- function( targetI, targetIMask, atlasList,
  beta=4, rad=NA, labelList=NA, doscale = TRUE,
  doNormalize=TRUE, maxAtlasAtVoxel=c(1,Inf), rho=0.01, # debug=F,
  useSaferComputation=FALSE, usecor=FALSE, rSearch=0, slices=NA,
  includezero=TRUE, computeProbs=FALSE )
{
  if (nargs() == 0)
    {
    print(args(jointIntensityFusion3D))
    return(1)
    }
  if ( targetI@dimension != 3 )
    {
    print("must be a 3D image")
    return(NA)
    }
  segvals<-NA
  localJIF2Dp=NA
  if ( ! all( is.na(labelList) ) )
    {
    segmat<-imageListToMatrix( labelList, targetIMask )
    if ( all( is.na(segvals) ) )
      {
      segvals<-c(sort( unique( as.numeric(segmat)) ))
      if ( ! ( 0 %in% segvals ) ) segvals<-c(0,segvals)
      }
    if ( !includezero ) segvals = segvals[ segvals != 0 ]
    rm(segmat)
    }
  maskout<-antsImageClone( targetIMask )
  maskout[ targetIMask==1 ]<-0
  whichMaskSlice<-0
  if ( all( is.na(rad) ) ) rad<-rep(4,3)
  if ( all(is.na(slices))  ) slices<-1:dim(targetI)[3]
  for ( i in slices )
    {
    mask2d<-antsImageClone(targetIMask)
    mask2d<-as.array(mask2d)
    if ( i < dim(mask2d)[3] & i > 0 )
    if ( sd( mask2d[,,i] ) > 0   )
      {
      for ( j in 1:dim(targetI)[3] )
        {
        if ( j != i ) mask2d[,,j]<-0
        }
      mask2d<-as.antsImage(mask2d)
      mask2d<-antsCopyImageInfo(targetIMask,mask2d)
      maskout[ mask2d == 1 ]<-1
      oo2d<-jointIntensityFusion( targetI=targetI,
        targetIMask=mask2d, atlasList=atlasList,
        beta=beta, rad=rad, labelList=labelList,
        rSearch=rSearch,
        doscale=doscale, doNormalize=doNormalize,
        maxAtlasAtVoxel=maxAtlasAtVoxel, rho=rho, segvals=segvals,
        useSaferComputation=useSaferComputation, usecor=usecor,
        computeProbs=computeProbs )
      if ( whichMaskSlice == 0 )
        {
        localJIF2Di<-oo2d$predimg
        localJIF2Ds<-oo2d$segimg
        if ( computeProbs ) localJIF2Dp<-oo2d$probimgs
        } else {
          localJIF2Di[ mask2d == 1 ]<-localJIF2Di[ mask2d == 1 ]+
            oo2d$predimg[ mask2d == 1 ]
          localJIF2Ds[ mask2d == 1 ]<-localJIF2Ds[ mask2d == 1 ]+
            oo2d$segimg[ mask2d == 1 ]
          probct<-1
          if ( computeProbs )
            for ( probimg in localJIF2Dp )
              {
              probimg[ mask2d == 1 ]<-probimg[ mask2d == 1 ]+
                oo2d$probimgs[[probct]][ mask2d == 1 ]
              probct<-probct+1
              }
        }
      whichMaskSlice<-whichMaskSlice+1
      }
    } # endfor
  return( list( predimg=localJIF2Di, segimg=localJIF2Ds, mask=maskout,
     probimgs=localJIF2Dp ) )
}
