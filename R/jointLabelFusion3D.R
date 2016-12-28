#' jointLabelFusion3D
#'
#' Estimates an labelset from another set of 3D labels
#'
#' joint label fusion. this version is more efficient,
#' memory-wise, for 3D images. it is a thin wrapper
#' that goes slice-by-slice but produces the same results.
#' This program is for research.  For studies, please use jointLabelFusion in
#' ANTs or in ANTsR.
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
#' @param usecor employ correlation as local similarity
#' @param rSearch radius of search, default is 2
#' @param slices vector defining slices to use (speeds parameter selection)
#' @return approximated image, segmentation and probabilities
#' (latter are WIP, might be done by the time your read this ) ...
#' @author Brian B. Avants, Hongzhi Wang, Paul Yushkevich
#' @keywords fusion, template
#' @examples
#'
#' # see jointLabelFusion for a detailed example
#' # defaults for this function are current recommended parameters
#'
#' @export jointLabelFusion3D
jointLabelFusion3D <- function( targetI, targetIMask, atlasList,
  beta=4, rad=NA, labelList=NA, doscale = TRUE,
  doNormalize=TRUE, maxAtlasAtVoxel=c(1,Inf), rho=0.01, # debug=F,
  usecor=FALSE, rSearch=0, slices=NA )
{
  stop("Probably should not use this function. Use jointLabelFusion instead.")
  if (nargs() == 0)
    {
    print(args(jointLabelFusion3D))
    return(1)
    }
  if ( targetI@dimension != 3 )
    {
    print("must be a 3D image")
    return(NA)
    }
  segvals<-NA
  jifImage = targetI * 0
  if ( ! all( is.na(labelList) ) )
    {
    segmat<-imageListToMatrix( labelList, targetIMask )
    if ( all( is.na(segvals) ) )
      {
      segvals<-c(sort( unique( as.numeric(segmat)) ))
      if ( ! ( 0 %in% segvals ) ) segvals<-c(0,segvals)
      }
    rm(segmat)
    }
  probimgs=list() # weight for each label
  for ( i in 1:length(segvals) )
    probimgs[[ i ]] = targetI * 0
  maskout<-antsImageClone( targetIMask )
  maskout[ targetIMask==1 ]<-0
  whichMaskSlice<-0
  if ( all( is.na(rad) ) ) rad<-rep(4,3)
  if ( all(is.na(slices))  )
    slices<-seq.int(1,dim(targetI)[3], by=1)
#    slices<-seq.int(1,dim(targetI)[3], by=rad[3]*2+1)
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
      oo2d<-jointLabelFusion( targetI=targetI,
        targetIMask=mask2d, atlasList=atlasList,
        beta=beta, rad=rad, labelList=labelList, rho=rho
#        doscale=doscale, doNormalize=doNormalize,
#        maxAtlasAtVoxel=maxAtlasAtVoxel, segvals=segvals,
#        usecor=usecor, probimgs=probimgs, jifImage=jifImage
        )
      whichMaskSlice<-whichMaskSlice+1
      }
    } # endfor
#  finalseg=imageListToMatrix( allprobimgs, targetIMask  )
  finalseg=imageListToMatrix( oo2d$probimgs, targetIMask  ) # FIXME see above
  finalsegvec = apply( finalseg, FUN=which.max , MARGIN=2 )
  segimg = makeImage( targetIMask , finalsegvec  )
  # finally, remap to original segmentation labels
  rsegimg = segimg * 0
  for ( ct in 1:length(segvals) )
    rsegimg[  segimg == ct ] = segvals[ ct ]
  rm( segimg )
  return( list( segimg=rsegimg, mask=maskout,
     probimgs=oo2d$probimgs, predimg=jifImage,
     segvals=segvals ) )
}
