#' joint label fusion
#'
#' A multiple atlas voting scheme to customize labels for a new subject.
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
#' @param boundary.condition one of 'image' 'mean' 'NA'
#' @param segvals list of labels to expect
#' @return approximated image, segmentation and probabilities
#' @author Brian B. Avants, Hongzhi Wang, Paul Yushkevich
#' @keywords fusion, template
#' @examples
#'
#' set.seed(123)
#' ref<-antsImageRead( getANTsRData("r16"))
#' ref<-resampleImage(ref,c(50,50),1,0)
#' ref<-iMath(ref,"Normalize")
#' mi<-antsImageRead( getANTsRData("r27"))
#' mi2<-antsImageRead( getANTsRData("r30"))
#' mi3<-antsImageRead( getANTsRData("r62"))
#' mi4<-antsImageRead( getANTsRData("r64"))
#' mi5<-antsImageRead( getANTsRData("r85"))
#' refmask<-getMask(ref)
#' refmask<-iMath(refmask,"ME",2) # just to speed things up
#' ilist<-list(mi,mi2,mi3,mi4,mi5)
#' seglist<-list()
#' for ( i in 1:length(ilist) )
#'  {
#'  ilist[[i]]<-iMath(ilist[[i]],"Normalize")
#'  mytx<-antsRegistration(fixed=ref , moving=ilist[[i]] ,
#'    typeofTransform = c("Affine") )
#'  mywarpedimage<-antsApplyTransforms(fixed=ref,moving=ilist[[i]],
#'    transformlist=mytx$fwdtransforms)
#'  ilist[[i]]=mywarpedimage
#'  seg<-kmeansSegmentation( ilist[[i]], k=3, kmask = refmask)
#'  seglist[[i]]<-seg$segmentation
#'  }
#' r<-2
#' pp<-jointLabelFusion(ref,refmask,ilist, rSearch=2,
#'   labelList=seglist, rad=rep(r, length(dim(ref)) ) )
#'
#' @export jointLabelFusion
jointLabelFusion <- function( targetI, targetIMask, atlasList,
  beta=4, rad=NA, labelList=NA, doscale = TRUE,
  doNormalize=TRUE, maxAtlasAtVoxel=c(1,Inf), rho=0.01, # debug=F,
  usecor=FALSE, boundary.condition='image',
  rSearch=2, segvals=NA )
{
  haveLabels=FALSE
  BC=boundary.condition
  nvox = sum( targetIMask == 1 )
  if ( length(labelList) != length(atlasList) )
    stop("length(labelList) != length(atlasList)")
  includezero = TRUE
  if ( !( all( is.na(labelList) ) ) )
    {
    haveLabels=TRUE
    if ( all( is.na(segvals) ) )
      {
      segmat<-imageListToMatrix( labelList, targetIMask )
      segvals<-c(sort( unique( as.numeric(segmat)) ))
      if ( includezero )
        if ( ! ( 0 %in% segvals ) ) segvals<-c(0,segvals)
      if ( !includezero )
        segvals<-segvals[  segvals != 0 ]
      }
    }
  posteriorList=list() # weight for each label
  for ( i in 1:length(segvals) )
    posteriorList [[ i ]] = targetI * 0
  dim<-targetI@dimension
  if ( doNormalize )
    {
    for ( i in atlasList ) i = iMath(i,"Normalize")
    targetI = iMath(targetI,"Normalize")
    }
  if ( all(is.na(rad)) ) rad<-rep(3,dim)
  n<-1
  for ( k in 1:length(rad)) n<-n*(rad[k]*2+1)
  wmat<-t(replicate(length(atlasList), rep(0.0,n) ) )
  matcenter<-round(n/2)+1
  targetIvStruct<-getNeighborhoodInMask(targetI,
    targetIMask,rad,boundary.condition=BC,spatial.info=T)
  targetIv<-targetIvStruct$values
  indices<-targetIvStruct$indices
  rm( targetIvStruct )
  if ( doscale ) targetIv<-scale(targetIv)
  m<-length(atlasList)
  onev<-rep(1,m)
  ct<-1
  natlas<-length(atlasList)
  atlasLabels<-1:natlas
  if ( maxAtlasAtVoxel[2] > natlas ) maxAtlasAtVoxel[2]<-natlas
  progress <- txtProgressBar(min = 0,
                max = ncol(targetIv), style = 3)
  badct<-0
  basewmat<-t( replicate(length(atlasList), rep(0.0,n) ) )
  for ( voxel in 1:ncol( targetIv ) )
    {
    zsd<-rep(1,natlas)
    wmat<-basewmat
    targetint<-targetIv[,voxel]
    cent<-indices[voxel,]
    segmat<-matrix( 0, ncol=length(targetint)  , nrow=natlas )
    for ( ct in 1:natlas)
      {
      nhsearch = .Call("jointLabelFusionNeighborhoodSearch",
        targetint, cent, max(rad), rSearch,
        atlasList[[ct]],
        labelList[[ct]], PACKAGE="ANTsR" )
      segval = nhsearch[[ 1 ]]
      v = nhsearch[[ 2 ]]
      vmean = nhsearch[[ 3 ]]
      sdv = nhsearch[[ 4 ]]
      bestcor = nhsearch[[ 5 ]]
      segmat[ct, ] = nhsearch[[ 6 ]]
      if ( sdv == 0 ) {
        zsd[ct]<-0 # assignment
        sdv<-1
        } else sdv=sd(v)
      if ( doscale ) {
        v<-( v - mean(v) )/sdv
        }
      if ( !usecor )
        wmat[ct,]<-abs(v-(targetint)) # assignment
      else {
        ip<- ( v * targetint )
        wmat[ct,]<-( ip*(-1.0))  # assignment
        }
      } # for all atlases
    if ( maxAtlasAtVoxel[2] < natlas ) {
      ords<-order(rowMeans(abs(wmat)))
      inds<-maxAtlasAtVoxel[1]:maxAtlasAtVoxel[2]
      # zsd[ ords[-inds] ]<-0
      }
    if ( sum(zsd) > (2) )
      {
      cormat = ( ( (wmat) %*% t(wmat) ) / ( ncol(wmat) - 1 ) )^beta
      tempmat = ( antsrimpute(cormat) + diag(ncol(cormat)) * rho )
      onev<-rep(1,ncol(cormat))
      wts = solve( tempmat, onev )
      wts = wts * 1.0 / sum( wts * onev )
      if ( ! is.na( mean(wts)) ) {
        # hongzhi method
        segct = 1
        for ( lseg in segvals )
          {
          lsegmat = segmat * 0
          lsegmat[ segmat == lseg ] = 1
          lsegprobs = wts %*% lsegmat
          lsegprobs[ lsegprobs <  0 ] = 0
          .Call("addNeighborhoodToImage",
            posteriorList[[segct]], cent, rad, lsegprobs,
            package="ANTsR" )
          segct = segct + 1
          }
      } else badct<-badct+1
    }
  } # loop over voxels
  totalImage = targetIMask * 0
  for ( poo in 1:length(posteriorList) )
    totalImage = totalImage + posteriorList[[poo]]
  selector = totalImage > 0
  for ( poo in 1:length(posteriorList) )
    {
    posteriorList[[poo]][ selector ] =
      posteriorList[[poo]][ selector ] /
      totalImage[ selector ]
    }
  close( progress )
  rm( segmat )
  rm( targetIv )
  rm( indices )
  finalseg=imageListToMatrix( posteriorList, targetIMask  )
  finalsegvec = apply( finalseg, FUN=which.max , MARGIN=2 )
  segimg = makeImage( targetIMask , finalsegvec  )
  # finally, remap to original segmentation labels
  rsegimg = segimg * 0
  for ( ct in 1:length(segvals) )
    rsegimg[  segimg == ct ] = segvals[ ct ]
  rm( segimg )
  return( list( segimg=rsegimg, probimgs=posteriorList,
    badct=badct, segvals=segvals  ) )
}
