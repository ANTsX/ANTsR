#' joint intensity fusion
#'
#' Estimates an image from another set of images - intensity generalization of joint label fusion.  Search radius used only when employing labels - WIP to speed it up.
#'
#' @param targetI antsImage to be approximated
#' @param targetIMask mask with value 1
#' @param atlasList list containing antsImages
#' @param beta weight sharpness, default to 2
#' @param rad neighborhood radius, default to 3
#' @param doscale scale neighborhood intensities
#' @param doNormalize normalize each image range to 0, 1
#' @param maxAtlasAtVoxel min/max n atlases to use at each voxel
#' @param rho ridge penalty increases robustness to outliers but also
#'   makes image converge to average
#' @param usecor employ correlation as local similarity
#' @param rSearch radius of search, default is 2
#' @param boundary.condition one of 'image' 'mean' 'NA'
#' @param jifImage the current estimated jif image (helps speed slice by slice)
#' @param imputeList 2nd modality to impute
#' @param imputedImage current estimate of 2nd modality
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
#' implist<-list()
#' for ( i in 1:length(ilist) )
#'  {
#'  ilist[[i]]<-iMath(ilist[[i]],"Normalize")
#'  mytx<-antsRegistration(fixed=ref , moving=ilist[[i]] ,
#'    typeofTransform = c("Affine") )
#'  mywarpedimage<-antsApplyTransforms(fixed=ref,moving=ilist[[i]],
#'    transformlist=mytx$fwdtransforms)
#'  ilist[[i]]=mywarpedimage
#'  implist[[i]]=mywarpedimage %>% iMath("Laplacian")
#'  }
#' r<-2
#' d<-2
#' pp<-jointIntensityFusion( ref, refmask, ilist, rSearch=1, rad=rep(r,d) )
#' pp<-jointIntensityFusion( ref, refmask, ilist, rSearch=1, rad=rep(r,d),
#'   imputeList=implist )
#'
#' @export jointIntensityFusion
jointIntensityFusion <- function( targetI, targetIMask, atlasList,
  beta=4, rad=NA, doscale = TRUE,
  doNormalize=TRUE, maxAtlasAtVoxel=c(1,Inf), rho=0.01,
  usecor=FALSE, boundary.condition='image', rSearch=2,
  jifImage=NA, imputeList=NA, imputedImage=NA )
{
  BC=boundary.condition
  nvox = sum( targetIMask == 1 )
  includezero = TRUE
  if ( is.na(jifImage) ) jifImage = targetI * 0
  doImputation = FALSE
  if ( is.na(imputedImage) & ! all( is.na( imputeList ) ) )
    {
    imputedImage = targetI * 0
    doImputation = TRUE
    }
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
  ct<-1
  natlas<-length(atlasList)
  atlasLabels<-1:natlas
  if ( maxAtlasAtVoxel[2] > natlas ) maxAtlasAtVoxel[2]<-natlas
  progress <- txtProgressBar(min = 0,
                max = ncol(targetIv), style = 3)
  badct<-0
  basewmat<-t(replicate(length(atlasList), rep(0.0,n) ) )
  for ( voxel in 1:ncol( targetIv ) )
    {
    zsd<-rep(1,natlas)
    wmat<-basewmat
    targetint<-targetIv[,voxel]
    cent<-indices[voxel,]
    intmat<-matrix( 0, ncol=length(targetint)  , nrow=natlas )
    if ( doImputation )
      {
      if ( length(imputeList) != length(atlasList))
        stop("imputation list must be same length as atlas list")
      impmat<-matrix( 0, ncol=length(targetint)  , nrow=natlas )
      }
    for ( ct in 1:natlas)
      {
      if ( !doImputation )
        nhsearch = .Call("jointLabelFusionNeighborhoodSearch",
          targetint, cent, max(rad), rSearch,
          atlasList[[ct]],
          atlasList[[ct]], PACKAGE="ANTsR" )
      if ( doImputation )
        nhsearch = .Call("jointLabelFusionNeighborhoodSearch",
          targetint, cent, max(rad), rSearch,
          atlasList[[ct]],
          imputeList[[ct]], PACKAGE="ANTsR" )
      segval = nhsearch[[ 1 ]]
      v = nhsearch[[ 2 ]]
      vmean = nhsearch[[ 3 ]]
      sdv = nhsearch[[ 4 ]]
      bestcor = nhsearch[[ 5 ]]
      intmat[ct, ] = v
      if ( doImputation ) impmat[ct, ] = nhsearch[[ 6 ]]
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
      }
    if ( sum(zsd) > (2) )
      {
      cormat = ( ( (wmat) %*% t(wmat) ) / ( ncol(wmat) - 1 ) )^beta
      tempmat = ( (cormat) + diag(ncol(cormat)) * rho )
      onev<-rep(1,ncol(cormat))
      wts = solve( tempmat, onev )
      wts = wts * 1.0 / sum( wts * onev )

# this approach leads to positive weights but may not be as nice
#      cormat = ( ( (wmat) %*% t(wmat) ) / ( ncol(wmat) - 1 ) )
#      invmat<-solve( cormat + diag(ncol(cormat))*rho )^beta
#      wts<-as.numeric( invmat %*% onev / ( sum( onev * invmat %*% onev )) )
      if ( ! is.na( mean(wts)) ) {
        pwts = wts
        pwts[ wts < 0 ] = 0
        pwts = pwts / sum( pwts )
        lintensity = pwts %*% intmat
        .Call("addNeighborhoodToImage",
            jifImage, cent, rad, lintensity,
            package="ANTsR" )
        if ( doImputation )
          {
          impintensity = pwts %*% impmat
          .Call("addNeighborhoodToImage",
            imputedImage, cent, rad, impintensity,
            package="ANTsR" )
          }
      } else badct<-badct+1
    }
  } # loop over voxels
  close( progress )
  rm( targetIv )
  rm( indices )
  if ( ! doImputation ) return( jifImage )
  return( list( jifImage, imputedImage ) )
}
