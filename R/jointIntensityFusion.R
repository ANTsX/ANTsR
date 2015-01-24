#' @name jointIntensityFusion
#' @title Estimates an image from another set of images
#' @description  intensity generalization of joint label fusion.
#' @usage  outlist<-jointIntensityFusion(targetI, targetIMask,
#'      atlasList, beta = 1, rad = NA,
#'      labelList = NA, doscale = FALSE)
#' @param targetI antsImage to be approximated
#' @param targetIMask  mask with value 1
#' @param atlasList list containing antsImages
#' @param beta  weight sharpness, default to 2
#' @param rad  neighborhood radius, default to 4
#' @param labelList list containing antsImages
#' @param doscale  scale neighborhood intensities
#' @param doNormalize  normalize each image range to 0, 1
#' @param maxAtlasAtVoxel  min/max n atlases to use at each voxel
#' @param rho ridge penalty increases robustness to outliers
#' @param useSaferComputation slower but more error checking
#' @param usecor employ correlation as local similarity
#' @return approximated image, segmentation and probabilities
#' @author Brian B. Avants, Hongzhi Wang, Paul Yushkevich
#' @keywords fusion, template
#' @examples
#' set.seed(123)
#' ref<-antsImageRead( getANTsRData('r16'), 2)
#' ImageMath(2,ref,"Normalize",ref)
#' mi<-antsImageRead( getANTsRData('r27'),  2)
#' mi2<-antsImageRead( getANTsRData('r30') ,2)
#' mi3<-antsImageRead( getANTsRData('r62') ,2)
#' mi4<-antsImageRead( getANTsRData('r64') ,2)
#' mi5<-antsImageRead( getANTsRData('r85') ,2)
#' refmask<-getMask(ref,mean(ref) )
#' ilist<-list(mi,mi2,mi3,mi4,mi5)
#' km<-"kmeans[3]"; mrf<-"[0.2,1x1]"; conv<-"[5,0]"
#' seglist<-list()
#' for ( i in 1:length(ilist) )
#'  {
#'  ImageMath(2,ilist[[i]],"Normalize",ilist[[i]])
#'  tx<-antsRegistration(ref,ilist[[i]],'SyN',tempfile())
#'  ilist[[i]]=tx$warpedmovout
#'  seg<-Atropos( d = 2, a = ilist[[i]],   m = mrf, c =conv,  i = km, x = refmask)
#'  seglist[[i]]<-seg$segmentation
#'  }
#' r<-4
#' d<-2
#' pp<-jointIntensityFusion(ref,refmask,ilist,
#'   labelList=seglist, rad=rep(r,d) )
#' mm<-imageListToMatrix(ilist,refmask)
#' avg<-makeImage(refmask,colMeans(mm)) # compare to pp[[1]]
#' # save memory by separating masks
#' refmaske<-antsImageClone(refmask)
#' ImageMath(d,refmaske,"ME",refmask,15)
#' refmask[refmaske==1]<-0
#' pp1<-jointIntensityFusion(ref,refmask,ilist,
#'   beta=2,rad=rep(r,d))
#' pp2<-jointIntensityFusion(ref,refmaske,ilist,
#'   beta=2,rad=rep(r,d))
#' pp1[[1]][refmaske==1]<-pp2[[1]][refmaske==1]
jointIntensityFusion <- function( targetI, targetIMask, atlasList,
  beta=1, rad=NA, labelList=NA, doscale = TRUE,
  doNormalize=TRUE, maxAtlasAtVoxel=c(1,Inf), rho=0.1, # debug=F,
  useSaferComputation=FALSE, usecor=FALSE )
{
  if (nargs() == 0) {
    print(args(ajointIntensityFusion))
    return(1)
  }
  havefastsvd<-F
  pckg <- try(require(RcppArmadillo))
  if (!pckg) { havefastsvd<-F }
  dim<-targetI@dimension
  if ( doNormalize )
    {
    for ( i in atlasList ) ImageMath(dim,i,"Normalize",i)
    ImageMath(dim,targetI,"Normalize",targetI)
    }
  if ( all(is.na(rad)) ) rad<-rep(2,dim)
  n<-1
  for ( k in 1:length(rad)) n<-n*(rad[k]*2+1)
  wmat<-t(replicate(length(atlasList), rep(0.0,n) ) )
  matcenter<-round(n/2)+1
  intmat<-wmat
  targetIvStruct<-antsGetNeighborhoodMatrix(targetI,
    targetIMask,rad,boundary.condition="image",spatial.info=T)
  targetIv<-targetIvStruct$values
  indices<-targetIvStruct$indices
  rm(targetIvStruct)
  if ( doscale ) targetIv<-scale(targetIv)
  newmeanvec<-rep(0,ncol(targetIv))
  m<-length(atlasList)
  onev<-rep(1,m)
  weightmat<-matrix( rep(0, m*ncol(targetIv) ), nrow=m )
  ct<-1
  natlas<-length(atlasList)
  atlasLabels<-1:natlas
  maxSimImg<-rep(0,ncol(targetIv))
  if ( maxAtlasAtVoxel[2] > natlas ) maxAtlasAtVoxel[2]<-natlas
  progress <- txtProgressBar(min = 0,
                max = ncol(targetIv), style = 3)
  basewmat<-t(replicate(length(atlasList), rep(0.0,n) ) )
  for ( voxel in 1:ncol(targetIv) )
    {
    zsd<-rep(1,natlas)
    wmat<-basewmat
    for ( ct in 1:natlas)
      {
      # is this a BUG/FIXME?
      # see antsImage_GetNeighborhood
      cent<-indices[voxel,]+1
      v<-antsGetNeighborhood(atlasList[[ct]],cent,rad)$values
      intmat[ct,]<-v
      sdv<-sd(v)
      if ( sdv == 0 ) {
        zsd[ct]<-0
        sdv<-1
        }
      if ( doscale ) {
        v<-( v - mean(v))/sdv
      }
      if ( usecor )
        wmat[ct,]<-(v-targetIv[,voxel])
      else {
        wmat[ct,]<-( 1.0 - cor(v,targetIv[,voxel]) )
        }
    }
    if ( maxAtlasAtVoxel[2] < natlas ) {
      ords<-order(rowMeans(abs(wmat)))
      inds<-maxAtlasAtVoxel[1]:maxAtlasAtVoxel[2]
      zsd[ ords[-inds] ]<-0
      }
    if ( sum(zsd) > (2) )
      {
      wmat<-wmat[zsd==1,]
      cormat<-( wmat %*% t(wmat) )
      cormatnorm<-norm(cormat,"F")
      corrho<-cormatnorm*rho
#      cormat<-cor(t(wmat)) # more stable wrt outliers
      if ( useSaferComputation ) # safer computation
      {
      tempf<-function(betaf)
        {
        if ( havefastsvd )
          invmat<-pinv(
            cormat + diag(ncol(cormat))*corrho, 0.01 )^betaf
        if (!havefastsvd )
          invmat<-solve( cormat + diag(ncol(cormat))*corrho )^betaf
        onev<-rep(1,sum(zsd))
        wts<-invmat %*% onev / ( sum( onev * invmat %*% onev ))
        return(wts)
        }
      wts<-tryCatch( tempf(beta),
          error = function(e)
            {
            szsd<-sum(zsd)
            wts<-rep(1.0/szsd,szsd)
            return( wts )
            }
        )
      } else {
        invmat<-solve( cormat + diag(ncol(cormat))*corrho )^beta
        onev<-rep(1,sum(zsd))
        wts<-invmat %*% onev / ( sum( onev * invmat %*% onev ))
      }
      if ( ! is.na( mean(wts)) ) {
        weightmat[zsd==1,voxel]<-wts
        maxSimImg[ voxel ]<-atlasLabels[zsd==1][  which.max(wts) ]
        newmeanvec[voxel]<-(intmat[zsd==1,matcenter] %*% wts)[1]
      }
      if ( FALSE ) {
        print("DEBUG MODE")
        print(maxAtlasAtVoxel)
            return(
                list(voxel=voxel,
                     wts=wts,intmat=intmat,
                     wmat=wmat,cormat=cormat, pvox=newmeanvec[voxel],
                     zsd=zsd,intmatc=intmat[zsd==1,matcenter])
                )
              }
      if ( voxel %% 500 == 0 ) {
            setTxtProgressBar( progress, voxel )
        }
    }
  }
  close( progress )
  newimg<-makeImage(targetIMask,newmeanvec)
  maxSimImg<-makeImage(targetIMask,maxSimImg)
  segimg<-NA
  probImgList<-NA
  if ( !( all( is.na(labelList) ) ) )
    {
    segmat<-imageListToMatrix( labelList, refmask )
    segvec<-rep( 0, ncol(segmat) )
    segvals<-sort( unique( as.numeric(segmat)) )
    probImgList<-list()
    probImgVec<-list()
    for ( p in 1:length(segvals) )
      probImgVec[[p]]<-rep(0,ncol(segmat))
    for ( voxel in 1:ncol(segmat) )
      {
      probvals<-rep(0,length(segvals))
      for ( p in 1:length(segvals))
        {
        ww<-which(segmat[,voxel]==segvals[p] &
          weightmat[  , voxel ] > 0 )
          if ( length(ww) > 0 )
            {
            probvals[p]<-sum((weightmat[ ww , voxel ]))
            }
        }
      probvals<-probvals/sum(probvals)
      for ( p in 1:length(segvals))
        probImgVec[[p]][voxel]<-probvals[p]
      k<-which(probvals==max(probvals,na.rm=T))
      segvec[voxel]=segvals[ k ]
      }
    for ( p in 1:length(segvals) )
      probImgList[[p]]<-makeImage( targetIMask, probImgVec[[p]] )
    segimg<-makeImage(targetIMask,segvec)
    }
  return( list( predimg=newimg, segimg=segimg,
    localWeights=weightmat, probimgs=probImgList,
    maxSimImg=maxSimImg  ) )
}
