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
#' @return approximated image, segmentation and probabilities
#' @author Brian B. Avants, Hongzhi Wang, Paul Yushkevich
#' @keywords fusion, template
#' @examples
#' set.seed(123)
#' ref<-antsImageRead( getANTsRData('r16'), 2)
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
#'  tx<-antsRegistration(ref,ilist[[i]],'SyN',tempfile())
#'  ilist[[i]]=tx$warpedmovout
#'  seg<-Atropos( d = 2, a = ilist[[i]],   m = mrf, c =conv,  i = km, x = refmask)
#'  seglist[[i]]<-seg$segmentation
#'  }
#' r<-4
#' d<-2
#' pp<-jointIntensityFusion(ref,refmask,ilist,
#'   beta=1,labelList=seglist, rad=rep(r,d) )
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
    beta=2, rad=NA, labelList=NA, doscale = TRUE,
    doNormalize=TRUE ) {
  if (nargs() == 0) {
    print(args(ajointIntensityFusion))
    return(1)
  }
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
  progress <- txtProgressBar(min = 0,
    max = ncol(targetIv), style = 3)
  for ( voxel in 1:ncol(targetIv) )
    {
      zsd<-rep(1,natlas)
      wmat<-t(replicate(length(atlasList), rep(0.0,n) ) )
      for ( ct in 1:natlas) {
#        v<-imatlist[[ct]][,voxel] # too costly, memory-wise
        cent<-indices[voxel,]
        v<-antsGetNeighborhood(atlasList[[ct]],cent,rad)$values
        intmat[ct,]<-v
        # handle case where sd is 0
        if ( sd(v) == 0 ) {
          zsd[ct]<-0
        }
        if ( doscale ) v<-scale(v)
        wmat[ct,]<-v-targetIv[,voxel]
      }
      if ( sum(zsd) > (natlas/2) )
      {
      wmat<-wmat[zsd==1,]
      cormat<-( wmat %*% t(wmat) )^beta
      invmat<-solve( cormat + diag(ncol(cormat))*1.e-6 )
      onev<-rep(1,sum(zsd))
      wts<-invmat %*% onev / ( sum( onev * invmat %*% onev ))
      weightmat[zsd==1,voxel]<-wts
      wts<-weightmat[,voxel]
      # if ( abs(sum(wts,na.rm=T) - 1) > 0.01 )
      #  wts<-rep(1.0/length(wts),length(wts))
      newmeanvec[voxel]<-(intmat[,matcenter] %*% wts )[1]
      if ( voxel %% 500 == 0 ) {
        setTxtProgressBar( progress, voxel )
      }
      }
    }
    close( progress )
    newimg<-makeImage(targetIMask,newmeanvec)
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
      localWeights=weightmat, probimgs=probImgList  ) )
}
#
##    newmeanvec<-antsrimpute(newmeanvec)
#    newmeanvec[newmeanvec>max(targetI)]<-max(targetI)
#    newmeanvec[newmeanvec<min(targetI)]<-min(targetI)
#  cormat<-cor(t(wmat))^beta
#  wmat<-t(scale(t(wmat)))
#      cormat<-antsrimpute(( wmat %*% t(wmat) )^beta)
#      invmat<-tryCatch( solve( cormat + diag(ncol(cormat))*1e-6 ) ,
#      error = function(e) return( diag(1.0/ncol(cormat)) ) )
#      if ( typeof(invmat)=='character')
#        {
#        wts<-rep(1.0/natlas,natlas)
#        } else {
#        wts<-invmat %*% onev / ( sum( onev * invmat %*% onev ))
#      }
#      wts<-wts*zsd
#
#invmat<-tryCatch
#( solve( cormat + diag(ncol(cormat))*1e-4 ) ,
#error = function(e)
#{
#  return(  diag(1.0/ncol(cormat)) )
#}
#)
#
