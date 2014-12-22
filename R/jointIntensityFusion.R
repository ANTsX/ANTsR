#' @name jointIntensityFusion
#' @title Estimates an image from another set of images
#' @description  intensity generalization of joint label fusion.
#' @usage  outlist<-jointIntensityFusion(targetI, targetIMask,
#'      atlasList, beta = 1, rad = NA,
#'      labelList = NA, doscale = FALSE)
#' @param targetI antsImage to be approximated
#' @param targetIMask  mask with value 1
#' @param atlasList list containing antsImages
#' @param beta  weight sharpness
#' @param rad  neighborhood radius
#' @param labelList list containing antsImages
#' @param doscale  scale neighborhood intensities
#' @return approximated image
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
#'  tx<-antsRegistration(ref,ilist[[i]],'SyN',tempfile())
#'  ImageMath(2,tx$warpedmovout,"Normalize",tx$warpedmovout)
#'  ilist[[i]]=tx$warpedmovout
#'  seg<-Atropos( d = 2, a = ilist[[i]],   m = mrf, c =conv,  i = km, x = refmask)
#'  seglist[[i]]<-seg$segmentation
#'  }
#' pp<-jointIntensityFusion(ref,refmask,ilist,beta=4,labelList=seglist )
#' mm<-imageListToMatrix(ilist,refmask)
#' avg<-makeImage(refmask,colMeans(mm)) # compare to pp[[1]]
jointIntensityFusion <- function( targetI, targetIMask, atlasList,
    beta=1, rad=NA, labelList=NA, doscale = FALSE ) {
  if (nargs() == 0) {
    print(args(ajointIntensityFusion))
    return(1)
  }
  dim<-targetI@dimension
  if ( all(is.na(rad)) ) rad<-rep(2,dim)
  n<-1
  for ( k in 1:length(rad)) n<-n*(rad[k]*2+1)
  wmat<-t(replicate(length(atlasList), rnorm(n)) )
  matcenter<-round(n/2)+1
  intmat<-wmat
  targetIv<-antsGetNeighborhoodMatrix(targetI,
    targetIMask,rad,boundary.condition="mean")
  if ( doscale ) targetIv<-scale(targetIv)
  imatlist<-list()
  newmeanvec<-rep(0,ncol(targetIv))
  m<-length(atlasList)
  onev<-rep(1,m)
  weightmat<-matrix( rep(0, m*ncol(targetIv) ), nrow=m )
  ct<-1
  for ( i in atlasList )
    {
    imatlist[[ct]]<-antsGetNeighborhoodMatrix(i,
      targetIMask,rad,boundary.condition="mean")
      ct<-ct+1
    }
  for ( voxel in 1:ncol(targetIv) )
    {
      for ( ct in 1:length(imatlist)) {
        v<-imatlist[[ct]][,voxel]
        intmat[ct,]<-v
        if ( doscale ) v<-scale(v)
        wmat[ct,]<-(v-targetIv[,voxel])
      }
      #  cormat<-cor(t(wmat))^beta
      #  wmat<-t(scale(t(wmat)))
      cormat<-antsrimpute(( wmat %*% t(wmat) )^beta)
      invmat<-tryCatch( solve( cormat + diag(ncol(cormat))*1e-2 ) ,
      error = function(e) return( diag(ncol(cormat)) ) )
      if ( typeof(invmat)=='character')
        invmat<-diag(ncol(cormat))
      wts<-invmat %*% onev / ( sum( onev * invmat %*% onev ))
      weightmat[,voxel]<-wts
      newmeanvec[voxel]<-(intmat[,matcenter] %*% wts )[1]
    }
    newmeanvec<-antsrimpute(newmeanvec)
    newmeanvec[newmeanvec>max(targetI)]<-max(targetI)
    newmeanvec[newmeanvec<min(targetI)]<-min(targetI)
    newimg<-makeImage(targetIMask,newmeanvec)
    segimg<-NA
    if ( !( all( is.na(labelList) ) ) )
    {
    segmat<-imageListToMatrix( labelList, refmask )
    segvec<-rep( 0, ncol(segmat) )
    segvals<-sort( unique( as.numeric(segmat)) )
    for ( voxel in 1:ncol(segmat) )
      {
      probvals<-rep(0,length(segvals))
      for ( p in 1:length(segvals))
        {
        ww<-which(segmat[,voxel]==segvals[p])
        if ( length(ww) > 0 )
          {
          probvals[p]<-sum((weightmat[ ww , voxel ]))
          }
        }
      k<-which(probvals==max(probvals,na.rm=T))
      segvec[voxel]=segvals[ k ]
      }
    segimg<-makeImage(targetIMask,segvec)
    }
    return( list(predimg=newimg, localWeights=weightmat,
      segimg=segimg ) )
}
