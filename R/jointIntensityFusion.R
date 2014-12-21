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
#' @param labelList list containing antsImages - not yet used
#' @param doscale  scale neighborhood intensities
#' @return approximated image
#' @author Brian B. Avants, Hongzhi Wang, Paul Yushkevich
#' @keywords fusion, template
#' @examples
#' set.seed(123)
#' ref<-antsImageRead( getANTsRData('r16') ,2)
#' ImageMath(2,ref,"Normalize",ref)
#' mi<-antsImageRead( getANTsRData('r27') ,2)
#' mi2<-antsImageRead( getANTsRData('r30') ,2)
#' mi3<-antsImageRead( getANTsRData('r62') ,2)
#' mi4<-antsImageRead( getANTsRData('r64') ,2)
#' mi5<-antsImageRead( getANTsRData('r85') ,2)
#' refmask<-getMask(ref)
#' ilist<-list(mi,mi2,mi3,mi4,mi5)
#' for ( i in 1:length(ilist) )
#'   {
#'   tx<-antsRegistration(ref,ilist[[i]],'SyN',tempfile())
#'   ImageMath(2,tx$warpedmovout,"Normalize",tx$warpedmovout)
#'   ilist[[i]]=tx$warpedmovout
#'   }
#' pp<-jointIntensityFusion(ref,refmask,ilist)
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
  matcenter<-round(ncol(wmat)/2.0)
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
        if ( doscale ) v<-scale(v)
        intmat[ct,]<-v
        wmat[ct,]<-(v-targetIv[,voxel])
      }
      #  cormat<-cor(t(wmat))^beta
      #  wmat<-t(scale(t(wmat)))
      cormat<-antsrimpute(( wmat %*% t(wmat) )^beta)
      invmat<-solve( cormat + diag(ncol(cormat))*1e-2 )
      wts<-invmat %*% onev / ( sum( onev * invmat %*% onev ))
      weightmat[,voxel]<-wts
      newmeanvec[voxel]<-(intmat[,matcenter] %*% wts )[1]
    }
    newmeanvec<-antsrimpute(newmeanvec)
    newmeanvec[newmeanvec>max(targetI)]<-max(targetI)
    newmeanvec[newmeanvec<min(targetI)]<-min(targetI)
    newimg<-makeImage(targetIMask,newmeanvec)
    return( list(predimg=newimg, localWeights=weightmat) )
}
