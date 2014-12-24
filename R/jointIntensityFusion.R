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
    beta=2, rad=NA, labelList=NA, doscale = TRUE ) {
  if (nargs() == 0) {
    print(args(ajointIntensityFusion))
    return(1)
  }
  dim<-targetI@dimension
  for ( i in atlasList ) ImageMath(dim,i,'Normalize',i)
  ImageMath(dim,targetI,"Normalize",targetI)
  if ( all(is.na(rad)) ) rad<-rep(2,dim)
  n<-1
  for ( k in 1:length(rad)) n<-n*(rad[k]*2+1)
  wmat<-t(replicate(length(atlasList), rnorm(n)) )
  matcenter<-round(n/2)+1
  intmat<-wmat
  targetIvStruct<-antsGetNeighborhoodMatrix(targetI,
    targetIMask,rad,boundary.condition="image",spatial.info=T)
  if ( doscale ) targetIvStruct$values<-scale(targetIvStruct$values)
#  imatlist<-list()
  newmeanvec<-rep(0,ncol(targetIvStruct$values))
  m<-length(atlasList)
  onev<-rep(1,m)
  weightmat<-matrix( rep(0, m*ncol(targetIvStruct$values) ), nrow=m )
  ct<-1
#  for ( i in atlasList )
#    {
#    imatlist[[ct]]<-antsGetNeighborhoodMatrix(i,
#      targetIMask,rad,boundary.condition="image")
#    ct<-ct+1
#    }
  natlas<-length(atlasList)
  progress <- txtProgressBar(min = 0,
    max = ncol(targetIvStruct$values), style = 3)
  for ( voxel in 1:ncol(targetIvStruct$values) )
    {
      for ( ct in 1:natlas) {
#        v<-imatlist[[ct]][,voxel]
        cent<-targetIvStruct$indices[voxel,]
        v<-antsGetNeighborhood(atlasList[[ct]],cent,rad)$values
        intmat[ct,]<-v
        if ( doscale ) v<-scale(v)
        wmat[ct,]<-(v-targetIvStruct$values[,voxel])
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
      if ( voxel %% 500 == 0 ) {
        setTxtProgressBar( progress, voxel )
      }
    }
    close( progress )
    newmeanvec<-antsrimpute(newmeanvec)
    newmeanvec[newmeanvec>max(targetI)]<-max(targetI)
    newmeanvec[newmeanvec<min(targetI)]<-min(targetI)
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
        ww<-which(segmat[,voxel]==segvals[p])
        if ( length(ww) > 0 )
          {
          probvals[p]<-sum((weightmat[ ww , voxel ]))
          }
        probImgVec[[p]][voxel]<-probvals[p]
        }
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
