#' joint intensity fusion
#'
#' Estimates an image from another set of images - intensity generalization of joint label fusion.  Search radius used only when employing labels - WIP to speed it up.
#'
#' @param targetI antsImage to be approximated
#' @param targetIMask mask with value 1
#' @param atlasList list containing antsImages
#' @param beta weight sharpness, default to 2
#' @param rad neighborhood radius, default to 3
#' @param labelList list containing antsImages
#' @param doscale scale neighborhood intensities
#' @param doNormalize normalize each image range to 0, 1
#' @param maxAtlasAtVoxel min/max n atlases to use at each voxel
#' @param rho ridge penalty increases robustness to outliers but also
#'   makes image converge to average
#' @param useSaferComputation slower but more error checking
#' @param usecor employ correlation as local similarity
#' @param rSearch radius of search, default is 2
#' @param boundary.condition one of 'image' 'mean' 'NA'
#' @param segvals list of labels to expect
#' @param computeProbs boolean - requires more memory
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
#' d<-2
#' pp<-jointIntensityFusion(ref,refmask,ilist, rSearch=0,
#'   labelList=seglist, rad=rep(r,d) )
#'
#' @export jointIntensityFusion
jointIntensityFusion <- function( targetI, targetIMask, atlasList,
  beta=4, rad=NA, labelList=NA, doscale = TRUE,
  doNormalize=TRUE, maxAtlasAtVoxel=c(1,Inf), rho=0.01, # debug=F,
  useSaferComputation=FALSE, usecor=FALSE, boundary.condition='mean',
  rSearch=2, segvals=NA, computeProbs=FALSE )
{
  haveLabels=FALSE
  BC=boundary.condition
  nvox = sum( targetIMask == 1 )
  if ( length(labelList) != length(atlasList) )
    stop("length(labelList) != length(atlasList)")
  includezero = TRUE
  jifImage = targetI * 0
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
    posteriorList = lappend( posteriorList , targetI * 0 )
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
  weightmat<-matrix( rep(0, m*ncol(targetIv) ), nrow=m )
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
    segmat<-matrix( 0, ncol=length(targetint)  , nrow=natlas )
    intmat<-matrix( 0, ncol=length(targetint)  , nrow=natlas )
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
      intmat[ct, ] = v
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
        weightmat[,voxel]<-wts
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
        pwts = wts
        pwts[ wts < 0 ] = 0
        pwts = pwts / sum( pwts )
        lintensity = pwts %*% intmat
        .Call("addNeighborhoodToImage",
            jifImage, cent, rad, lintensity,
            package="ANTsR" )
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
  return( list( segimg=rsegimg, predimg=jifImage,
    localWeights=weightmat, probimgs=posteriorList,
    badct=badct, segvals=segvals  ) )
}

.jointIntensityFusionOld <- function( targetI, targetIMask, atlasList,
  beta=4, rad=NA, labelList=NA, doscale = TRUE,
  doNormalize=TRUE, maxAtlasAtVoxel=c(1,Inf), rho=0.01, # debug=F,
  useSaferComputation=FALSE, usecor=FALSE, boundary.condition='mean',
  rSearch=2, segvals=NA, includezero=TRUE, computeProbs=FALSE )
{
  haveLabels=FALSE
  BC=boundary.condition
  nvox = sum( targetIMask == 1 )
  if ( !( all( is.na(labelList) ) ) )
    {
    segmat<-imageListToMatrix( labelList, targetIMask )
    segmatSearch<-segmat
    haveLabels=TRUE
    if ( all( is.na(segvals) ) )
      {
      segvals<-c(sort( unique( as.numeric(segmat)) ))
      if ( includezero )
        if ( ! ( 0 %in% segvals ) ) segvals<-c(0,segvals)
      if ( !includezero )
        segvals<-segvals[  segvals != 0 ]
      }
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
  intmat<-wmat
  targetIvStruct<-getNeighborhoodInMask(targetI,
    targetIMask,rad,boundary.condition=BC,spatial.info=T)
  targetIv<-( targetIvStruct$values )
  if ( doscale ) targetIv<-scale( targetIvStruct$values )
  indices<-targetIvStruct$indices
  rm(targetIvStruct)
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
  badct<-0
  basewmat<-t(replicate(length(atlasList), rep(0.0,n) ) )
  for ( voxel in 1:ncol(targetIv) )
    {
    zsd<-rep(1,natlas)
    wmat<-basewmat
    targetint<-targetIv[,voxel]
    cent<-indices[voxel,]
    for ( ct in 1:natlas)
      {
      nhsearch = .Call("jointLabelFusionNeighborhoodSearch",
        targetint, cent, max(rad), rSearch,
        atlasList[[ct]],
        labelList[[ct]],
        PACKAGE="ANTsR" )
      segval = nhsearch[[ 1 ]]
      v = nhsearch[[ 2 ]]
      vmean = nhsearch[[ 3 ]]
      sdv = nhsearch[[ 4 ]]
      segmatSearch[ct,voxel]<-segval
      intmat[ct,] = v
      if ( sdv == 0 ) {
        zsd[ct]<-0 # assignment
        sdv<-1
        }
      if ( doscale ) {
        v<-( v - vmean ) / sdv
        }
      if ( !usecor )
        wmat[ct,]<-abs(v-targetint) # assignment
      else {
        ip<- ( v * targetint )
        wmat[ct,]<-( ip * (-1.0) )  # assignment
        }
      } # for all atlases
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
      } else badct<-badct+1
      if ( FALSE ) {
        print("DEBUG MODE")
            return(
                list(voxel=voxel,
                     wts=wts,intmat=intmat,
                     wmat=wmat,cormat=cormat, pvox=newmeanvec[voxel],
                     zsd=zsd,intmatc=intmat[zsd==1,matcenter],
                     segmat=segmatSearch,
                     targetint=targetint)
                )
              }
      if ( voxel %% 500 == 0 ) {
            setTxtProgressBar( progress, voxel )
        }
    }
  } # loop over voxels
  close( progress )
  rm( segmat )
  rm( targetIv )
  rm( indices )
  newimg<-makeImage( targetIMask, newmeanvec )
  maxSimImg<-makeImage( targetIMask, maxSimImg )
  segimg<-NA
  probImgList<-NA
  if ( !( all( is.na(labelList) ) ) )
    {
    segvec<-rep( 0, nvox )
    if ( computeProbs ) {
      probImgList<-list()
      probImgVec<-list()
      for ( p in 1:length( segvals ) )
        probImgVec[[p]]<-rep( 0, nvox )
      }
    for ( voxel in 1:nvox )
      {
      probvals<-rep(0,length(segvals))
      segsearch<-segmatSearch[,voxel]
      if  ( sd(segsearch) > 0 )
      {
        for ( p in 1:length(segvals))
        {
        ww<-which( segsearch==segvals[p] &  weightmat[  , voxel ] > 0 )
          if ( length(ww) > 0 )
            {
            probvals[p]<-sum(weightmat[ ww , voxel ],na.rm=T)
            }
        }
      probvals<-probvals/sum(probvals)
      } else {
        probvals[ which(segsearch[1]==segvals) ]<-1
      }
      if ( computeProbs )
        for ( p in 1:length(segvals))
          probImgVec[[p]][voxel]<-probvals[p]
      k<-which(probvals==max(probvals,na.rm=T))
      if ( length(k) > 0 )
        segvec[voxel]=segvals[ k ][1]
    }
    if ( computeProbs )
      for ( p in 1:length(segvals) )
        probImgList[[p]]<-makeImage( targetIMask, probImgVec[[p]] )
    segimg<-makeImage(targetIMask,segvec)
    # 1st probability is background i.e. 0 label
    if ( computeProbs )
      probImgList<-probImgList[2:length(probImgList)]
    }
  return( list( predimg=newimg, segimg=segimg,
    localWeights=weightmat, probimgs=probImgList,
    maxSimImg=maxSimImg, badct=badct  ) )
}
