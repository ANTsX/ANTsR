#' multi-res voxelwise neighborhood random forest segmentation learning
#'
#' Represents multiscale feature images as a neighborhood and uses the features
#' to build a random forest segmentation model from an image population
#'
#' @param y list of training labels. either an image or numeric value
#' @param x a list of lists where each list contains feature images
#' @param labelmasks a list of masks where each mask defines the image space
#' for the given list. that is, the nth mask indexes the nth feature set.
#' @param rad vector of dimensionality d define nhood radius
#' @param nsamples (per subject to enter training)
#' @param ntrees (for the random forest model)
#' @param multiResSchedule an integer vector defining multi-res levels
#' @param asFactors boolean - treat the y entries as factors
#' @param voxchunk split up prediction by this number of voxels per chunk
#' @return list a 4-list with the rf model, training vector, feature matrix
#' and the random mask
#' @author Avants BB, Tustison NJ
#'
#' @examples
#'
#' mask<-makeImage( c(10,10), 0 )
#' mask[ 3:6, 3:6 ]<-1
#' mask[ 5, 5:6]<-2
#' ilist<-list()
#' lablist<-list()
#' masklist<-list()
#' inds<-1:50
#' scl<-0.33 # a noise parameter
#' for ( predtype in c("label","scalar") )
#' {
#' for ( i in inds ) {
#'   img<-antsImageClone(mask)
#'   imgb<-antsImageClone(mask)
#'   limg<-antsImageClone(mask)
#'   if ( predtype == "label") {  # 4 class prediction
#'     img[ 3:6, 3:6 ]<-rnorm(16)*scl+(i %% 4)+scl*mean(rnorm(1))
#'     imgb[ 3:6, 3:6 ]<-rnorm(16)*scl+(i %% 4)+scl*mean(rnorm(1))
#'     limg[ 3:6, 3:6 ]<-(i %% 4)+1  # the label image is constant
#'     }
#'     if ( predtype == "scalar") {
#'       img[ 3:6, 3:6 ]<-rnorm(16,1)*scl*(i)+scl*mean(rnorm(1))
#'       imgb[ 3:6, 3:6 ]<-rnorm(16,1)*scl*(i)+scl*mean(rnorm(1))
#'       limg<-i^2.0  # a real outcome
#'       }
#'     ilist[[i]]<-list(img,imgb)  # two features
#'     lablist[[i]]<-limg
#'     masklist[[i]] = mask
#'   }
#' rad<-rep( 1, 2 )
#' mr <- c(1.5,1)
#' rfm<-mrvnrfs( lablist , ilist, masklist, rad=rad, multiResSchedule=mr,
#'      asFactors = (  predtype == "label" ) )
#' rfmresult<-mrvnrfs.predict( rfm$rflist,
#'      ilist, masklist, rad=rad, asFactors=(  predtype == "label" ),
#'      multiResSchedule=mr )
#' if ( predtype == "scalar" )
#'   print( cor( unlist(lablist) , unlist(rfmresult$seg) ) )
#' } # end predtype loop
#'
#' \dontrun{
#' img = antsImageRead( getANTsRData("r16") )
#' mask = getMask( img )
#' grad = iMath(img,"Grad",1,1)
#' predimglist = list( grad )
#' featimglist = list( list( img ) )
#' masklist = list( mask )
#' mr <- c(3,2,1)
#' rad=c(2,2)
#' vwout = mrvnrfs( predimglist , featimglist, masklist,
#'   rad=rad, nsamples = 2000, multiResSchedule=mr, ntrees=1000,
#'   asFactors=FALSE )
#'
#' img = antsImageRead( getANTsRData("r64") )
#' mask = getMask( img )
#' grad = iMath(img,"Grad",1,1)
#' predimglist = list( grad )
#' featimglist = list( list( img ) )
#' masklist = list( mask )
#' rfmresult<-mrvnrfs.predict( vwout$rflist, featimglist, masklist,
#'  rad=rad, multiResSchedule=mr,
#'  asFactors=FALSE )
#' plot( iMath(img,"Grad",1,1) ) # real gradient image
#' dev.new()
#' plot( rfmresult$probs[[1]][[1]] )
#' }
#'
#' @export mrvnrfs
mrvnrfs <- function( y, x, labelmasks, rad=NA, nsamples=1,
  ntrees=500, multiResSchedule=c(4,2,1), asFactors=TRUE, voxchunk=1000 ) {
    # check y type
    yisimg<-TRUE
    useFirstMask=FALSE
    if ( typeof(labelmasks) != "list" ) {
      inmask = antsImageClone( labelmasks )
      labelmasks=list()
      for ( i in 1:length(x) ) labelmasks[[i]] = inmask
      useFirstMask = TRUE
    }
    if ( typeof(y[[1]]) == "integer" | typeof(y[[1]]) == "double") yisimg<-FALSE
    rflist<-list()
    rfct<-1
    newprobs<-list()
    for ( mr in multiResSchedule )
      {
      submasks = list()
      for ( i in 1:length(labelmasks) )
        {
        subdim<-round( dim( labelmasks[[i]] ) / mr )
        subdim[ subdim < 2*rad+1 ] <- ( 2*rad+1 )[  subdim < 2*rad+1 ]
        submask<-resampleImage( labelmasks[[i]], subdim, useVoxels=1,
          interpType=as.numeric(asFactors) )
        submasks[[i]]=submask
        }
      ysub<-y
      if ( yisimg )
      {
      for ( i in 1:(length(y)) )
        {
        subdim<-dim( submasks[[i]] )
        ysub[[i]]<-resampleImage( y[[i]], subdim, useVoxels=1,
          interpType=as.numeric(asFactors) ) # might be labels
        }
      }
      xsub<-x
      if ( rfct > 1 )
        {
        for ( kk in 1:length(xsub) )
          {
          p1<-unlist( xsub[[kk]] )
          p2<-unlist(newprobs[[kk]])
          temp<-lappend(  p1 ,  p2  )
          xsub[[kk]]<-temp
          }
        }
      for (  i in 1:length(xsub) )
        for ( j in 1:length(xsub[[i]]))
          {
          subdim<-dim( submasks[[i]] )
          xsub[[i]][[j]] =
            resampleImage( xsub[[i]][[j]], subdim, useVoxels=1,
               interpType=as.numeric(asFactors) ) # might be labels
          }
      if ( ! useFirstMask )
        sol<-vwnrfs( ysub, xsub, submasks,
          rad, nsamples, ntrees, asFactors )
      if ( useFirstMask )
        sol<-vwnrfs( ysub, xsub, submasks[[1]],
          rad, nsamples, ntrees, asFactors )
      nfeats<-length(xsub[[1]])
      predtype<-'response'
      if ( asFactors ) predtype<-'prob'
      for ( i in 1:(length(xsub)) )
        {
        splitter = round( sum(  submasks[[i]] / voxchunk ) )
        if ( splitter <= 2 ) splitter=1
        subsubmask = splitMask( submasks[[i]] , splitter )
        for ( sp in 1:splitter )
          {
          locmask = thresholdImage( subsubmask, sp, sp )
          m1<-t(getNeighborhoodInMask( xsub[[i]][[1]], locmask,
            rad, spatial.info=F, boundary.condition='image' ))
          if ( nfeats > 1 )
          for ( k in 2:nfeats )
            {
            m2<-t(getNeighborhoodInMask( xsub[[i]][[k]], locmask,
                rad, spatial.info=F, boundary.condition='image' ))
            m1<-cbind( m1, m2 )
            }
          subprobsrf<-t(
            predict( sol$rfm, newdata=m1, type=predtype ) )
          if ( sp == 1 )
            {
            probsrf = subprobsrf
            } else {
              probsrf = cbind( probsrf, subprobsrf )
            }
          }
      if ( asFactors )
        probsx<-matrixToImages(probsrf,  submasks[[i]] )
      else probsx<-list(
        makeImage( submasks[[i]], probsrf ) )
      for ( temp in 1:length(probsx) )
        {
        if ( !all( dim( probsx[[temp]] ) == dim(labelmasks[[i]]) ) )
          probsx[[temp]]<-resampleImage( probsx[[temp]],
            dim(labelmasks[[i]]), useVoxels=1, 0 )
        }
      newprobs[[i]]<-probsx
      }
    rflist[[rfct]]<-sol$rfm
    rfct<-rfct+1
    } # mr loop
    return( list(rflist=rflist, probs=newprobs, randmask=sol$randmask ) )
}



#' multi-res voxelwise neighborhood random forest segmentation
#'
#' Represents multiscale feature images as a neighborhood and uses the features
#' to apply a random forest segmentation model to a new image
#'
#' @param rflist a list of random forest models from mrvnrfs
#' @param x a list of lists where each list contains feature images
#' @param labelmasks a list of masks where each mask defines the image space
#' for the given list. that is, the nth mask indexes the nth feature set.
#' @param rad vector of dimensionality d define nhood radius
#' @param multiResSchedule an integer vector defining multi-res levels
#' @param asFactors boolean - treat the y entries as factors
#' @param voxchunk split up prediction by this number of voxels per chunk
#' @return list a 4-list with the rf model, training vector, feature matrix
#' and the random mask
#' @author Avants BB, Tustison NJ
#'
#' @export mrvnrfs.predict
mrvnrfs.predict <- function( rflist, x,
  labelmasks,
  rad=NA,
  multiResSchedule=c(4,2,1),
  asFactors=TRUE, voxchunk=1000 )
  {
    if ( typeof(labelmasks) != "list" ) {
      inmask = antsImageClone( labelmasks )
      labelmasks=list()
      for ( i in 1:length(x) ) labelmasks[[i]] = inmask
    }
    rfct<-1
    predtype<-'response'
    if ( asFactors ) predtype<-'prob'
    newprobs = list()
    for ( mr in multiResSchedule )
      {
      newsegs = list()
      submasks = list()
      for ( i in 1:length(labelmasks) )
        {
        subdim<-round( dim( labelmasks[[i]] ) / mr )
        subdim[ subdim < 2*rad+1 ] <- ( 2*rad+1 )[  subdim < 2*rad+1 ]
        submask<-resampleImage( labelmasks[[i]], subdim, useVoxels=1,
          interpType=as.numeric(asFactors) )
        submasks[[i]]=submask
        }
      xsub<-x
      if ( rfct > 1 )
        {
        for ( kk in 1:length(xsub) )
          {
          temp<-lappend(  unlist( xsub[[kk]] ) ,
            unlist(newprobs[[kk]])  )
          xsub[[kk]]<-temp
          }
        }
      nfeats<-length(xsub[[1]])
      for ( i in 1:(length(x)) )
        {
        subdim = dim( submasks[[i]] )
        xsub[[i]][[1]]<-resampleImage(
          xsub[[i]][[1]], subdim, useVoxels=1, 0 )
        splitter = round( sum(  submasks[[i]] / voxchunk ) )
        if ( splitter <= 2 ) splitter=1
        subsubmask = splitMask( submasks[[i]] , splitter )
        for ( sp in 1:splitter )
          {
          locmask = thresholdImage( subsubmask, sp, sp )
          m1<-t(getNeighborhoodInMask( xsub[[i]][[1]], locmask,
            rad, spatial.info=F, boundary.condition='image' ))
          if ( nfeats > 1 )
          for ( k in 2:nfeats )
            {
            xsub[[i]][[k]]<-resampleImage( xsub[[i]][[k]], subdim,
                useVoxels=1, 0 )
            m2<-t(getNeighborhoodInMask( xsub[[i]][[k]], locmask,
                rad, spatial.info=F, boundary.condition='image' ))
            m1<-cbind( m1, m2 )
            }
          subprobsrf<-t(
            predict( rflist[[rfct]], newdata=m1, type=predtype ) )
          if ( sp == 1 )
            {
            probsrf = subprobsrf
            } else {
              probsrf = cbind( probsrf, subprobsrf )
            }
          }
      if ( asFactors )
        probsx<-matrixToImages(probsrf,  submasks[[i]] )
      else probsx<-list(
        makeImage( submasks[[i]], probsrf ) )
      for ( temp in 1:length(probsx) )
        {
        if ( !all( dim( probsx[[temp]] ) == dim(labelmasks[[i]]) ) )
          probsx[[temp]]<-resampleImage( probsx[[temp]],
            dim(labelmasks[[i]]), useVoxels=1, 0 )
        }
      if ( asFactors )
      {
      rfseg<-imageListToMatrix( unlist(probsx) , labelmasks[[i]] )
      rfseg<-apply( rfseg, FUN=which.max, MARGIN=2 )
      newsegs[[i]] = makeImage( submasks[[i]], rfseg )
      } else newsegs[[i]] = median( probsrf )
      newprobs[[i]]<-probsx
      }
    rfct<-rfct+1
    } # mr loop
    return( list( seg=newsegs, probs=newprobs) )
}



#' split a mask into n labeled sub-masks
#'
#' @param mask antsImage mask
#' @param n number of mask chunks
#' @return relabeledMask
#' @author Avants BB, Tustison NJ
#'
#' @examples
#' mask = getMask( antsImageRead( getANTsRData("r16" ) ) )
#' smask = splitMask( mask, 10 )
#'
#' @export splitMask
splitMask <- function( mask, n )
{
# first compute chunk size
  hasvalues = mask >= 0.5
  nnz = sum( hasvalues )
  voxchunk = round( nnz / n ) - 1
  chunk.seq = seq(1, nnz, by=voxchunk )
  chunk.seq[ length(chunk.seq) ] = nnz
  smask = mask * 0
  for ( ch in 1:( length(chunk.seq)-1 ) )
    {
    # set end of this chunk
    chnxt = chunk.seq[ ch + 1 ] - 1
    if ( ch ==  ( length(chunk.seq)-1 ) ) chnxt = nnz
    # create mask for this chunk
    temp = which( hasvalues, arr.ind=T )[ chunk.seq[ch]:chnxt ]
    tnnz = hasvalues
    tnnz[ -temp ] = FALSE
    smask[ tnnz ] = ch
    }
  if ( sum( mask >= 0.5 ) != sum(smask >= 0.5 ) )
    {
    stop("submask non-zero entries should be the same as input mask" )
    }
  return( smask )
}
