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
#'   print( cor( unlist(lablist) , rfmresult$seg ) )
#' } # end predtype loop
#'
#'
#' @export mrvnrfs
mrvnrfs <- function( y, x, labelmasks, rad=NA, nsamples=1,
  ntrees=500, multiResSchedule=c(4,2,1), asFactors=TRUE ) {
    # check y type
    yisimg<-TRUE
    if ( typeof(labelmasks) != "list" ) {
      inmask = antsImageClone( labelmasks )
      labelmasks=list()
      for ( i in 1:length(x) ) labelmasks[[i]] = inmask
    }
    if ( typeof(y[[1]]) == "integer" | typeof(y[[1]]) == "double") yisimg<-FALSE
    rflist<-list()
    rfct<-1
    for ( mr in multiResSchedule )
      {
      submasks = list()
      for ( i in 1:length(labelmasks) )
        {
        subdim<-round( dim( labelmasks[[i]] ) / mr )
        subdim[ subdim < 2*rad+1 ] <- ( 2*rad+1 )[  subdim < 2*rad+1 ]
        submask<-resampleImage( labelmasks[[i]], subdim, useVoxels=1,
          interpType=1 )
        submasks[[i]]=submask
        }
      ysub<-y
      if ( yisimg )
      {
      for ( i in 1:(length(y)) )
        {
        ysub[[i]]<-resampleImage( y[[i]], subdim, useVoxels=1,
          interpType=1 ) # might be labels
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
      nfeats<-length(xsub[[1]])
      testmat<-t(getNeighborhoodInMask( submasks[[1]], submasks[[1]],
        rad, spatial.info=F, boundary.condition='image' ))
      hdsz<-nrow(testmat) # neighborhood size
      nent<-nfeats*ncol(testmat)*nrow(testmat)*length(xsub)*1.0
      fm<-matrix( nrow=(nrow(testmat)*length(xsub)) ,
        ncol=ncol(testmat)*nfeats  )
      rm( testmat )
      seqby<-seq.int( 1, hdsz*length(xsub)+1, by=hdsz )
      for ( i in 1:(length(xsub)) )
        {
        subdim = dim( submasks[[i]] )
        xsub[[i]][[1]]<-resampleImage( xsub[[i]][[1]], subdim, useVoxels=1, 0 )
        m1<-t(getNeighborhoodInMask( xsub[[i]][[1]], submasks[[i]],
          rad, spatial.info=F, boundary.condition='image' ))
        if ( nfeats > 1 )
        for ( k in 2:nfeats )
          {
          xsub[[i]][[k]]<-resampleImage( xsub[[i]][[k]], subdim,
            useVoxels=1, 0 )
          m2<-t(getNeighborhoodInMask( xsub[[i]][[k]], submasks[[i]],
              rad, spatial.info=F, boundary.condition='image' ))
          m1<-cbind( m1, m2 )
          }
        nxt<-seqby[ i + 1 ]-1
        fm[ seqby[i]:nxt, ]<-m1
        }
#    return(list(ysub=ysub,xsub=xsub,submask=submask))
    sol<-vwnrfs( ysub, xsub, submasks, rad, nsamples, ntrees, asFactors )
    predtype<-'response'
    if ( asFactors ) predtype<-'prob'
    probsrf<-t( predict( sol$rfm, newdata=fm, type=predtype ) )
    newprobs<-list()
    for ( i in 1:(length(xsub)) )
      {
      nxt<-seqby[ i + 1 ]-1
      probsx<-list(labelmasks[[i]])
      if ( asFactors )
        probsx<-matrixToImages(probsrf[,seqby[i]:nxt],  submasks[[i]] )
      else probsx<-list( makeImage( submasks[[i]], probsrf[seqby[i]:nxt] ) )
      for ( temp in 1:length(probsx) )
        {
        if ( ! all( dim( probsx[[temp]] ) == dim(labelmasks[[temp]]) ) )
          probsx[[temp]]<-resampleImage( probsx[[temp]],
            dim(labelmasks[[temp]]), useVoxels=1, 0 )
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
#' @return list a 4-list with the rf model, training vector, feature matrix
#' and the random mask
#' @author Avants BB, Tustison NJ
#'
#' @export mrvnrfs.predict
mrvnrfs.predict <- function( rflist, x,
  labelmasks,
  rad=NA,
  multiResSchedule=c(4,2,1),
  asFactors=TRUE )
  {
    if ( typeof(labelmasks) != "list" ) {
      inmask = antsImageClone( labelmasks )
      labelmasks=list()
      for ( i in 1:length(x) ) labelmasks[[i]] = inmask
    }
    rfct<-1
    for ( mr in multiResSchedule )
      {
      submasks = list()
      for ( i in 1:length(labelmasks) )
        {
        subdim<-round( dim( labelmasks[[i]] ) / mr )
        subdim[ subdim < 2*rad+1 ] <- ( 2*rad+1 )[  subdim < 2*rad+1 ]
        submask<-resampleImage( labelmasks[[i]], subdim, useVoxels=1,
          interpType=1 )
        submasks[[i]]=submask
        }
      xsub<-x
      if ( rfct > 1 )
        {
        for ( kk in 1:length(xsub) )
          {
          temp<-lappend(  unlist( xsub[[kk]] ) ,  unlist(newprobs[[kk]])  )
          xsub[[kk]]<-temp
          }
        }
      nfeats<-length(xsub[[1]])
      testmat<-t(getNeighborhoodInMask( submasks[[1]], submasks[[1]],
        rad, spatial.info=F, boundary.condition='image' ))
      hdsz<-nrow(testmat) # neighborhood size
      nent<-nfeats*nrow(testmat)*ncol(testmat)*length(x)
      fm<-matrix( nrow=(nrow(testmat)*length(x)) ,
                  ncol=ncol(testmat)*nfeats  )
      rm( testmat )
      seqby<-seq.int( 1, hdsz*length(x)+1, by=hdsz )
      for ( i in 1:(length(x)) )
        {
        subdim = dim( submasks[[i]] )
        xsub[[i]][[1]]<-resampleImage(
          xsub[[i]][[1]], subdim, useVoxels=1, 0 )
        m1<-t(getNeighborhoodInMask( xsub[[i]][[1]], submasks[[i]],
          rad, spatial.info=F, boundary.condition='image' ))
        if ( nfeats > 1 )
        for ( k in 2:nfeats )
          {
          xsub[[i]][[k]]<-resampleImage( xsub[[i]][[k]], subdim,
            useVoxels=1, 0 )
          m2<-t(getNeighborhoodInMask( xsub[[i]][[k]], submasks[[i]],
              rad, spatial.info=F, boundary.condition='image' ))
          m1<-cbind( m1, m2 )
          }
        nxt<-seqby[ i + 1 ]-1
        fm[ seqby[i]:nxt, ]<-m1
        }
    predtype<-'response'
    if ( asFactors ) predtype<-'prob'
    probs<-t( predict( rflist[[rfct]] ,newdata=fm, type=predtype) )
    newprobs<-list()
    for ( i in 1:(length(x)) )
      {
      nxt<-seqby[ i + 1 ]-1
      if ( asFactors )
        probsx<-matrixToImages(probs[,seqby[i]:nxt],  submasks[[i]] )
      else probsx<-list( makeImage( submasks[[i]], probs[seqby[i]:nxt] ) )
      if ( ! all( dim( probsx[[1]] ) == dim(labelmasks[[i]]) ) )
      for ( temp in 1:length(probsx) )
        probsx[[temp]]<-resampleImage(
          probsx[[temp]],
          dim(labelmasks[[i]]),
          useVoxels=1, 0 )
      newprobs[[i]]<-probsx
      }
    rfct<-rfct+1
    } # mr loop
    if ( asFactors )
      {
      rfseg<-imageListToMatrix( unlist(newprobs) , labelmasks[[1]] )
      rfseg<-apply( rfseg, FUN=which.max, MARGIN=2)
      rfseg<-makeImage( labelmasks[[1]] , rfseg )
      return( list( seg=rfseg, probs=newprobs ) )
      }
    rfseg<-apply( imageListToMatrix( unlist(newprobs) ,
       labelmasks[[1]] ), FUN=median, MARGIN=1 )
    return( list( seg=rfseg, probs=newprobs ) )
}
