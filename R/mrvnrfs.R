#' multi-res voxelwise neighborhood random forest segmentation learning
#'
#' Represents multiscale feature images as a neighborhood and uses the features
#' to build a random forest segmentation model from an image population
#'
#' @param y list of training label images, can be a factor or numeric vector
#' this can also be a regular old vector
#' @param x a list of lists where each list contains feature images
#' @param labelmask a mask for the features (all in the same image space)
#' the labelmask defines the number of parallel samples that will be used
#' per subject sample. two labels will double the number of predictors
#' contributed from each feature image.
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
#' mask[ 5, 5 ]<-2
#' ilist<-list()
#' lablist<-list()
#' inds<-1:50
#' scl<-0.33 # a noise parameter
#' for ( i in inds ) {
#'   img<-antsImageClone(mask)
#'   imgb<-antsImageClone(mask)
#'   img[ 3:6, 3:6 ]<-rnorm(16)*scl+(i %% 2)+scl*mean(rnorm(1))
#'   imgb[ 3:6, 3:6 ]<-rnorm(16)*scl+(i %% 2)+scl*mean(rnorm(1))
#'   limg<-antsImageClone(mask)
#'   limg[ 3:6, 3:6 ]<-(i %% 2)  # the label image is constant
#'   ilist[[i]]<-list(img,imgb)  # two features
#'   lablist[[i]]<-limg
#' }
#' rfm<-mrvnrfs( lablist , ilist, mask, rad=c(0,0), multiResSchedule=1 )
#' rfmresult<-mrvnrfs.predict( rfm$rflist ,
#'    list(ilist[[2]]), mask, rad=c(0,0),
#'    multiResSchedule=1 )
#'
#' @export mrvnrfs
mrvnrfs <- function( y, x, labelmask, rad=NA, nsamples=1,
  ntrees=500, multiResSchedule=c(4,2,1), asFactors=TRUE ) {
    # check y type
    yisimg<-TRUE
    if (  typeof(y) == "integer" | typeof(y) == "double" ) yisimg<-FALSE
    rflist<-list()
    rfct<-1
    for ( mr in multiResSchedule )
      {
      subdim<-round( dim( labelmask ) / mr )
      submask<-resampleImage( labelmask, subdim, useVoxels=1,
        interpType=1 )
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
      if ( rfct > 1 & asFactors )
        {
        for ( kk in 1:length(xsub) )
          {
          temp<-lappend(  unlist( xsub[[kk]] ) ,  unlist(newprobs[[kk]])  )
          xsub[[kk]]<-temp
          }
        }
      nfeats<-length(xsub[[1]])
      testmat<-t(antsGetNeighborhoodMatrix( submask, submask,
        rad, spatial.info=F, boundary.condition='image' ))
      hdsz<-nrow(testmat) # neighborhood size
      nent<-nfeats*nrow(testmat)*ncol(testmat)*length(x)
      fm<-matrix( rep(NA, nent ), nrow=(nrow(testmat)*length(x)) )
      seqby<-seq.int( 1, hdsz*length(x)+1, by=hdsz )
      for ( i in 1:(length(x)) )
        {
        xsub[[i]][[1]]<-resampleImage( xsub[[i]][[1]], subdim, useVoxels=1, 0 )
        m1<-t(antsGetNeighborhoodMatrix( xsub[[i]][[1]], submask,
          rad, spatial.info=F, boundary.condition='image' ))
        if ( nfeats > 1 )
        for ( k in 2:nfeats )
          {
          xsub[[i]][[k]]<-resampleImage( xsub[[i]][[k]], subdim,
            useVoxels=1, 0 )
          m2<-t(antsGetNeighborhoodMatrix( xsub[[i]][[k]], submask,
              rad, spatial.info=F, boundary.condition='image' ))
          m1<-cbind( m1, m2 )
          }
        nxt<-seqby[ i + 1 ]-1
        fm[ seqby[i]:nxt, ]<-m1
        }
#    return(list(ysub=ysub,xsub=xsub,submask=submask))
    sol<-vwnrfs( ysub, xsub, submask, rad, nsamples, ntrees, asFactors )
#    print("runrfdone")
    if ( asFactors )
    {
    probs<-t( predict(sol$rfm,newdata=fm,type='prob') )
    newprobs<-list()
    for ( i in 1:(length(x)) )
      {
      nxt<-seqby[ i + 1 ]-1
      probsx<-matrixToImages(probs[,seqby[i]:nxt],  submask )
      for ( temp in 1:length(probsx) )
        probsx[[temp]]<-resampleImage( probsx[[temp]], dim(labelmask),
          useVoxels=1, 0 )
      newprobs[[i]]<-probsx
      }
    } else {
      newprobs<-predict(sol$rfm,newdata=fm)
    }
    rflist[[rfct]]<-sol$rfm
    rfct<-rfct+1
    } # mr loop
    return( list(rflist=rflist, probs=newprobs ) )
}



#' multi-res voxelwise neighborhood random forest segmentation
#'
#' Represents multiscale feature images as a neighborhood and uses the features
#' to apply a random forest segmentation model to a new image
#'
#' @param rflist a list of random forest models from mrvnrfs
#' @param x a list of lists where each list contains feature images
#' @param labelmask a mask for the features (all in the same image space)
#' @param rad vector of dimensionality d define nhood radius
#' @param multiResSchedule an integer vector defining multi-res levels
#' @param asFactors boolean - treat the y entries as factors
#' @return list a 4-list with the rf model, training vector, feature matrix
#' and the random mask
#' @author Avants BB, Tustison NJ
#'
#' @export mrvnrfs.predict
mrvnrfs.predict <- function( rflist, x, labelmask, rad=NA,
  multiResSchedule=c(4,2,1), asFactors=TRUE ) {
    rfct<-1
    for ( mr in multiResSchedule )
      {
      subdim<-round( dim( labelmask ) / mr )
      submask<-resampleImage( labelmask, subdim, useVoxels=1,
        interpType=1 )
      xsub<-x
      if ( rfct > 1 & asFactors )
        {
        for ( kk in 1:length(xsub) )
          {
          temp<-lappend(  unlist( xsub[[kk]] ) ,  unlist(newprobs[[kk]])  )
          xsub[[kk]]<-temp
          }
        }
      nfeats<-length(xsub[[1]])
      testmat<-t(antsGetNeighborhoodMatrix( submask, submask,
        rad, spatial.info=F, boundary.condition='image' ))
      hdsz<-nrow(testmat) # neighborhood size
      nent<-nfeats*nrow(testmat)*ncol(testmat)*length(x)
      fm<-matrix( rep(NA, nent ), nrow=(nrow(testmat)*length(x)) )
      seqby<-seq.int( 1, hdsz*length(x)+1, by=hdsz )
      for ( i in 1:(length(x)) )
        {
        xsub[[i]][[1]]<-resampleImage( xsub[[i]][[1]], subdim, useVoxels=1, 0 )
        m1<-t(antsGetNeighborhoodMatrix( xsub[[i]][[1]], submask,
          rad, spatial.info=F, boundary.condition='image' ))
        if ( nfeats > 1 )
        for ( k in 2:nfeats )
          {
          xsub[[i]][[k]]<-resampleImage( xsub[[i]][[k]], subdim,
            useVoxels=1, 0 )
          m2<-t(antsGetNeighborhoodMatrix( xsub[[i]][[k]], submask,
              rad, spatial.info=F, boundary.condition='image' ))
          m1<-cbind( m1, m2 )
          }
        nxt<-seqby[ i + 1 ]-1
        fm[ seqby[i]:nxt, ]<-m1
        }
    if ( asFactors )
    {
    probs<-t( predict( rflist[[rfct]] ,newdata=fm,type='prob') )
    newprobs<-list()
    for ( i in 1:(length(x)) )
      {
      nxt<-seqby[ i + 1 ]-1
      probsx<-matrixToImages(probs[,seqby[i]:nxt],  submask )
      for ( temp in 1:length(probsx) )
        probsx[[temp]]<-resampleImage( probsx[[temp]], dim(labelmask),
          useVoxels=1, 0 )
      newprobs[[i]]<-probsx
      }
    } else {
      newprobs<-predict( rflist[[rfct]] ,newdata=fm)
    }
    rfct<-rfct+1
    } # mr loop
    rfseg<-imageListToMatrix( unlist(newprobs) , labelmask )
    rfseg<-apply( rfseg, FUN=which.max, MARGIN=2)
    rfseg<-makeImage( labelmask , rfseg )
    return( list( seg=rfseg, probs=newprobs ) )
}
