#' multi-res voxelwise neighborhood random forest segmentation learning
#'
#' Represents multiscale feature images as a neighborhood and uses the features
#' to build a random forest segmentation model from an image population
#'
#' @param y list of training labels. either an image or numeric value
#' @param x a list of lists where each list contains feature images
#' @param labelmasks a mask (or list of masks) used to define where the 
#' samples will come from. Note, two labels (e.g., GM and WM) will double
#' the number of samples from each feature images. If the mask is binary, 
#' samples are selected randomly within 1 values.
#' @param rad vector of dimensionality d define nhood radius
#' @param nsamples (per subject to enter training)
#' @param ntrees (for the random forest model)
#' @param multiResSchedule an integer vector defining multi-res levels
#' @param asFactors boolean - treat the y entries as factors
#' @param voxchunk value of maximal voxels to predict at once. This value
#' is used to split the prediction into smaller chunks such that memory
#' requirements do not become too big
#' @return list a 4-list with the rf model, training vector, feature matrix
#' and the random mask
#' @author Avants BB, Tustison NJ, Pustina D
#'
#' @examples
#'
#' mask<-makeImage( c(10,10), 0 )
#' mask[ 3:6, 3:6 ]<-1
#' mask[ 5, 5:6]<-2
#' ilist<-list()
#' lablist<-list()
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
#'   }
#' rad<-rep( 1, 2 )
#' mr <- c(1.5,1)
#' rfm<-mrvnrfs( lablist , ilist, mask, rad=rad, multiResSchedule=mr,
#'      asFactors = (  predtype == "label" ) )
#' rfmresult<-mrvnrfs.predict( rfm$rflist,
#'      ilist, mask, rad=rad, asFactors=(  predtype == "label" ),
#'      multiResSchedule=mr )
#' if ( predtype == "scalar" )
#'   print( cor( unlist(lablist) , rfmresult$seg ) )
#' } # end predtype loop
#'
#'
#' @export mrvnrfs
mrvnrfs <- function( y, x, labelmasks, rad=NA, nsamples=1,
                     ntrees=500, multiResSchedule=c(4,2,1), asFactors=TRUE,
                     voxchunk=50000) {
  
  # check if Y is antsImage or a number
  yisimg<-TRUE
  if ( typeof(y[[1]]) == "integer" | typeof(y[[1]]) == "double") yisimg<-FALSE
  rflist<-list()
  rfct<-1
  
  # for a single labelmask create a list with it
  useFirstMask=FALSE
  if ( typeof(labelmasks) != "list" ) {
    inmask = antsImageClone( labelmasks )
    labelmasks=list()
    for ( i in 1:length(x) ) labelmasks[[i]] = inmask
    useFirstMask = TRUE
  }
  
  # loop of resolutions
  mrcount=0
  for ( mr in multiResSchedule ) {
    mrcount=mrcount+1
    message(paste(mrcount,'of',length(multiResSchedule)))
    
    invisible(gc())
    
    # add newprobs from previous run, already correct dimension
    if ( rfct > 1 ) {
      for ( kk in 1:length(x) ) {
        p1<-unlist( x[[kk]] )
        p2<-unlist(newprobs[[kk]])
        temp<-lappend(  p1 ,  p2  )
        x[[kk]]<-temp
      }
      rm(newprobs); invisible(gc())
    }
    
    invisible(gc())
    
    # build model for this mr
    if (!useFirstMask) sol<-vwnrfs( y, x, labelmasks, rad, nsamples, ntrees, asFactors, reduceFactor = mr )
    if (useFirstMask)  sol<-vwnrfs( y, x, labelmasks[[1]], rad, nsamples, ntrees, asFactors, reduceFactor = mr )
    
    sol$fm = sol$tv = sol$randmask = NULL
    
    invisible(gc())
    
    # if not last mr, predict new features for next round
    if (mrcount < length(multiResSchedule)) {
      predme = vwnrfs.predict(rfm=sol$rfm, x=x, labelmasks=labelmasks,
                          rad=rad, asFactors=TRUE, voxchunk=voxchunk,
                            reduceFactor = mr)
   
      newprobs=predme$probs
      rm(predme); invisible(gc())
      for ( tt1 in 1:length(newprobs) )
        for (tt2 in 1:length(newprobs[[tt1]]))
          newprobs[[tt1]][[tt2]]<-resampleImage( newprobs[[tt1]][[tt2]], dim(labelmasks[[tt1]]), useVoxels=1, 0 )
    }
    
    invisible(gc())
    rflist[[rfct]]<-sol$rfm
    rfct<-rfct+1
  } # mr loop
  
  return( list(rflist=rflist) )
}



#' multi-res voxelwise neighborhood random forest segmentation
#'
#' Represents multiscale feature images as a neighborhood and uses the features
#' to apply a random forest segmentation model to a new image
#'
#' @param rflist a list of random forest models from mrvnrfs
#' @param x a list of lists where each list contains feature images
#' @param labelmasks a mask (or list of masks) used to define the area to predict. 
#' This is used to save time by contstrain the prediction in within the brain.
#' @param rad vector of dimensionality d define nhood radius
#' @param multiResSchedule an integer vector defining multi-res levels
#' @param asFactors boolean - treat the y entries as factors
#' @param voxchunk value of maximal voxels to predict at once. This value
#' is used to split the prediction into smaller chunks such that memory
#' requirements do not become too big
#' @return list a 4-list with the rf model, training vector, feature matrix
#' and the random mask
#' @author Avants BB, Tustison NJ, Pustina D
#'
#' @export mrvnrfs.predict
<<<<<<< HEAD
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
#      print( paste("opt",antsrGetPointerName( x[[1]][[1]] ) ))
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
#      print( paste("opt-2",antsrGetPointerName( x[[1]][[1]] ) ))
#      print( paste("npt-1",antsrGetPointerName( xsub[[1]][[1]] ) ))
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
=======
mrvnrfs.predict <- function( rflist, x, labelmasks, rad=NA,
                             multiResSchedule=c(4,2,1), asFactors=TRUE,
                             voxchunk=60000) {
  if ( ! usePkg("randomForest") )
    stop("Please install the randomForest package, example: install.packages('randomForest')")
  
  
  # for a single labelmask create a list the same
  useFirstMask=FALSE
  if ( typeof(labelmasks) != "list" ) {
    inmask = antsImageClone( labelmasks )
    labelmasks=list()
    for ( i in 1:length(x) ) labelmasks[[i]] = inmask
    useFirstMask = TRUE
  }
  
  predtype<-'response'
  if ( asFactors ) predtype<-'prob'
  
  rfct<-1
  for ( mr in multiResSchedule ){
>>>>>>> origin/master

    if ( rfct > 1 ) {
      for ( kk in 1:length(x) ) {
        p1<-unlist( x[[kk]] )
        p2<-unlist(newprobs[[kk]])
        temp<-lappend(  p1 ,  p2  )
        x[[kk]]<-temp
      }
      rm(newprobs); invisible(gc())
    }
    
    
    predme = vwnrfs.predict(rflist[[rfct]], x=x, labelmasks=labelmasks,
                              rad=rad, asFactors=TRUE, voxchunk=voxchunk,
                              reduceFactor = mr)

    newprobs = predme$probs
    newseg = predme$seg
    if (rfct < length(multiResSchedule)) {
      for ( tt1 in 1:length(newprobs) )
        for (tt2 in 1:length(newprobs[[tt1]]))
          newprobs[[tt1]][[tt2]]<-resampleImage( newprobs[[tt1]][[tt2]], dim(labelmasks[[1]]), useVoxels=1, 0 )
    }
    
    rfct<-rfct+1
  } # mr loop
  return(list(seg=newseg, probs=newprobs))
}
