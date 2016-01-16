#' multi-res voxelwise neighborhood random forest segmentation learning
#'
#' Represents multiscale feature images as a neighborhood and uses the features
#' to build a random forest segmentation model from an image population
#'
#' @param y list of training labels. either an image or numeric value
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
#' @param labelmask a mask for the features (all in the same image space)
#' @param rad vector of dimensionality d define nhood radius
#' @param multiResSchedule an integer vector defining multi-res levels
#' @param asFactors boolean - treat the y entries as factors
#' @return list a 4-list with the rf model, training vector, feature matrix
#' and the random mask
#' @author Avants BB, Tustison NJ, Pustina D
#'
#' @export mrvnrfs.predict
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
