#' voxelwise neighborhood random forest segmentation and prediction
#'
#' Represents feature images as a neighborhood and uses the features
#' to build a random forest prediction from an image population
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
#' rfm<-vwnrfs( lablist , ilist, mask, rad=c(0,0) )
#' sum(abs(as.numeric(rfm$tv)-as.numeric(predict(rfm$rfm))))
#' rfm<-vwnrfs( lablist , ilist, mask, rad=c(2,2) )
#' sum(abs(as.numeric(rfm$tv)-as.numeric(predict(rfm$rfm))))
#'
#' @export vwnrfs
vwnrfs <- function( y, x, labelmask, rad=NA, nsamples=1,
  ntrees=500, asFactors=TRUE ) {
  if ( all( is.na( rad )  ) ) {
    rad<-rep(0, x[[1]][[1]]@dimension )
  }
  idim<-length(rad)
  if ( idim != x[[1]][[1]]@dimension )
    stop("vwnrfs: dimensionality does not match")
  # first thing - find unique labels
  ulabs<-sort(unique(c(as.numeric(labelmask))))
#  if ( min(ulabs) == 0 ) ulabs<-ulabs[-1] # background
  # second thing - create samples for each unique label
  randmask<-antsImageClone( labelmask )*0
  for ( ulab in ulabs )
    {
    ulabvec<-( labelmask == as.numeric( ulab ) )
    randvec<-rep( FALSE, length( ulabvec ) )
    k<-min( c( nsamples, sum(ulabvec == TRUE) ) )
    n<-sum( ulabvec == TRUE )
    randvec[ ulabvec == TRUE ][ sample(1:n)[1:k] ]<-TRUE
    randmask[ randvec ]<-ulab
    }
  # third thing - at each sample, find the training label/value
  # and the features at that location - go subject by subject
  # 3.1 first define the training vector
  rmsz<-sum( randmask > 0 ) # entries in mask
  tv<-rep(NA, length(y)*rmsz )
  seqby<-seq.int( 1, length(tv)+1, by=rmsz )
  # check y type
  yisimg<-TRUE
  if (  typeof(y) == "integer" | typeof(y) == "double" ) yisimg<-FALSE
  for ( i in 1:(length(y)) )
    {
    nxt<-seqby[ i + 1 ]-1
#    tv[ seqby[i]:nxt ]<-y[[i]][ randmask > 0 ]
    if ( yisimg )
      tv[ seqby[i]:nxt ]<-y[[i]][ randmask > 0 ]
#      tv[ seqby[i]:nxt ]<-t( antsGetNeighborhoodMatrix(
#        y[[i]], randmask, rep(0,idim), spatial.info=F,
#        boundary.condition='image' ) )
    else tv[ seqby[i]:nxt ]<-rep( y[i], rmsz )
    }
  if ( asFactors ) tv<-factor( tv )
  # 3.2 next define the feature matrix
  testmat<-t(antsGetNeighborhoodMatrix( x[[1]][[1]], randmask,
    rad, spatial.info=F, boundary.condition='image' ))
  hdsz<-ncol(testmat) # neighborhood size
  nfeats<-length(x[[1]])
  fm<-matrix( rep(NA, length(tv)*nfeats*hdsz ), nrow=length(tv)  )
  for ( i in 1:(length(y)) )
    {
    m1<-t(antsGetNeighborhoodMatrix( x[[i]][[1]], randmask,
      rad, spatial.info=F, boundary.condition='image' ))
    for ( k in 2:nfeats )
      {
      m2<-t(antsGetNeighborhoodMatrix( x[[i]][[k]], randmask,
          rad, spatial.info=F, boundary.condition='image' ))
      m1<-cbind( m1, m2 )
      }
    nxt<-seqby[ i + 1 ]-1
    fm[ seqby[i]:nxt, ]<-m1
    }
  if ( usePkg("randomForest") )
    {
    rfm<-randomForest(y=tv,x=fm,ntrees=ntrees)
    return( list(rfm=rfm, tv=tv, fm=fm, randmask=randmask ) )
    }
  else
    {
    return("install the randomForest package")
    }
}
