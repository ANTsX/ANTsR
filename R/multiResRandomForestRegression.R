#' multiple resolution neighborhood random forest regression
#'
#' Represents feature images as a neighborhood across scales
#' to build a random forest prediction from an image population. A use case
#' for this function is to predict cognition from multiple image features, e.g.
#' from the voxelwise FA of the corpus callosum and, in parallel, voxelwise
#' measurements of the volume of the inferior frontal gyrus.
#'
#' @param y vector of scalar values or labels.  if a factor, do classification,
#' otherwise regression.
#' @param x a list of lists where each list contains feature images
#' @param labelmasks a list of masks where each mask defines the image space
#' for the given list and the number of parallel predictors.  more labels means
#' more predictors.  alternatively, separate masks may be used for each feature
#' in which case this should be a list of lists.  see examples.
#' @param rad vector of dimensionality d define nhood radius
#' @param nsamples (per subject to enter training)
#' @param multiResSchedule an integer vector defining multi-res levels
#' @param ntrees (for the random forest model)
#' @return list with a random forest model, a vector identifying which rows
#' correspond to which subjects and a prediction vector.
#' @author Avants BB, Tustison NJ
#'
#' @references Pustina, D, et al.  Automated segmentation of chronic stroke
#' lesions using LINDA: Lesion Identification with Neighborhood Data Analysis,
#' Human Brain Mapping, 2016. (related work, not identical)
#'
#' @seealso \code{\link{getMultiResFeatureMatrix}} \code{\link{mrvnrfs}}
#' @examples
#'
#' mask<-makeImage( c(100,100), 0 )
#' mask[ 30:60, 30:60 ]<-1
#' mask[ 35:45, 50:60]<-2
#' ilist<-list()
#' masklist<-list()
#' inds<-1:8
#' yvec<-rep(0,length(inds))
#' scl<-0.33 # a noise parameter
#' for ( i in inds ) {
#'   img<-antsImageClone(mask)
#'   imgb<-antsImageClone(mask)
#'   limg<-antsImageClone(mask)
#'   img[ 3:6, 3:6 ]<-rnorm(16,1)*scl*(i)+scl*mean(rnorm(1))
#'   imgb[ 3:6, 3:6 ]<-rnorm(16,1)*scl*(i)+scl*mean(rnorm(1))
#'   ilist[[i]]<-list(img,imgb)  # two features
#'   yvec[i]<-i^2.0  # a real outcome
#'   masklist[[i]] = antsImageClone( mask )
#'   }
#' r=c(1,1)
#' mr=c(2,0)
#' featMat <- getMultiResFeatureMatrix( ilist[[1]], masklist[[1]],
#'   rad=r, , multiResSchedule=mr )
#' rfm <- multiResRandomForestRegression(
#'   yvec , ilist, masklist, rad=r, multiResSchedule=mr )
#' preds = predict( rfm, newdata=featMat )
#' \dontrun{
#' # data: https://github.com/stnava/ANTsTutorial/tree/master/phantomData
#' fns = Sys.glob("phantom*wmgm.jpg")
#' ilist = imageFileNames2ImageList( fns )
#' masklist = list( )
#' flist = list( )
#' for ( i in 1:length(fns) )
#'   {
#' # 2 labels means 2 sets of side by side predictors and features at each scale
#'   locseg = kmeansSegmentation( ilist[[i]], 2 )$segmentation
#'   masklist[[ i ]] = list( locseg, locseg %>% thresholdImage(2,2), locseg )
#'   flist[[ i ]] = list( ilist[[i]], ilist[[i]] %>% iMath("Laplacian",1),
#'     ilist[[i]] %>% iMath("Grad",1)  )
#'   }
#' yvec = factor( rep( c(1,2), each = 4 ) ) # classification
#' r = c( 1, 1 )
#' mr = c( 2, 1, 0 )
#' ns = 50
#' trn = c(1:3,6:8)
#' ytrain = yvec[ trn ]
#' ftrain = flist[ trn ]
#' mtrain = masklist[ trn ]
#' mrrfr = multiResRandomForestRegression( ytrain, ftrain, mtrain, rad=c(1,1),
#'   nsamples = ns, multiResSchedule=mr )
#' mypreds = rep( NA, length( fns ) )
#' mymode <- function(x) {
#'  ux <- unique(x)
#'  ux[which.max(tabulate(match(x, ux)))]
#' }
#' for ( i in 4:5 ) # test set
#'   {
#'   fmat = getMultiResFeatureMatrix( flist[[i]], masklist[[i]],
#'          rad=r,  multiResSchedule=mr, nsamples = ns )
#'   myp = predict( mrrfr, newdata=fmat )
#'   mypreds[ i ] = mymode( myp ) # get the most frequent observation
#'   # use median or mean for continuous predictions
#'   }
#' print("predicted")
#' print( mypreds[-trn] )
#' print("ground truth")
#' print( yvec[-trn] )
#' }
#' @export multiResRandomForestRegression
multiResRandomForestRegression <- function(
  y,
  x,
  labelmasks,
  rad=NA,
  nsamples=10,
  multiResSchedule=c(0),
  ntrees=500 ) {
  if ( all( is.na( rad )  ) ) {
    rad<-rep(0, x[[1]][[1]]@dimension )
  }
  idim<-length(rad)
  if ( idim != x[[1]][[1]]@dimension )
    stop("multiResRandomForestRegression: dimensionality does not match")
  fm = getMultiResFeatureMatrix( x[[1]], labelmasks[[1]],
    rad=rad, multiResSchedule = multiResSchedule, nsamples=nsamples )
  if ( length( labelmasks ) > 1 )
    {
    for ( kk in 2:length( labelmasks ) )
      {
      temp = getMultiResFeatureMatrix( x[[kk]], labelmasks[[kk]],
        rad=rad, multiResSchedule = multiResSchedule, nsamples=nsamples )
      fm = rbind( fm, temp )
      }
    }
  # getMultiResFeatureMatrix
  if ( usePkg("randomForest") )
    {
    yExt = rep( y, each=nsamples )
    rfm <- randomForest::randomForest(y=yExt, x=fm, ntree = ntrees,
      importance = FALSE, proximity = FALSE, keep.inbag = FALSE,
      keep.forest = TRUE , na.action = na.omit, norm.votes=FALSE )
    # need the forest for prediction
    return( rfm )
    }
  else
    {
    stop("install the randomForest package")
    }
}



#' build multiple resolution predictor matrix from feature images
#'
#' Represents feature images as a neighborhood across scales. each subject
#' gets a label image and feature list.  these labels/features should be
#' the same type for all subjects.  e.g each subject has a k-label image
#' where the labels cover the same anatomy and the feature images are the same.
#' for each label in a mask, produce a multi-resolution neighborhood
#' sampling from the data within the label, for a given feature. do this for
#' each feature. so, 2 labels will double the width of the predictor matrix.
#' an additional scale parameter or feature will increase the width.  more
#' samples will increase the number of rows in the predictor matrix. the labels
#' allow one to collect predictors side-by-side from different parts of an
#' image-based feature set.  future work will allow other covariates.
#'
#' @param x a list of feature images
#' @param labelmask the mask defines the image space for the associated feature
#' and increases the number of predictors.  more labels means more
#' predictors.  a different mask may be used for each feature, if desired, in
#' which case this should be a list of masks where the length of the list
#' is equal to the number of features.
#' @param rad vector of dimensionality d define nhood radius
#' @param multiResSchedule a vector of smoothing values
#' @param nsamples defines the number of samples to take for each label.
#' @return mat a matrix of predictors with n samples rows
#' @author Avants BB, Tustison NJ
#'
#' @examples
#'
#' img <- antsImageRead( getANTsRData("r16"))
#' seg <- kmeansSegmentation( img, 3 )$segmentation
#' flist = list( img, img %>% iMath("Grad") )
#' featMat <- getMultiResFeatureMatrix( flist, seg, rad=c(1,1),
#'   multiResSchedule = c( 2, 1), nsamples=10 )
#' @export getMultiResFeatureMatrix
getMultiResFeatureMatrix <- function(
  x,
  labelmask,
  rad=NA,
  multiResSchedule=c(0),
  nsamples=10
  )
  {
  # loop over features, labels and multires
  for ( featk in 1:length(x) )
    {
    if ( class( labelmask )[1] == 'antsImage' )
      locmask = ( labelmask )
    if ( class( labelmask )[1] == 'list' )
      locmask = labelmask[[ featk ]]
    ulabs = sort( unique( locmask[ locmask >  0  ] ) )
    for ( lab in ulabs )
      {
      randmask = locmask * 0
      ulabvec<-( locmask == as.numeric( lab ) )
      randvec<-rep( FALSE, length( ulabvec ) )
      k<-min( c( nsamples, sum(ulabvec == TRUE) ) )
      n<-sum( ulabvec == TRUE )
      randvec[ ulabvec == TRUE ][ sample(1:n)[1:k] ]<-TRUE
      randmask[ randvec ]<-1
      for ( smlev in multiResSchedule )
        {
        if ( ! exists("testmat") )
          {
          testmat<-t( getNeighborhoodInMask(
            image=x[[featk]] %>% smoothImage(smlev), mask=randmask,
              radius=rad, spatial.info=F, boundary.condition='image' ) )
          }
        else
          {
          testmat <- cbind( testmat,
            t( getNeighborhoodInMask(
              image=x[[featk]] %>% smoothImage(smlev), mask=randmask,
                radius=rad, spatial.info=F, boundary.condition='image' ) )
            )
          }
        } # multires
      } # labels
    } # features
  return( testmat )
}
