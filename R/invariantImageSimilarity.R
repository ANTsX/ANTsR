#' similarity metrics between two images as a function of geometry
#'
#' compute similarity metric between two images as image is rotated about its
#' center w/or w/o optimization
#'
#' @param in_image1 reference image
#' @param in_image2 moving image
#' @param localSearchIterations integer controlling local search in multistart
#' @param metric which metric MI or GC (string)
#' @param thetas numeric vector of search angles in degrees
#' @param thetas2 numeric vector of search angles in degrees around principal axis 2 (3D)
#' @param thetas3 numeric vector of search angles in degrees around principal axis 3 (3D)
#' @param scaleImage global scale
#' @param doReflection reflect image about principal axis
#' @param txfn if present, write optimal tx to .mat file
#' @param transform either Affine or Similarity transform (rigid possible in future)
#' @return dataframe with metric values and transformation parameters
#' @author Brian B. Avants
#' @keywords image similarity
#' @examples
#
#' fi<-antsImageRead( getANTsRData("r16") )
#' mi<-antsImageRead( getANTsRData("r64") )
#' mival<-invariantImageSimilarity( fi, mi, thetas = c(0,10,20) )
#' mapped = antsApplyTransforms( fi, mi, transformlist=mival[[2]] )
#' areg = antsRegistration( fi, mi, typeofTransform="Affine",
#'   initialTransform=mival[[2]] )
#' bestInd = which.min( mival[[1]]$MetricValue )
#' txparams = as.numeric( mival[[1]][ bestInd,2:(ncol( mival[[1]] )-2) ] )
#' txfixedparams = as.numeric( mival[[1]][ bestInd,(ncol( mival[[1]] )-2+1):ncol( mival[[1]] )] )
#' affTx = createAntsrTransform( type = "AffineTransform", dimension = 2,
#'   parameters = txparams, fixed.parameters = txfixedparams )
#' mapped2 = applyAntsrTransformToImage( affTx, mi, fi )
#'
#' scaleMat = diag( 2 ) * 0.75
#' affTx = createAntsrTransform( type = "AffineTransform", dimension = 2,
#'      matrix = scaleMat, fixed.parameters = c(125.2706, 129.2100) )
#' temp = applyAntsrTransformToImage( affTx, mi, mi )
#' mival<-invariantImageSimilarity( fi, temp, thetas = c(0,10,20),
#'   localSearchIterations = 10, transform='Similarity'  )
#' mapped = antsApplyTransforms( fi, temp, transformlist=mival[[2]] )
#' mival<-invariantImageSimilarity( fi, temp, thetas = c(0,10,20),
#'   localSearchIterations = 10, transform='Affine'  )
#' mapped2 = antsApplyTransforms( fi, temp, transformlist=mival[[2]] )
#' print( cor( fi[ fi > 0 ], temp[fi>0] ))
#' print( cor( fi[ fi > 0 ], mapped[fi>0] ))
#' print( cor( fi[ fi > 0 ], mapped2[fi>0] ))
#' 
#' @export invariantImageSimilarity
invariantImageSimilarity <- function(
  in_image1,
  in_image2,
  localSearchIterations = 0,
  metric = "MI",
  thetas  = seq( from = 0, to = 360, length.out = 5 ),
  thetas2 = seq( from = 0, to = 360, length.out = 5 ),
  thetas3 = seq( from = 0, to = 360, length.out = 5 ),
  scaleImage = 1,
  doReflection = 0,
  txfn = NA,
  transform = c("Affine", "Similarity") ) {
  if (length(dim(in_image1)) == 1)
    if (dim(in_image1)[1] == 1)
      return(NULL)
  if (in_image1@pixeltype != "float" | in_image2@pixeltype != "float") {
    print(args(invariantImageSimilarity))
    print("input images must have float pixeltype")
    return(NA)
  }
  transform = match.arg( transform )
  if ( transform == "Affine" ) transform = 0
  if ( transform == "Similarity" ) transform = 1
  if ( is.na( txfn ) )
    txfn = tempfile( fileext = ".mat" )
  # convert to radians
  thetain <- thetas
  thetain <- (thetas * pi)/180   # convert to radians
  thetain2 <- (thetas2 * pi)/180
  thetain3 <- (thetas3 * pi)/180
  in_image1 = iMath(in_image1, "Normalize")
  in_image2 = iMath(in_image2, "Normalize")
  if (class(localSearchIterations) != "numeric") {
    print("wrong input: localSearchIterations is not numeric")
    return(NA)
  }
  if (class(metric) != "character") {
    print("wrong input: metric is not numeric")
    return(NA)
  }
  idim = in_image1@dimension
  fpname = paste("FixedParam",1:idim,sep='')
  if (doReflection == 0) {
    r1 <- .Call("invariantImageSimilarity", in_image1, in_image2,
      thetain, thetain2, thetain3, localSearchIterations,
      metric, scaleImage, doReflection, txfn, transform, PACKAGE = "ANTsR")
    pnames = paste("Param", 1:( ncol( r1 ) - 1 ), sep='' )
    pnames[ ( length(pnames)-idim+1 ):length(pnames) ] = fpname
    colnames( r1 ) = c( "MetricValue", pnames )
    return( list( data.frame( r1 ), txfn ) )
  }
  txfn1 <- tempfile(fileext = ".mat")
  txfn2 <- tempfile(fileext = ".mat")
  txfn3 <- tempfile(fileext = ".mat")
  txfn4 <- tempfile(fileext = ".mat")
  r1 <- .Call("invariantImageSimilarity", in_image1, in_image2,
    thetain, thetain2, thetain3, localSearchIterations,
    metric, scaleImage, 0, txfn1, transform, PACKAGE = "ANTsR")
  pnames = paste("Param", 1:( ncol( r1 ) - 1 ), sep='' )
  pnames[ ( length(pnames)-idim+1 ):length(pnames) ] = fpname
  colnames( r1 ) = c( "MetricValue", pnames )
  r2 <- .Call("invariantImageSimilarity", in_image1, in_image2,
    thetain, thetain2, thetain3, localSearchIterations,
    metric, scaleImage, 1, txfn2, transform, PACKAGE = "ANTsR")
  colnames( r2 ) = c( "MetricValue", pnames )
  r3 <- .Call("invariantImageSimilarity", in_image1, in_image2,
    thetain, thetain2, thetain3, localSearchIterations,
    metric, scaleImage, 2, txfn3, transform, PACKAGE = "ANTsR")
  colnames( r3 ) = c( "MetricValue", pnames )
  r4 <- .Call("invariantImageSimilarity", in_image1, in_image2,
    thetain, thetain2, thetain3, localSearchIterations,
    metric, scaleImage, 3, txfn4, transform, PACKAGE = "ANTsR")
  colnames( r4 ) = c( "MetricValue", pnames )
  ww <- which.min(c(min(r1[,1]), min(r2[,1]), min(r3[,1]), min(r4[,1])))
  if (ww == 1) {
    return(list( data.frame( r1 ), txfn1))
  }
  if (ww == 2) {
    return(list( data.frame( r2 ), txfn2))
  }
  if (ww == 3) {
    return(list( data.frame( r3 ), txfn3))
  }
  if (ww == 4) {
    return(list( data.frame( r4 ), txfn4))
  }
}
