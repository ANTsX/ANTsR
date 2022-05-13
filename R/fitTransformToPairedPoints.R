#' fitTransformToPairedPoints
#'
#' Estimate a transform using paired point sets.  An ANTsR transform is returned.
#'
#' @param movingPoints moving points specified in physical space as a \code{n x d}
#' matrix where \code{n} is the number of points and \code{d} is the dimensionality.
#' @param fixedPoints fixed points specified in physical space as a \code{n x d}
#' matrix where \code{n} is the number of points and \code{d} is the dimensionality.
#' @param transformType 'rigid', 'similarity', "affine', 'bspline', or 'diffeo'.
#' @param regularization ridge penalty in [0,1] for linear transforms.
#' @param domainImage defines physical domain of the B-spline transform.
#' @param numberOfFittingLevels integer specifying the number of fitting levels
#' (B-spline only).
#' @param meshSize scalar or vector defining the mesh size at the initial fitting level
#' (B-spline only).
#' @param splineOrder spline order of the B-spline object.  Default = 3 (B-spline
#' only).
#' @param enforceStationaryBoundary ensure no displacements on the image boundary
#' (B-spline only).
#' @param displacementWeights vector defining the individual weighting of the corresponding
#' scattered data value.  (B-spline only).  Default = NULL meaning all displacements are
#' weighted the same.
#' @param numberOfCompositions total number of compositions for the diffeomorphic transform.
#' @param gradientStep scalar multiplication factor for the diffeomorphic transform.
#' @param sigma gaussian smoothing sigma (in mm) for the diffeomorphic transform.
#' @return object containing ANTsR transform, error, and scale (or displacement field)
#'
#' @author B Avants
#' @examples
#' fixed <- matrix( c( 50, 50, 200, 50, 50, 200 ), ncol = 2, byrow = TRUE )
#' moving <- matrix( c( 75, 75, 175, 75, 75, 175 ), ncol = 2, byrow = TRUE )
#'
#' # Affine transform
#' xfrm <- fitTransformToPairedPoints( moving, fixed, transformType = "Affine", regularization = 0 )
#' error <- norm( moving - applyAntsrTransformToPoint( xfrm, fixed ), "F" ) / nrow( fixed )
#' params <- getAntsrTransformParameters( xfrm )
#'
#' # Rigid transform
#' xfrm <- fitTransformToPairedPoints( moving, fixed, transformType = "Rigid", regularization = 0 )
#' error <- norm( moving - applyAntsrTransformToPoint( xfrm, fixed ), "F" ) / nrow( fixed )
#' params <- getAntsrTransformParameters( xfrm )
#'
#' # Similarity transform
#' xfrm <- fitTransformToPairedPoints( moving, fixed, transformType = "Similarity", regularization = 0 )
#' error <- norm( moving - applyAntsrTransformToPoint( xfrm, fixed ), "F" ) / nrow( fixed )
#' params <- getAntsrTransformParameters( xfrm )
#'
#' # B-spline transform
#' domainImage <- antsImageRead( getANTsRData( "r16" ) )
#' xfrm <- fitTransformToPairedPoints( moving, fixed, transformType = "Bspline", domainImage = domainImage, numberOfFittingLevels = 5 )
#' error <- norm( moving - applyAntsrTransformToPoint( xfrm, fixed ), "F" ) / nrow( fixed )
#'
#' # Diffeo transform
#' domainImage <- antsImageRead( getANTsRData( "r16" ) )
#' xfrm <- fitTransformToPairedPoints( moving, fixed, transformType = "Diffeo", domainImage = domainImage, numberOfFittingLevels = 6 )
#' error <- norm( moving - applyAntsrTransformToPoint( xfrm, fixed ), "F" ) / nrow( fixed )
#'
#' @export fitTransformToPairedPoints

fitTransformToPairedPoints <- function(
  movingPoints,
  fixedPoints,
  transformType = 'affine',
  regularization = 1e-6,
  domainImage = NULL,
  numberOfFittingLevels = 4,
  meshSize = 1,
  splineOrder = 3,
  enforceStationaryBoundary = TRUE,
  displacementWeights = NULL,
  numberOfCompositions = 10,
  gradientStep = 0.5,
  smoothingFactor = 3.0
  ) {

  polarDecomposition <- function( X )
    {
    svdX <- svd( X )
    P <- svdX$u %*% diag( svdX$d ) %*% t( svdX$u )
    Z <- svdX$u %*% t( svdX$v )
    if( det( Z ) < 0 )
      {
      D <- diag( nrow( X ) )
      D[1, 1] <- -1.0
      Z <- Z %*% D
      }
    return( list( P = P, Z = Z, Xtilde = P %*% Z ) )
    }

  invert <- function( x, y )
    {
    tryCatch(
      {
      return( qr.solve( x, y ) )
      },
      error = function( c ) {
        return( MASS::ginv( x ) %*% y )
      })
    }

  if( ! any( tolower( transformType ) %in%
        c( "rigid", "affine", "similarity", "bspline", "diffeo" ) ) )
    {
    stop( paste0( transformType, " transform not supported." ) )
    }
  transformType <- tolower( transformType )

  if( ! all( dim( fixedPoints ) == dim( movingPoints ) ) )
    {
    stop( "Mismatch in the size of the point sets." )
    }

  if( regularization > 1 )
    {
    regularization <- 1
    } else if( regularization < 0 ) {
    regularization <- 0
    }

  numberOfPoints <- nrow( fixedPoints )
  dimensionality <- ncol( fixedPoints )

  if( transformType %in% c( "rigid", "affine", "similarity" ) )
    {
    centerFixed = colMeans( fixedPoints )
    centerMoving = colMeans( movingPoints )

    x <- sweep( fixedPoints, 2, centerFixed, '-' )
    y <- sweep( movingPoints, 2, centerMoving, '-' )

    yPrior = cbind( y, rep( 1, numberOfPoints ) )

    x11 <- cbind( x, rep( 1, numberOfPoints ) )
    M <- x11 * ( 1.0 - regularization ) + regularization * yPrior
    Minv <- invert( M, y )

    p <- polarDecomposition( t( Minv[1:dimensionality, 1:dimensionality] ) )
    A <- p$Xtilde
    translation <- Minv[dimensionality+1,] + centerMoving - centerFixed

    if( transformType %in% c( "rigid", "similarity" ) )
      {
      # Kabsch algorithm
      #    http://web.stanford.edu/class/cs273/refs/umeyama.pdf

      C <- ( t( y ) %*% x )
      xSvd <- svd( C * ( 1.0 - regularization ) + diag( dimensionality ) * regularization )
      xDet <- det( xSvd$u %*% t( xSvd$v ) )

      if( xDet > 0 )
        {
        A <- xSvd$u %*% t( xSvd$v )
        } else {
        signAdj <- diag( c( -1, rep( 1, dimensionality - 1 ) ) )
        A <- ( xSvd$u %*% signAdj ) %*% t( xSvd$v )
        }

      if ( transformType == "similarity" )
        {
        scaling <- sqrt( mean( rowSums( y^2 ) / numberOfPoints )  ) /
                   sqrt( mean( rowSums( x^2 ) / numberOfPoints )  )
        A <- A %*% ( diag( dimensionality ) * scaling )
        }
      }

    xfrm <- createAntsrTransform( matrix = A, translation = translation,
      dimension = dimensionality, center = centerFixed )


    return( xfrm )

    } else if( transformType == "bspline" ) {

    bsplineDisplacementField <- fitBsplineDisplacementField(
      displacementOrigins = fixedPoints,
      displacements = movingPoints - fixedPoints,
      displacementWeights = displacementWeights,
      origin = antsGetOrigin( domainImage ),
      spacing = antsGetSpacing( domainImage ),
      size = dim( domainImage ),
      direction = antsGetDirection( domainImage ),
      numberOfFittingLevels = numberOfFittingLevels,
      meshSize = meshSize,
      splineOrder = splineOrder,
      enforceStationaryBoundary = enforceStationaryBoundary )

    xfrm <- antsrTransformFromDisplacementField( bsplineDisplacementField )

    return( xfrm )

    } else {

    updatedFixedPoints <- fixedPoints

    xfrmList <- list()
    totalFieldXfrm <- NULL

    for( i in seq.int( numberOfCompositions ) )
      {
      updateField <- fitBsplineDisplacementField(
        displacementOrigins = fixedPoints,
        displacements = movingPoints - updatedFixedPoints,
        displacementWeights = displacementWeights,
        origin = antsGetOrigin( domainImage ),
        spacing = antsGetSpacing( domainImage ),
        size = dim( domainImage ),
        direction = antsGetDirection( domainImage ),
        numberOfFittingLevels = numberOfFittingLevels,
        meshSize = meshSize,
        splineOrder = splineOrder,
        enforceStationaryBoundary = TRUE
        )

      updateField <- updateField * gradientStep
      if( sigma > 0 )
        {
        updateField <- smoothImage( updateField, sigma )
        }

      xfrmList[[i]] <- antsrTransformFromDisplacementField( updateFieldSmooth )
      totalFieldXfrm <- composeAntsrTransforms( xfrmList )

      if( i < numberOfCompositions )
        {
        updatedFixedPoints <- applyAntsrTransformToPoint( totalFieldXfrm, fixedPoints )
        }
      }

    return( totalFieldXfrm )
    }
}
