#' fitTransformToPairedPoints
#'
#' Estimate a transform using paired point sets.  An ANTsR transform is returned.
#'
#' @param movingPoints moving points specified in physical space as a \code{n x d}
#' matrix where \code{n} is the number of points and \code{d} is the dimensionality.
#' @param fixedPoints fixed points specified in physical space as a \code{n x d}
#' matrix where \code{n} is the number of points and \code{d} is the dimensionality.
#' @param transformType 'rigid', 'similarity', "affine', 'bspline', 'diffeo', 'syn',
#' or 'time-varying (tv)'.
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
#' @param compositionStepSize scalar multiplication factor for the diffeomorphic transform.
#' @param sigma gaussian smoothing sigma (in mm) for the diffeomorphic transform.
#' @param convergenceThreshold Composition-based convergence parameter for the diff. transforms using a
#' window size of 10 values.
#' @param numberOfIntegrationPoints Time-varying velocity field parameter.
#' @param verbose Print progress to the screen.
#' @return object containing ANTsR transform, error, and scale (or displacement field)
#'
#' @author B Avants
#' @examples
#' fixed <- matrix( c( 50, 50, 200, 50, 50, 200 ), ncol = 2, byrow = TRUE )
#' moving <- matrix( c( 50, 50, 50, 200, 200, 200 ), ncol = 2, byrow = TRUE )
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
#' # SyN transform
#' domainImage <- antsImageRead( getANTsRData( "r16" ) )
#' xfrm <- fitTransformToPairedPoints( moving, fixed, transformType = "SyN", domainImage = domainImage, numberOfFittingLevels = 6, numberOfCompositions = 10, compositionStepSize = 0.01 )
#' error <- norm( moving - applyAntsrTransformToPoint( xfrm, fixed ), "F" ) / nrow( fixed )
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
  compositionStepSize = 0.5,
  sigma = 0.0,
  convergenceThreshold = 0.0,
  numberOfIntegrationPoints = 2,
  verbose = FALSE
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

  createZeroDisplacementField <- function( domainImage )
    {
    fieldArray <- array( data = 0, dim = c( domainImage@dimension, dim( domainImage ) ) )
    origin <- c( antsGetOrigin( domainImage ), 0.0 )
    spacing <- c( antsGetSpacing( domainImage ), 1.0 )
    direction <- antsGetDirection( domainImage )
    field <- as.antsImage( fieldArray, origin = origin, spacing = spacing, direction = direction, components = TRUE )
    return( field )
    }

  createZeroVelocityField <- function( domainImage, numberOfTimePoints = 2 )
    {
    fieldArray <- array( data = 0, dim = c( domainImage@dimension, dim( domainImage ), numberOfTimePoints ) )
    origin <- c( antsGetOrigin( domainImage ), 0.0 )
    spacing <- c( antsGetSpacing( domainImage ), 1.0 )
    direction <- diag( domainImage@dimension + 1 )
    direction[1:domainImage@dimension, 1:domainImage@dimension] <- antsGetDirection( domainImage )
    field <- as.antsImage( fieldArray, origin = origin, spacing = spacing, direction = direction, components = TRUE )
    return( field )
    }

  convergenceMonitoring <- function( values, windowSize = 10 )
    {
    if( length( values ) >= windowSize )
      {
      u <- seq( from = 0.0, to = 1.0, length.out = windowSize )
      scatteredData <- as.matrix( tail( values, n = windowSize ), ncol = 1 )
      parametricData <- as.matrix( u, ncol = 1 )
      spacing <- 1.0 / ( windowSize - 1.0 )
      bsplineLine <- fitBsplineObjectToScatteredData( scatteredData, parametricData,
          parametricDomainOrigin = c( 0.0 ), parametricDomainSpacing = c( spacing ),
          parametricDomainSize = c( windowSize ), numberOfFittingLevels = 1, meshSize = 1,
          splineOrder = 1 )
      bsplineSlope <- -( bsplineLine[2, 1] - bsplineLine[1, 1] ) / spacing
      return( bsplineSlope )
      }
    else
      {
      return( NA )
      }
    }

  if( ! any( tolower( transformType ) %in%
        c( "rigid", "affine", "similarity", "bspline", "diffeo", "syn", "tv", "time-varying" ) ) )
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

      if( xDet < 0 )
        {
        xSvd$v[dimensionality,] <- xSvd$v[dimensionality,] * -1
        }

      A <- xSvd$u %*% t( xSvd$v )

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

    } else if( transformType == "diffeo" ) {

    updatedFixedPoints <- fixedPoints

    xfrmList <- list()
    totalFieldXfrm <- NULL

    errorValues <- c()
    for( i in seq.int( numberOfCompositions ) )
      {
      updateField <- fitBsplineDisplacementField(
        displacementOrigins = updatedFixedPoints,
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

      updateField <- updateField * compositionStepSize
      if( sigma > 0 )
        {
        updateField <- smoothImage( updateField, sigma )
        }

      xfrmList[[i]] <- antsrTransformFromDisplacementField( updateField )
      totalFieldXfrm <- composeAntsrTransforms( xfrmList )

      if( i < numberOfCompositions )
        {
        updatedFixedPoints <- applyAntsrTransformToPoint( totalFieldXfrm, fixedPoints )
        }

      if( verbose )
        {
        error <- norm( movingPoints - updatedFixedPoints, "F" ) / nrow( updatedFixedPoints )
        errorValues <- append( errorValues, error )
        convergenceValue <- convergenceMonitoring( errorValues )
        cat( "Composition ", i, ": error = ", error, " (convergence = ", convergenceValue, ")\n", sep = "" )
        }
      if( ! is.na( convergenceValue ) && convergenceValue < convergenceThreshold )
        {
        break
        }
      }
    return( totalFieldXfrm )

    } else if( transformType == "syn" ) {

    updatedFixedPoints <- fixedPoints
    updatedMovingPoints <- movingPoints

    totalFieldFixedToMiddle <- createZeroDisplacementField( domainImage )
    totalInverseFieldFixedToMiddle <- createZeroDisplacementField( domainImage )

    totalFieldMovingToMiddle <- createZeroDisplacementField( domainImage )
    totalInverseFieldMovingToMiddle <- createZeroDisplacementField( domainImage )

    errorValues <- c()
    for( i in seq.int( numberOfCompositions ) )
      {
      updateFieldFixedToMiddle <- fitBsplineDisplacementField(
        displacementOrigins = updatedFixedPoints,
        displacements = updatedMovingPoints - updatedFixedPoints,
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

      updateFieldMovingToMiddle <- fitBsplineDisplacementField(
        displacementOrigins = updatedMovingPoints,
        displacements = updatedFixedPoints - updatedMovingPoints,
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

      updateFieldFixedToMiddle <- updateFieldFixedToMiddle * compositionStepSize
      updateFieldMovingToMiddle <- updateFieldMovingToMiddle * compositionStepSize
      if( sigma > 0 )
        {
        updateFieldFixedToMiddle <- smoothImage( updateFieldFixedToMiddle, sigma )
        updateFieldMovingToMiddle <- smoothImage( updateFieldMovingToMiddle, sigma )
        }

      # Add the update field to both forward displacement fields.

      totalFieldFixedToMiddle <- composeDisplacementFields( updateFieldFixedToMiddle, totalFieldFixedToMiddle )
      totalFieldMovingToMiddle <- composeDisplacementFields( updateFieldMovingToMiddle, totalFieldMovingToMiddle )

      # Iteratively estimate the inverse fields.

      totalInverseFieldFixedToMiddle <- invertDisplacementField( totalFieldFixedToMiddle, totalInverseFieldFixedToMiddle )
      totalInverseFieldMovingToMiddle <- invertDisplacementField( totalFieldMovingToMiddle, totalInverseFieldMovingToMiddle )

      totalFieldFixedToMiddle <- invertDisplacementField( totalInverseFieldFixedToMiddle, totalFieldFixedToMiddle )
      totalFieldMovingToMiddle <- invertDisplacementField( totalInverseFieldMovingToMiddle, totalFieldMovingToMiddle )

      totalFieldFixedToMiddleXfrm <- createAntsrTransform( type = "DisplacementFieldTransform", displacement.field = totalFieldFixedToMiddle )
      totalFieldMovingToMiddleXfrm <- createAntsrTransform( type = "DisplacementFieldTransform", displacement.field = totalFieldMovingToMiddle )

      totalInverseFieldFixedToMiddleXfrm <- createAntsrTransform( type = "DisplacementFieldTransform", displacement.field = totalInverseFieldFixedToMiddle )
      totalInverseFieldMovingToMiddleXfrm <- createAntsrTransform( type = "DisplacementFieldTransform", displacement.field = totalInverseFieldMovingToMiddle )

      if( i < numberOfCompositions )
        {
        updatedFixedPoints <- applyAntsrTransformToPoint( totalFieldFixedToMiddleXfrm, fixedPoints )
        updatedMovingPoints <- applyAntsrTransformToPoint( totalFieldMovingToMiddleXfrm, movingPoints )
        }

      if( verbose )
        {
        error <- norm( updatedMovingPoints - updatedFixedPoints, "F" ) / nrow( updatedFixedPoints )
        errorValues <- append( errorValues, error )
        convergenceValue <- convergenceMonitoring( errorValues )
        cat( "Composition ", i, ": error = ", error, " (convergence = ", convergenceValue, ")\n", sep = "" )
        }
      if( ! is.na( convergenceValue ) && convergenceValue < convergenceThreshold )
        {
        break
        }
      }

    xfrmForwardList <- list( totalFieldFixedToMiddleXfrm, totalInverseFieldMovingToMiddleXfrm )
    totalForwardXfrm <- composeAntsrTransforms( xfrmForwardList )
    xfrmInverseList <- list( totalFieldMovingToMiddleXfrm, totalInverseFieldFixedToMiddleXfrm )
    totalInverseXfrm <- composeAntsrTransforms( xfrmInverseList )

    return( list( forwardTransform = totalForwardXfrm,
                  inverseTransform = totalInverseXfrm,
                  fixedToMiddleTransform = totalFieldFixedToMiddleXfrm,
                  middleToFixedTransform = totalInverseFieldFixedToMiddleXfrm,
                  movingToMiddleTransform = totalFieldMovingToMiddleXfrm,
                  middleToMovingTransform = totalInverseFieldMovingToMiddleXfrm ) )

    } else if( transformType == "tv" || transformType == "time-varying" ) {

    updatedFixedPoints <- fixedPoints
    updatedMovingPoints <- movingPoints

    velocityField <- createZeroVelocityField( domainImage, numberOfIntegrationPoints )
    velocityFieldArray <- as.array( velocityField )

    lastUpdateDerivativeField <- createZeroVelocityField( domainImage, numberOfIntegrationPoints )
    lastUpdateDerivativeFieldArray <- as.array( lastUpdateDerivativeField )

    errorValues <- c()
    for( i in seq.int( numberOfCompositions ) )
      {
      updateDerivativeField <- createZeroVelocityField( domainImage, numberOfIntegrationPoints )
      updateDerivativeFieldArray <- as.array( updateDerivativeField )

      for( n in seq.int( numberOfIntegrationPoints ) )
        {
        t <- ( n - 1 ) / ( numberOfIntegrationPoints - 1.0 )

        if( n > 1 )
          {
          integratedForwardField <- integrateVelocityField( velocityField, 0.0, t, 100 )
          integratedForwardFieldXfrm <- createAntsrTransform( type = "DisplacementFieldTransform", displacement.field = integratedForwardField )
          updatedFixedPoints <- applyAntsrTransformToPoint( integratedForwardFieldXfrm, fixedPoints )
          } else {
          updatedFixedPoints <- fixedPoints
          }

        if( n < numberOfIntegrationPoints )
          {
          integratedInverseField <- integrateVelocityField( velocityField, 1.0, t, 100 )
          integratedInverseFieldXfrm <- createAntsrTransform( type = "DisplacementFieldTransform", displacement.field = integratedInverseField )
          updatedMovingPoints <- applyAntsrTransformToPoint( integratedInverseFieldXfrm, movingPoints )
          } else {
          updatedMovingPoints <- movingPoints
          }

        updateDerivativeFieldAtTimePoint <- fitBsplineDisplacementField(
          displacementOrigins = updatedFixedPoints,
          displacements = updatedMovingPoints - updatedFixedPoints,
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
        if( sigma > 0 )
          {
          updateDerivativeFieldAtTimePoint <- smoothImage( updateDerivativeFieldAtTimePoint, sigma )
          }

        updateDerivativeFieldAtTimePointArray <- as.array( updateDerivativeFieldAtTimePoint )
        maxNorm <- sqrt( max( base::colSums( updateDerivativeFieldAtTimePointArray ^ 2, dims = 1 ) ) )
        updateDerivativeFieldAtTimePointArray <- updateDerivativeFieldAtTimePointArray / maxNorm
        if( domainImage@dimension == 2 )
          {
          updateDerivativeFieldArray[,,,n] <- updateDerivativeFieldAtTimePointArray
          } else {
          updateDerivativeFieldArray[,,,,n] <- updateDerivativeFieldAtTimePointArray
          }
        }
      updateDerivativeFieldArray <- ( updateDerivativeFieldArray + lastUpdateDerivativeFieldArray ) * 0.5
      lastUpdateDerivativeFieldArray <- updateDerivativeFieldArray

      velocityFieldArray <- velocityFieldArray + updateDerivativeFieldArray * compositionStepSize
      velocityField <- as.antsImage( velocityFieldArray, origin = antsGetOrigin( velocityField ),
          spacing = antsGetSpacing( velocityField ), direction = antsGetDirection( velocityField ),
          components = TRUE )

      if( verbose )
        {
        error <- norm( updatedMovingPoints - updatedFixedPoints, "F" ) / nrow( updatedFixedPoints )
        errorValues <- append( errorValues, error )
        convergenceValue <- convergenceMonitoring( errorValues )
        cat( "Composition ", i, ": error = ", error, " (convergence = ", convergenceValue, ")\n", sep = "" )
        }
      if( ! is.na( convergenceValue ) && convergenceValue < convergenceThreshold )
        {
        break
        }
      }

    integratedForwardField <- integrateVelocityField( velocityField, 0.0, t, 100 )
    forwardXfrm <- createAntsrTransform( type = "DisplacementFieldTransform", displacement.field = integratedForwardField )

    integratedInverseField <- integrateVelocityField( velocityField, 1.0, t, 100 )
    inverseXfrm <- createAntsrTransform( type = "DisplacementFieldTransform", displacement.field = integratedInverseField )

    return( list( forwardTransform = forwardXfrm,
                  inverseTransform = inverseXfrm,
                  velocityField = velocityField ) )

    } else {
      stop( "Unrecognized transformType." )
    }

}
