#' fitTransformToPairedPoints
#'
#' Estimate a transform using paired point sets.  An ANTsR transform is returned.
#'
#' @param movingPoints Moving points specified in physical space as a \code{n x d}
#' matrix where \code{n} is the number of points and \code{d} is the dimensionality.
#' @param fixedPoints Fixed points specified in physical space as a \code{n x d}
#' matrix where \code{n} is the number of points and \code{d} is the dimensionality.
#' @param transformType 'rigid', 'similarity', 'affine', 'bspline', 'tps',
#' 'diffeo', 'syn', 'tv', or 'time-varying'.
#' @param regularization Ridge penalty in [0,1] for linear transforms.
#' @param domainImage Defines physical domain of the B-spline transform.  Must be defined
#' for nonlinear transforms.
#' @param numberOfFittingLevels Integer specifying the number of fitting levels for the
#' B-spline interpolation of the displacement field.
#' @param meshSize Defines the mesh size at the initial fitting level for the B-spline
#' interpolation of the displacement field.
#' @param splineOrder Spline order of the B-spline displacement field.
#' @param enforceStationaryBoundary Ensure no displacements on the image boundary
#' (B-spline only).
#' @param displacementWeights Defines the individual weighting of the corresponding scattered
#' data value.  Default = None meaning all displacements are weighted the same.
#' @param numberOfCompositions Total number of compositions for the diffeomorphic transform.
#' @param compositionStepSize Scalar multiplication factor of the weighting of the update field
#' for the diffeomorphic transforms.
#' @param sigma Gaussian smoothing standard deviation of the update field (in mm).
#' @param convergenceThreshold Composition-based convergence parameter for the diffeomorphic
#' transforms using a window size of 10 values.
#' @param numberOfIntegrationPoints Time-varying velocity field parameter.
#' @param numberOfIntegrationSteps Number of steps used for integrating the velocity field.
#' @param rasterizePoints Use nearest neighbor rasterization of points for estimating update
#' field (potential speed-up).
#' @param verbose Print progress to the screen.
#' @return object containing ANTsR transform, error, and scale (or displacement field)
#'
#' @author B Avants, N Tustison
#' @examples
#' fixed <- matrix( c( 50, 50, 200, 50, 50, 200 ), ncol = 2, byrow = TRUE )
#' moving <- matrix( c( 50, 50, 50, 200, 200, 200 ), ncol = 2, byrow = TRUE )
#'
#' # Affine transform
#' xfrm <- fitTransformToPairedPoints( moving, fixed, transformType = "Affine", regularization = 0 )
#' params <- getAntsrTransformParameters( xfrm )
#'
#' # Rigid transform
#' xfrm <- fitTransformToPairedPoints( moving, fixed, transformType = "Rigid", regularization = 0 )
#' params <- getAntsrTransformParameters( xfrm )
#'
#' # Similarity transform
#' xfrm <- fitTransformToPairedPoints( moving, fixed, transformType = "Similarity", regularization = 0 )
#' params <- getAntsrTransformParameters( xfrm )
#'
#' # B-spline transform
#' domainImage <- antsImageRead( getANTsRData( "r16" ) )
#' xfrm <- fitTransformToPairedPoints( moving, fixed, transformType = "Bspline", domainImage = domainImage, numberOfFittingLevels = 5 )
#'
#' # Diffeo transform
#' domainImage <- antsImageRead( getANTsRData( "r16" ) )
#' xfrm <- fitTransformToPairedPoints( moving, fixed, transformType = "Diffeo", domainImage = domainImage, numberOfFittingLevels = 6 )
#'
#' # SyN transform
#' domainImage <- antsImageRead( getANTsRData( "r16" ) )
#' xfrm <- fitTransformToPairedPoints( moving, fixed, transformType = "SyN", domainImage = domainImage, numberOfFittingLevels = 6, numberOfCompositions = 10, compositionStepSize = 0.01 )
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
  numberOfIntegrationSteps = 100,
  rasterizePoints = FALSE,
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
    origin <- antsGetOrigin( domainImage )
    spacing <- antsGetSpacing( domainImage )
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
        c( "rigid", "affine", "similarity", "bspline", "tps", "diffeo", "syn", "tv", "time-varying" ) ) )
    {
    stop( paste0( transformType, " transform not supported." ) )
    }
  transformType <- tolower( transformType )

  if( is.null( domainImage ) && any( tolower( transformType ) %in% c( "bspline", "tps", "diffeo", "syn", "tv", "time-varying" ) ) )
    {
    stop( "Domain image needs to be specified." )
    }

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
      enforceStationaryBoundary = enforceStationaryBoundary,
      rasterizePoints = rasterizePoints )

    xfrm <- antsrTransformFromDisplacementField( bsplineDisplacementField )

    return( xfrm )

    } else if( transformType == "tps" ) {

    tpsDisplacementField <- fitThinPlateSplineDisplacementField(
      displacementOrigins = fixedPoints,
      displacements = movingPoints - fixedPoints,
      origin = antsGetOrigin( domainImage ),
      spacing = antsGetSpacing( domainImage ),
      size = dim( domainImage ),
      direction = antsGetDirection( domainImage ) )

    xfrm <- antsrTransformFromDisplacementField( tpsDisplacementField )

    return( xfrm )

    } else if( transformType == "diffeo" ) {

    if( verbose )
      {
      startTotalTime <- Sys.time()
      }

    updatedFixedPoints <- fixedPoints

    totalField <- createZeroDisplacementField( domainImage )
    totalFieldXfrm <- NULL

    errorValues <- c()
    for( i in seq.int( numberOfCompositions ) )
      {
      if( verbose )
        {
        startTime <- Sys.time()
        }
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
        enforceStationaryBoundary = TRUE,
        rasterizePoints = rasterizePoints
       )

      updateField <- updateField * compositionStepSize
      if( sigma > 0 )
        {
        updateField <- smoothImage( updateField, sigma )
        }

      totalField <- composeDisplacementFields( updateField, totalField )
      totalFieldXfrm <- antsrTransformFromDisplacementField( totalField )

      if( i < numberOfCompositions )
        {
        updatedFixedPoints <- applyAntsrTransformToPoint( totalFieldXfrm, fixedPoints )
        }

      error <- mean( sqrt( rowSums( ( updatedFixedPoints - updatedMovingPoints )^2 ) ) )
      errorValues <- append( errorValues, error )
      convergenceValue <- convergenceMonitoring( errorValues )
      if( verbose )
        {
        endTime <- Sys.time()
        diffTime <- endTime - startTime
        cat( "Composition ", i, ": error = ", error, " (convergence = ", convergenceValue, ", elapsed time = ", diffTime, ")\n", sep = "" )
        }
      if( ! is.na( convergenceValue ) && convergenceValue < convergenceThreshold )
        {
        break
        }
      }

    if( verbose )
      {
      endTotalTime <- Sys.time()
      diffTotalTime <- endTotalTime - startTotalTime
      cat( "Total elapsed time = ", diffTotalTime, ".\n", sep = "" )
      }

    return( totalFieldXfrm )

    } else if( transformType == "syn" ) {

    if( verbose )
      {
      startTotalTime <- Sys.time()
      }

    updatedFixedPoints <- fixedPoints
    updatedMovingPoints <- movingPoints

    totalFieldFixedToMiddle <- createZeroDisplacementField( domainImage )
    totalInverseFieldFixedToMiddle <- createZeroDisplacementField( domainImage )

    totalFieldMovingToMiddle <- createZeroDisplacementField( domainImage )
    totalInverseFieldMovingToMiddle <- createZeroDisplacementField( domainImage )

    errorValues <- c()
    for( i in seq.int( numberOfCompositions ) )
      {
      if( verbose )
        {
        startTime <- Sys.time()
        }
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
        enforceStationaryBoundary = TRUE,
        rasterizePoints = rasterizePoints
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
        enforceStationaryBoundary = TRUE,
        rasterizePoints = rasterizePoints
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

      error <- mean( sqrt( rowSums( ( updatedFixedPoints - updatedMovingPoints )^2 ) ) )
      errorValues <- append( errorValues, error )
      convergenceValue <- convergenceMonitoring( errorValues )
      if( verbose )
        {
        endTime <- Sys.time()
        diffTime <- endTime - startTime
        cat( "Composition ", i, ": error = ", error, " (convergence = ", convergenceValue, ", elapsed time = ", diffTime, ")\n", sep = "" )
        }
      if( ! is.na( convergenceValue ) && convergenceValue < convergenceThreshold )
        {
        break
        }
      }

    totalForwardField <- composeDisplacementFields( totalInverseFieldMovingToMiddle, totalFieldFixedToMiddle )
    totalForwardXfrm <- createAntsrTransform( type = "DisplacementFieldTransform", displacement.field = totalForwardField )
    totalInverseField <- composeDisplacementFields( totalInverseFieldFixedToMiddle, totalFieldMovingToMiddle )
    totalInverseXfrm <- createAntsrTransform( type = "DisplacementFieldTransform", displacement.field = totalInverseField )

    if( verbose )
      {
      endTotalTime <- Sys.time()
      diffTotalTime <- endTotalTime - startTotalTime
      cat( "Total elapsed time = ", diffTotalTime, ".\n", sep = "" )
      }

    return( list( forwardTransform = totalForwardXfrm,
                  inverseTransform = totalInverseXfrm,
                  fixedToMiddleTransform = totalFieldFixedToMiddleXfrm,
                  middleToFixedTransform = totalInverseFieldFixedToMiddleXfrm,
                  movingToMiddleTransform = totalFieldMovingToMiddleXfrm,
                  middleToMovingTransform = totalInverseFieldMovingToMiddleXfrm ) )

    } else if( transformType == "tv" || transformType == "time-varying" ) {

    if( verbose )
      {
      startTotalTime <- Sys.time()
      }

    updatedFixedPoints <- fixedPoints
    updatedMovingPoints <- movingPoints

    velocityField <- createZeroVelocityField( domainImage, numberOfIntegrationPoints )
    velocityFieldArray <- as.array( velocityField )

    lastUpdateDerivativeField <- createZeroVelocityField( domainImage, numberOfIntegrationPoints )
    lastUpdateDerivativeFieldArray <- as.array( lastUpdateDerivativeField )

    errorValues <- c()
    for( i in seq.int( numberOfCompositions ) )
      {
      if( verbose )
        {
        startTime <- Sys.time()
        }
      updateDerivativeField <- createZeroVelocityField( domainImage, numberOfIntegrationPoints )
      updateDerivativeFieldArray <- as.array( updateDerivativeField )

      averageError <- 0.0
      for( n in seq_len( numberOfIntegrationPoints ) )
        {
        t <- ( n - 1 ) / ( numberOfIntegrationPoints - 1.0 )

        if( n > 1 )
          {
          integratedForwardField <- integrateVelocityField( velocityField, 0.0, t, numberOfIntegrationSteps )
          integratedForwardFieldXfrm <- createAntsrTransform( type = "DisplacementFieldTransform", displacement.field = integratedForwardField )
          updatedFixedPoints <- applyAntsrTransformToPoint( integratedForwardFieldXfrm, fixedPoints )
          } else {
          updatedFixedPoints <- fixedPoints
          }

        if( n < numberOfIntegrationPoints )
          {
          integratedInverseField <- integrateVelocityField( velocityField, 1.0, t, numberOfIntegrationSteps )
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
          enforceStationaryBoundary = TRUE,
          rasterizePoints = rasterizePoints
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

        rmse <- mean( sqrt( rowSums( ( updatedFixedPoints - updatedMovingPoints )^2 ) ) )
        averageError <- ( averageError * ( n - 1 ) + rmse ) / n
        }
      updateDerivativeFieldArray <- ( updateDerivativeFieldArray + lastUpdateDerivativeFieldArray ) * 0.5
      lastUpdateDerivativeFieldArray <- updateDerivativeFieldArray

      velocityFieldArray <- velocityFieldArray + updateDerivativeFieldArray * compositionStepSize
      velocityField <- as.antsImage( velocityFieldArray, origin = antsGetOrigin( velocityField ),
          spacing = antsGetSpacing( velocityField ), direction = antsGetDirection( velocityField ),
          components = TRUE )

      errorValues <- append( errorValues, averageError )
      convergenceValue <- convergenceMonitoring( errorValues )
      if( verbose )
        {
        endTime <- Sys.time()
        diffTime <- endTime - startTime
        cat( "Composition ", i, ": error = ", averageError, " (convergence = ", convergenceValue, ", elapsed time = ", diffTime, ")\n", sep = "" )
        }
      if( ! is.na( convergenceValue ) && convergenceValue < convergenceThreshold )
        {
        break
        }
      }

    integratedForwardField <- integrateVelocityField( velocityField, 0.0, t, numberOfIntegrationSteps )
    forwardXfrm <- createAntsrTransform( type = "DisplacementFieldTransform", displacement.field = integratedForwardField )

    integratedInverseField <- integrateVelocityField( velocityField, 1.0, t, numberOfIntegrationSteps )
    inverseXfrm <- createAntsrTransform( type = "DisplacementFieldTransform", displacement.field = integratedInverseField )

    if( verbose )
      {
      endTotalTime <- Sys.time()
      diffTotalTime <- endTotalTime - startTotalTime
      cat( "Total elapsed time = ", diffTotalTime, ".\n", sep = "" )
      }

    return( list( forwardTransform = forwardXfrm,
                  inverseTransform = inverseXfrm,
                  velocityField = velocityField ) )

    } else {
      stop( "Unrecognized transformType." )
    }
}


#' fitTimeVaryingTransformToPointSets
#'
#' Estimate a time-varying transform from corresponding point sets (> 2).
#'
#' @param pointSets Corresponding points across sets specified in physical space as a
#' \code{n x d} matrix where \code{n} is the number of points and \code{d} is the
#' dimensionality.
#' @param timePoints Set of scalar values, one for each point-set designating its time
#' position in the velocity flow.  If not set, it defaults to equal spacing between 0
#' and 1.
#' @param initialVelocityField Optional velocity field for initializing optimization.
#' Overrides the number of integration points.
#' @param numberOfIntegrationPoints Time-varying velocity field parameter.  Needs to
#' be equal to or greater than the number of point sets.  If not specified, it
#' defaults to the number of point sets.
#' @param domainImage Defines physical domain of the B-spline transform.  Must be defined
#' for nonlinear transforms.
#' @param numberOfFittingLevels Integer specifying the number of fitting levels for the
#' B-spline interpolation of the displacement field.
#' @param meshSize Defines the mesh size at the initial fitting level for the B-spline
#' interpolation of the displacement field.
#' @param splineOrder Spline order of the B-spline displacement field.
#' @param displacementWeights vector defining the individual weighting of the corresponding
#' scattered data value.  Default = NULL meaning all displacements are
#' weighted the same.
#' @param numberOfCompositions Total number of compositions for the diffeomorphic transform.
#' @param compositionStepSize Scalar multiplication factor of the weighting of the update field
#' for the diffeomorphic transforms.
#' @param sigma Gaussian smoothing standard deviation of the update field (in mm).
#' @param numberOfIntegrationSteps Number of steps used for integrating the velocity field.
#' @param convergenceThreshold Composition-based convergence parameter for the diffeomorphic
#' transforms using a window size of 10 values.
#' @param rasterizePoints Use nearest neighbor rasterization of points for estimating update
#' field (potential speed-up).
#' @param verbose Print progress to the screen.
#' @return object containing ANTsR transform, error, and scale (or displacement field)
#'
#' @author B Avants, N Tustison
#' @export fitTimeVaryingTransformToPointSets
fitTimeVaryingTransformToPointSets <- function(
  pointSets,
  timePoints = NULL,
  initialVelocityField = NULL,
  numberOfIntegrationPoints=NULL,
  domainImage = NULL,
  numberOfFittingLevels = 4,
  meshSize = 1,
  splineOrder = 3,
  displacementWeights = NULL,
  numberOfCompositions = 10,
  compositionStepSize = 0.5,
  numberOfIntegrationSteps = 100,
  sigma = 0.0,
  convergenceThreshold = 0.0,
  rasterizePoints = FALSE,
  verbose = FALSE
  ) {

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

  if( ! is.list( pointSets ) )
    {
    stop( "Point sets should be a list of corresponding point sets." )
    }

  numberOfPointSets <- length( pointSets )

  if( ! is.null( timePoints ) && length( timePoints ) != numberOfPointSets )
    {
    stop( "The number of time points should be the same as the number of point sets." )
    }

  if( is.null( timePoints ) )
    {
    timePoints <- seq( from = 0.0, to = 1.0, length.out = numberOfPointSets )
    }

  if( any( timePoints < 0.0 ) || any( timePoints > 1.0 ) )
    {
    stop( "Time point values should be between 0 and 1." )
    }

  if( numberOfPointSets < 3 )
    {
    stop( "Expecting three or greater point sets." )
    }

  if( is.null( domainImage ) )
    {
    stop( "Domain image needs to be specified." )
    }

  numberOfPoints <- nrow( pointSets[[1]] )
  dimensionality <- ncol( pointSets[[1]] )
  for( i in seq.int( 2, numberOfPointSets ) )
    {
    if( nrow( pointSets[[i]] ) != numberOfPoints )
      {
      stop( "Point sets should match in terms of the number of points." )
      }
    if( ncol( pointSets[[i]] ) != dimensionality )
      {
      stop( "Point sets should match in terms of dimensionality." )
      }
    }

  if( verbose )
    {
    startTotalTime <- Sys.time()
    }

  updatedFixedPoints <- array( data = 0, dim = dim( pointSets[[1]] ) )
  updatedMovingPoints <- array( data = 0, dim = dim( pointSets[[1]] ) )

  velocityField <- NULL
  if( is.null( initialVelocityField ) )
    {
    if( is.null( numberOfIntegrationPoints ) )
      {
      numberOfIntegrationPoints <- length( timePoints )
      }
    if( numberOfIntegrationPoints < numberOfPointSets )
      {
      stop( "The number of integration points should be at least as great as the number of point sets." )
      }
    velocityField <- createZeroVelocityField( domainImage, numberOfIntegrationPoints )
    } else {
    velocityField <- antsImageClone( initialVelocityField )
    numberOfIntegrationPoints <- tail( dim( initialVelocityField ), 1 )
    }
  velocityFieldArray <- as.array( velocityField )

  lastUpdateDerivativeField <- createZeroVelocityField( domainImage, numberOfIntegrationPoints )
  lastUpdateDerivativeFieldArray <- as.array( lastUpdateDerivativeField )

  errorValues <- c()
  for( i in seq.int( numberOfCompositions ) )
    {
    if( verbose )
      {
      startTime <- Sys.time()
      }
    updateDerivativeField <- createZeroVelocityField( domainImage, numberOfIntegrationPoints )
    updateDerivativeFieldArray <- as.array( updateDerivativeField )

    averageError <- 0.0
    for( n in seq_len( numberOfIntegrationPoints ) )
      {
      t <- ( n - 1 ) / ( numberOfIntegrationPoints - 1.0 )

      tIndex <- 0
      for( j in seq.int( from = 2, to = numberOfPointSets ) )
        {
        if( timePoints[j-1] <= t && timePoints[j] >= t )
          {
          tIndex <- j
          break
          }
        }

      if( n > 1 && n < numberOfIntegrationPoints && timePoints[tIndex-1] == t )
        {
        updatedFixedPoints <- pointSets[[tIndex-1]]
        integratedInverseField <- integrateVelocityField( velocityField, timePoints[tIndex], t, numberOfIntegrationSteps )
        integratedInverseFieldXfrm <- createAntsrTransform( type = "DisplacementFieldTransform", displacement.field = integratedInverseField )
        updatedMovingPoints <- applyAntsrTransformToPoint( integratedInverseFieldXfrm, pointSets[[tIndex]] )

        updateDerivativeFieldAtTimePointForward <- fitBsplineDisplacementField(
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
          enforceStationaryBoundary = TRUE,
          rasterizePoints = rasterizePoints
          )

        updatedMovingPoints <- pointSets[[tIndex-1]]
        integratedForwardField <- integrateVelocityField( velocityField, timePoints[tIndex-2], t, numberOfIntegrationSteps )
        integratedForwardFieldXfrm <- createAntsrTransform( type = "DisplacementFieldTransform", displacement.field = integratedForwardField )
        updatedFixedPoints <- applyAntsrTransformToPoint( integratedForwardFieldXfrm, pointSets[[tIndex-2]] )

        updateDerivativeFieldAtTimePointBack <- fitBsplineDisplacementField(
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
          enforceStationaryBoundary = TRUE,
          rasterizePoints = rasterizePoints
          )

        updateDerivativeFieldAtTimePoint <- ( updateDerivativeFieldAtTimePointForward +
                                              updateDerivativeFieldAtTimePointBack ) / 2.0

        } else {
        if( t == 0.0 && timePoints[tIndex-1] == 0.0 )
          {
          updatedFixedPoints <- pointSets[[1]]
          } else {
          integratedForwardField <- integrateVelocityField( velocityField, timePoints[tIndex-1], t, numberOfIntegrationSteps )
          integratedForwardFieldXfrm <- createAntsrTransform( type = "DisplacementFieldTransform", displacement.field = integratedForwardField )
          updatedFixedPoints <- applyAntsrTransformToPoint( integratedForwardFieldXfrm, pointSets[[tIndex-1]] )
          }

        if( t == 1.0 && timePoints[tIndex] == 1.0 )
          {
          updatedMovingPoints <- pointSets[[length( pointSets )]]
          } else {
          integratedInverseField <- integrateVelocityField( velocityField, timePoints[tIndex], t, numberOfIntegrationSteps )
          integratedInverseFieldXfrm <- createAntsrTransform( type = "DisplacementFieldTransform", displacement.field = integratedInverseField )
          updatedMovingPoints <- applyAntsrTransformToPoint( integratedInverseFieldXfrm, pointSets[[tIndex]] )
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
          enforceStationaryBoundary = TRUE,
          rasterizePoints = rasterizePoints
          )
        }
      if( sigma > 0 )
        {
        updateDerivativeFieldAtTimePoint <- smoothImage( updateDerivativeFieldAtTimePoint, sigma )
        }

      updateDerivativeFieldAtTimePointArray <- as.array( updateDerivativeFieldAtTimePoint )
      maxNorm <- sqrt( max( base::colSums( updateDerivativeFieldAtTimePointArray ^ 2, dims = 1 ) ) )
      if( verbose )
        {
        cat("  integration point ", t, ": maxNorm = ", maxNorm, "\n", sep = "")
        }
      updateDerivativeFieldAtTimePointArray <- updateDerivativeFieldAtTimePointArray / maxNorm
      if( domainImage@dimension == 2 )
        {
        updateDerivativeFieldArray[,,,n] <- updateDerivativeFieldAtTimePointArray
        } else {
        updateDerivativeFieldArray[,,,,n] <- updateDerivativeFieldAtTimePointArray
        }
      rmse <- mean( sqrt( rowSums( ( updatedFixedPoints - updatedMovingPoints )^2 ) ) )
      averageError <- ( averageError * ( n - 1 ) + rmse ) / n
      }
    updateDerivativeFieldArray <- ( updateDerivativeFieldArray + lastUpdateDerivativeFieldArray ) * 0.5
    lastUpdateDerivativeFieldArray <- updateDerivativeFieldArray

    velocityFieldArray <- velocityFieldArray + updateDerivativeFieldArray * compositionStepSize
    velocityField <- as.antsImage( velocityFieldArray, origin = antsGetOrigin( velocityField ),
        spacing = antsGetSpacing( velocityField ), direction = antsGetDirection( velocityField ),
        components = TRUE )

    errorValues <- append( errorValues, averageError )
    convergenceValue <- convergenceMonitoring( errorValues )
    if( verbose )
      {
      endTime <- Sys.time()
      diffTime <- endTime - startTime
      cat( "Composition ", i, ": error = ", averageError, " (convergence = ", convergenceValue, ", elapsed time = ", diffTime, ")\n", sep = "" )
      }
    if( ! is.na( convergenceValue ) && convergenceValue < convergenceThreshold )
      {
      break
      }
    }

  integratedForwardField <- integrateVelocityField( velocityField, 0.0, 1.0, numberOfIntegrationSteps )
  forwardXfrm <- createAntsrTransform( type = "DisplacementFieldTransform", displacement.field = integratedForwardField )

  integratedInverseField <- integrateVelocityField( velocityField, 1.0, 0.0, numberOfIntegrationSteps )
  inverseXfrm <- createAntsrTransform( type = "DisplacementFieldTransform", displacement.field = integratedInverseField )

  if( verbose )
    {
    endTotalTime <- Sys.time()
    diffTotalTime <- endTotalTime - startTotalTime
    cat( "Total elapsed time = ", diffTotalTime, ".\n", sep = "" )
    }

  return( list( forwardTransform = forwardXfrm,
                inverseTransform = inverseXfrm,
                velocityField = velocityField ) )

  }

