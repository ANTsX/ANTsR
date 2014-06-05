antsPreprocessfMRI <- function( boldImage,
  meanBoldFixedImageForMotionCorrection = NA,
  maskImage = NA, maskingMeanRatioThreshold = 0.75,
  initialNuisanceVariables = NA, numberOfCompCorComponents = 6,
  doMotionCorrection = TRUE, useMotionCorrectedImage = FALSE,
  spatialSmoothingType = "none",
  spatialSmoothingParameters = 0.0,
  frequencyLowThreshold = NA, frequencyHighThreshold = NA,
                               motionCorrectionAccuracyLevel = 1 )
{

# compute nuisance variables

nuisanceVariables <- initialNuisanceVariables

numberOfTimePoints <- dim( boldImage )[4]

# do motion correction
# http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3254728/

framewiseDisplacement <- rep( 0, numberOfTimePoints )
if( doMotionCorrection )
  {
  motionCorrectionResults <- motion_correction( boldImage, fixed = meanBoldFixedImageForMotionCorrection, moreaccurate = motionCorrectionAccuracyLevel )
  motionCorrectionParameters <- motionCorrectionResults$moco_params
  nuisanceVariables <- as.matrix( motionCorrectionParameters )[, 3:ncol( motionCorrectionParameters )]
  for( i in 2:numberOfTimePoints )
    {
    motionCorrectionParametersAtTime1 <- c( motionCorrectionParameters[i, 3:14] )
    rotationMatrixAtTime1 <- matrix( as.numeric( motionCorrectionParametersAtTime1[1:9] ), ncol = 3, nrow = 3 )
    translationAtTime1 <- as.numeric( motionCorrectionParametersAtTime1[10:12] )
    motionCorrectionParametersAtTime2 <- c( motionCorrectionParameters[i-1, 3:14] )
    rotationMatrixAtTime2 <- matrix( as.numeric( motionCorrectionParametersAtTime2[1:9] ), ncol = 3, nrow = 3 )
    translationAtTime2 <- as.numeric( motionCorrectionParametersAtTime2[10:12] )

    # pick a point 10 mm from the center

    samplePoint <- data.matrix( t( matrix( rep( 10, 3 ), nrow = 1 ) ) )

    # calculate the transformed point at time point i and ( i - 1 )

    transformedPointAtTime1 <- data.matrix( rotationMatrixAtTime1 ) %*% samplePoint + translationAtTime1
    transformedPointAtTime2 <- data.matrix( rotationMatrixAtTime2 ) %*% samplePoint + translationAtTime2
    framewiseDisplacement[i] <- sum( abs( transformedPointAtTime2 - transformedPointAtTime1 ) )
    }
  framewiseDisplacement[0] <- mean( framewiseDisplacement[2:numberOfTimePoints] )

  if( useMotionCorrectedImage )
    {
    boldImage <- motionCorrectionResults$moco_img
    }
#  nuisanceVariables<-framewiseDisplacement
  }

averageImage <- new( "antsImage", "float", 3 )
antsMotionCorr( list( d = 3, a = boldImage, o = averageImage ) )
# Calculate the mask, if not supplied.

if( is.na( maskImage ) )
  {
  maskImage <- getMask( averageImage, mean( averageImage ) * maskingMeanRatioThreshold , Inf, TRUE )
  }
averageImage[maskImage == 0] <- 0

# Calculate CompCor nuisance variables
#  http://www.ncbi.nlm.nih.gov/pmc/articles/PMC2214855/

if( numberOfCompCorComponents > 0 )
  {
  compCorNuisanceVariables <- compcor( boldImage, maskImage, ncompcor = numberOfCompCorComponents, variance_extreme = 0.975 )
  if( dim( nuisanceVariables )[1] > 0 )
    {
    nuisanceVariables <- cbind( nuisanceVariables, compCorNuisanceVariables )
    } else {
    nuisanceVariables <- compCorNuisanceVariables
    }
  }

# do nuisance regression then bandpass filtering
# http://blogs.discovermagazine.com/neuroskeptic/2013/06/12/when-cleaning-fmri-data-is-a-nuisance/

boldMatrix <- timeseries2matrix( boldImage, maskImage )
# replace boldMatrix in place with residualized version
if( ! is.na( nuisanceVariables[1] ) )
  {  
  print( colnames( nuisanceVariables ) )
  boldMatrix <- residuals( lm( boldMatrix ~ scale( nuisanceVariables ) ) )
  }
# replace boldMatrix in place with frequency filtered version
if( ! is.na( frequencyHighThreshold ) & ! is.na( frequencyHighThreshold ) &
  ( frequencyLowThreshold != frequencyHighThreshold ) )
  {
  boldMatrix <- frequencyFilterfMRI( boldMatrix, tr = antsGetSpacing( boldImage )[4],
    freqLo = frequencyLowThreshold, freqHi = frequencyHighThreshold, opt = "trig" )
  }

# For quality assurance measures, we calculate the temporal derivative
# of the RMS variance over voxels (DVARS as in
# http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3254728/)

DVARS <- rep( 0, nrow( boldMatrix ) )
for( i in 2:nrow( boldMatrix ) )
  {
  DVARS[i] <- sqrt( mean( ( boldMatrix[i,] - boldMatrix[i-1,] )^2 ) )
  }
DVARS[1] <- mean( DVARS )

# Convert the cleaned matrix back to a 4-D image
globalSignal   <- apply( boldMatrix, FUN=mean, MARGIN=2 )
cleanBoldImage <- matrix2timeseries( boldImage, maskImage, boldMatrix )

# anisotropically smooth the 4-D image, if desired

if( spatialSmoothingType == "gaussian" )
  {
  if( length( spatialSmoothingParameters ) == 1 )
    {
    sigmaVector <- paste0( spatialSmoothingParameters[1], 'x',
      spatialSmoothingParameters[1], 'x', spatialSmoothingParameters[1], 'x0' )
    ImageMath( 4, cleanBoldImage, "G", cleanBoldImage, sigmaVector )
    } else {
    cat( "Error:  expecting a single scalar parameter.  See help.\n" )
    return
    }
  } else if( spatialSmoothingType == "perona-malik" ) {
  if( length( spatialSmoothingParameters ) == 2 )
    {
    ImageMath( 4, cleanBoldImage, "PeronaMalik", cleanBoldImage,
      spatialSmoothingParameters[1], spatialSmoothingParameters[2] )
    } else {
    cat( "Error:  expecting a two element vector.  See help.\n" )
    return
    }
  } else if( spatialSmoothingType != "none" ) {
  cat( "Error:  unrecognized smoothing option.  See help.\n" )
  return
  }
#####################################################################
#####################################################################
return( list( cleanBoldImage = cleanBoldImage, maskImage = maskImage, DVARS = DVARS, FD = framewiseDisplacement, globalSignal = globalSignal ) )
}
