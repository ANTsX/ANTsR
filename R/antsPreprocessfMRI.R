antsPreprocessfMRI <- function( boldImage,
  maskImage = NA, maskingMeanRatioThreshold = 0.75,
  initialNuisanceVariables = NA, numberOfCompCorComponents = 6,
  doMotionCorrection = TRUE, useMotionCorrectedImage = FALSE,
  spatialSmoothingParameter = 0.0, spatialSmoothingNumberOfIterations = 5,
  frequencyLowThreshold = 0.01, frequencyHighThreshold = 0.1 )
{

# compute nuisance variables

if( ! is.na( initialNuisanceVariables ) )
  {
  nuisanceVariables <- matrix( NA, nrow = 0, ncol = 0 )
  } else {
  nuisanceVariables <- initialNuisanceVariables
  }

numberOfTimePoints <- dim( boldImage )[4]

# do motion correction
# http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3254728/

framewiseDisplacement <- rep( 0, numberOfTimePoints )
if( doMotionCorrection )
  {
  motionCorrectionResults <- motion_correction( boldImage, moreaccurate = TRUE )
  motionCorrectionParameters <- motionCorrectionResults$moco_params
  nuisanceVariables <- as.matrix( motionCorrectionParameters )[, 2:numberOfTimePoints]
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
  }

averageImage <- new( "antsImage", "float", 3 )
antsMotionCorr( list( d = 3, a = boldImage, o = averageImage ) )

# Calculate the mask, if not supplied.

if( is.na( maskImage ) )
  {
  maskImage <- getMask( averageImage, mean( averageImage ) * maskingThreshold , Inf, TRUE )
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
boldResiduals <- residuals( lm( boldMatrix ~ 1 + nuisanceVariables ) )
boldResidualsFiltered <- frequencyFilterfMRI( boldResiduals, tr = antsGetSpacing( boldImage )[4],
                           freqLo = frequencyLowThreshold, freqHi = frequencyHighThreshold, opt = "trig" )

# For quality assurance measures, we calculate the temporal derivative
# of the RMS variance over voxels (DVARS as in
# http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3254728/)

DVARS <- rep( 0, nrow( boldResidualsFiltered ) )
for ( i in 2:nrow( boldResidualsFiltered ) ) {
    DVARS[i] <- sqrt( mean( ( boldResidualsFiltered[i,] - boldResidualsFiltered[i-1,] )^2 ) )
}
DVARS[1] <- mean( DVARS )

# Convert the cleaned matrix back to a 4-D image

cleanBoldImage <- matrix2timeseries( boldImage, maskImage, boldResidualsFiltered )

# anisotropically smooth the 4-D image, if desired

if( spatialSmoothingParameter > 0.0 & spatialSmoothingNumberOfIterations > 0 )
  {
  ImageMath( 4, cleanBoldImage, "PeronaMalik", cleanBoldImage,
    spatialSmoothingParameter, spatialSmoothingNumberOfIterations )
  }

return( list( cleanBoldImage = cleanBoldImage, maskImage = maskImage, DVARS = DVARS, FD = framewiseDisplacement ) )
}
