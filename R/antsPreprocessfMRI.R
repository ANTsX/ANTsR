antsPreprocessfMRI <- function( boldImage,
  maskImage = NA, maskingThreshold = 0.75,
  initialNuisanceVariables = NA, doCompCor = 6,
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

if( doMotionCorrection )
  {
  motionCorrectionResults <- motion_correction( boldImage, moreaccurate = TRUE )
  motionCorrectionParameters <- motionCorrectionResults$moco_params
  nuisanceVariables <- as.matrix( motionCorrectionParameters )[,2:ncol( motionCorrectionParameters )]

  if( useMotionCorrectedImage )
    {
    boldImage <- motionCorrectionResults$moco_img
    }
  }

averageImage <- new( "antsImage", "float", 3 )
antsMotionCorr( list( d = 3, a = boldImage, o = averageImage ) )

if( is.na( maskImage ) )
  {
  maskImage <- getMask( averageImage, mean( averageImage ) * maskingThreshold , Inf, TRUE )
  }
averageImage[maskImage == 0] <- 0

if( doCompCor > 0 )
  {
  compcorNuisanceVariables <- compcor( boldImage, maskImage, ncompcor = doCompCor, variance_extreme = 0.975 )
  if( dim( nuisanceVariables )[1] > 0 )
    {
    nuisanceVariables <- cbind( nuisanceVariables, compcorNuisanceVariables )
    } else {
    nuisanceVariables <- compcorNuisanceVariables
    }
  }

# do nuisance regression then bandpass filtering
# http://blogs.discovermagazine.com/neuroskeptic/2013/06/12/when-cleaning-fmri-data-is-a-nuisance/

boldMatrix <- timeseries2matrix( boldImage, maskImage )
boldResiduals <- residuals( lm( boldMatrix ~ 1 + nuisanceVariables ) )
boldResidualsFiltered <- frequencyFilterfMRI( boldResiduals, tr = antsGetSpacing( boldImage )[4],
                           freqLo = frequencyLowThreshold, freqHi = frequencyHighThreshold, opt = "trig" )

DVARS<-rep(0,nrow(boldResidualsFiltered))
for ( i in 2:nrow(boldResidualsFiltered) ) {
    DVARS[i]<-sqrt( mean( ( boldResidualsFiltered[i,] - boldResidualsFiltered[i-1,] )^2 ) )
}

cleanBoldImage <- matrix2timeseries( boldImage, maskImage, boldResidualsFiltered )

# anisotropically smooth the images, if desired

if( spatialSmoothingParameter > 0.0 & spatialSmoothingNumberOfIterations > 0 )
  {
  ImageMath( 4, cleanBoldImage, "PeronaMalik", cleanBoldImage,
    spatialSmoothingParameter, spatialSmoothingNumberOfIterations )
  }

return( list( cleanBoldImage = cleanBoldImage, maskImage = maskImage, DVARS=DVARS ) )
}
