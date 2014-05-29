antsPreprocessfMRI <- function( boldImage,
  maskImage = NA, maskingThreshold = 0.75,
  initialNuisanceVariables = NA, doCompCor = 6,
  doMotionCorrection = TRUE, useMotionCorrectedImage = FALSE,
  spatialSmoothingParameter = 0.0, spatialSmoothingNumberOfIterations = 5,
  frequencyLowThreshold = 0.01, frequencyHighThreshold = 0.1 )
{

# compute nuisance variables
if( is.na( initialNuisanceVariables ) )
  {
  nuisanceVariables <- matrix( NA, nrow = 0, ncol = 0 )
  } else {
  nuisanceVariables <- initialNuisanceVariables
  }

ntp<-dim( boldImage )[4]
templateFD<-rep(0, ntp )
if( doMotionCorrection )
  {
  motionCorrectionResults <- motion_correction( boldImage, moreaccurate = TRUE )
  motionCorrectionParameters <- motionCorrectionResults$moco_params
  nuisanceVariables <- as.matrix( motionCorrectionParameters )[,2:ncol( motionCorrectionParameters )]
  for ( i in 2:nrow(motionCorrectionParameters) ) {
    mparams1<-c( motionCorrectionParameters[i,3:14] )
    tmat1<-matrix( as.numeric(mparams1[1:9]), ncol = 3, nrow = 3)
    mparams2<-c( motionCorrectionParameters[i-1,3:14] )
    tmat2<-matrix( as.numeric(mparams2[1:9]), ncol = 3, nrow = 3)
    pt<-t( matrix(  rep(10,3), nrow=1) )
    newpt1<-data.matrix(tmat1) %*%  data.matrix( pt )+as.numeric(mparams1[10:12])
    newpt2<-data.matrix(tmat2) %*%  data.matrix( pt )+as.numeric(mparams1[10:12])
    templateFD[i]<-sum(abs(newpt2-newpt1))
  }
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
if ( freqLo != freqHi )
  boldResidualsFiltered <- frequencyFilterfMRI( boldResiduals, tr = antsGetSpacing( boldImage )[4],
                           freqLo = frequencyLowThreshold, freqHi = frequencyHighThreshold, opt = "trig" ) else boldResidualsFiltered<-boldResiduals

DVARS<-rep(0,nrow(boldResidualsFiltered))
for ( i in 2:nrow(boldResidualsFiltered) ) {
    DVARS[i]<-sqrt( mean( ( boldResidualsFiltered[i,] - boldResidualsFiltered[i-1,] )^2 ) )
}
DVARS[1]<-mean(DVARS)

cleanBoldImage <- matrix2timeseries( boldImage, maskImage, boldResidualsFiltered )

# anisotropically smooth the images, if desired

if( spatialSmoothingParameter > 0.0 & spatialSmoothingNumberOfIterations > 0 )
  {
  ImageMath( 4, cleanBoldImage, "PeronaMalik", cleanBoldImage,
    spatialSmoothingParameter, spatialSmoothingNumberOfIterations )
  }

return( list( cleanBoldImage = cleanBoldImage, maskImage = maskImage, DVARS=DVARS, templateFD=templateFD ) )
}
