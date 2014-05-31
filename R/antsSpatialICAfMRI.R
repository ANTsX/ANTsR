library( fastICA )

antsSpatialICAfMRI <- function( boldImages, maskImage = NA, numberOfICAComponents = 40,
                                frequencyLowThreshold = 0.01, frequencyHighThreshold = 0.1,
                                normalizeComponentImages = TRUE )
{

numberOfBoldImages <- length( boldImages )

# We calculate the mean and max framewise displacements for
# each bold image.  These values can then be inspected for
# quality control purposes.

maxFramewiseDisplacements <- rep( 0, numberOfBoldImages )
meanFramewiseDisplacements <- rep( 0, numberOfBoldImages )
maxDVARS <- rep( 0, numberOfBoldImages )
meanDVARS <- rep( 0, numberOfBoldImages )

# Process each bold image.  Group ICA is performed by concatenating the time series of
# the bold images (similar to MELODIC---http://fsl.fmrib.ox.ac.uk/fsl/fslwiki/MELODIC).
# Other possible group approaches are described in http://www.ncbi.nlm.nih.gov/pubmed/19059344

for( i in 1:numberOfBoldImages )
  {
  cat( "Processing bold image ", i, " (out of ", numberOfBoldImages, ").\n", sep = "" )

  fmri <- antsPreprocessfMRI( boldImages[[i]],  maskImage = maskImage,
                              useMotionCorrectedImage = TRUE,
                              frequencyLowThreshold = frequencyLowThreshold,
                              frequencyHighThreshold = frequencyHighThreshold )
  maxDVARS[i] <- max( fmri$DVARS )
  meanDVARS[i] <- mean( fmri$DVARS )
  maxFramewiseDisplacements[i] <- max( fmri$FD )
  meanFramewiseDisplacements[i] <- mean( fmri$FD )

  if( i == 1 )
    {
    groupBoldMatrix <- timeseries2matrix( fmri$cleanBoldImage, mask )
    }
  else
    {
    groupBoldMatrix <- rbind( groupBoldMatrix, timeseries2matrix( fmri$cleanBoldImage, mask ) )
    }
  }

# Preprocessing in the fastICA algorithm includes centering and whitening

icaResults <- fastICA( X = t( groupBoldMatrix ), n.comp = numberOfICAComponents,
        alg.typ = c( "parallel" ),
        fun = c( "logcosh" ), alpha = 1.0, method = c( "C" ),
        row.norm = FALSE, maxit = 200, tol = 1e-04, verbose = TRUE,
        w.init = NULL )

# create componentImages

componentImages <- list()
for( i in 1:numberOfICAComponents )
  {
  componentVector <- icaResults$S[, i]
  if( normalizeComponentImages )
    {
    componentVector <- scale( componentVector )
    }
  componentImages[[i]] <- antsImageClone( mask, "float" )
  componentImages[[i]][mask != 0] <- componentVector
  }

#    standard ICA output items from the fastICA algorithm
#
# X	= pre-processed data matrix
# K = pre-whitening matrix that projects data onto the first n.comp principal components.
# W	= estimated un-mixing matrix (see definition in details)
# A	= estimated mixing matrix
# S	= estimated source matrix

# PCA components (whitened matrix) = X %*% K
# ICA components = S = X %*% K %*% W (the ICA algorithm attempts to find the unmixing
#   matrix, W)

return( list( X = icaResults$X, K = icaResults$K, W = icaResults$W,
              A = icaResults$A, S = icaResults$S,
              componentImages = componentImages, maskImage = maskImage,
              maxDVARS = maxDVARS, meanDVARS = meanDVARS,
              maxFD = maxFramewiseDisplacements, meanFD = meanFramewiseDisplacements ) )
}

