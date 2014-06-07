library( fastICA )

antsSpatialICAfMRI <- function( boldImages, numberOfICAComponents = 20
                                normalizeComponentImages = TRUE )
{

numberOfBoldImages <- length( boldImages )

# Group ICA is performed by concatenating the time series of the bold images
# (similar to MELODIC---http://fsl.fmrib.ox.ac.uk/fsl/fslwiki/MELODIC).
# Other possible group approaches are described in
# http://www.ncbi.nlm.nih.gov/pubmed/19059344

for( i in 1:numberOfBoldImages )
  {
  # if there's only 1 bold image, then it will be whitened in the fastICA function call

  subjectBoldMatrix <- timeseries2matrix( boldImages[[i]], mask )
  if( numberOfBoldImages > 1 )
    {
    subjectBoldMatrix <- t( icawhiten( t( subjectBoldMatrix ), numberOfICAComponents ) )
    }

  if( i == 1 )
    {
    groupBoldMatrix <- subjectBoldMatrix
    }
  else
    {
    groupBoldMatrix <- rbind( groupBoldMatrix, subjectBoldMatrix )
    }
  }

# taken from the fastICA package

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
              componentImages = componentImages )
}

