#' Calculate blood perfusion using deconvolution.
#'
#' Implementation of the deconvolution technique of Ostergaard et al.
#' (http://www.ncbi.nlm.nih.gov/pubmed/8916023) for calculating cerebral (or pulmonary)
#' blood flow.  Other relevant references include http://www.ncbi.nlm.nih.gov/pubmed/16261573,
#' http://www.ncbi.nlm.nih.gov/pubmed/8916023, and http://www.ncbi.nlm.nih.gov/pubmed/15332240.
#'
#' @param perfusionImage time series (n-D + time) perfusion acquisition.
#' @param voiMaskImage n-D mask image indicating where the cerebral blood flow parameter
#'        images are calculated.
#' @param aifMaskImage n-D mask image indicating where the arterial input function is
#'        calculated.
#' @param thresholdSVD is used to threshold the smaller elements of the diagonal matrix
#'        during the SVD regularization.  0.2 is a common choice (cf. page 571, end of
#'        column 2 in http://www.ncbi.nlm.nih.gov/pubmed/16971140).
#' @param deltaTime time between volumetric acquisitions.  We assume a uniform time sampling.
#'
#' @return list with the cerebral blood flow image (cbfImage), cerebral blood volume image
#'         (cbvImage), mean transit time (mttImage), and arterial input function signal
#'         from the image (aifSignal) and the calculated arterial input function
#'         concentration (aifConcentration).
#'
#' @author Tustison NJ
#'
#' @examples
#' \dontrun{
#'
#' perfusionImage <- antsImageRead( filename = perfusionFileName, dimension = 4, pixeltype = 'float' )
#' voiMaskImage <- antsImageRead( filename = voiMaskFileName, dimension = 3, pixeltype = 'unsigned int' )
#' aifMaskImage <- antsImageRead( filename = aifMaskFileName, dimension = 3, pixeltype = 'unsigned int' )
#'
#'
#' deltaTime <- 3.4
#'
#' results <- bloodPerfusionSVD( perfusionImage, voiMaskImage, aifMaskImage,
#'                              thresholdSVD = 0.2, deltaTime = deltaTime )
#'
#' antsImageWrite( results$cbfImage, paste0( 'cbf.nii.gz' ) );
#' antsImageWrite( results$cbvImage, paste0( 'cpbv.nii.gz' ) );
#' antsImageWrite( results$mttImage, paste0( 'mtt.nii.gz' ) );
#'
#' }
bloodPerfusionSVD <- function( perfusionImage, voiMaskImage, aifMaskImage,
  thresholdSVD = 0.2, deltaTime = 1.0 )
{

if( missing( perfusionImage ) )
  {
  stop( "Error:  The perfusion image is not specified.\n" )
  }

if( missing( voiMaskImage ) )
  {
  stop( "Error:  The VOI mask image is not specified.\n" )
  }

if( missing( aifMaskImage ) )
  {
  stop( "Error:  The arterial input function mask image is not specified.\n" )
  }

# The AIF signal is defined as the mean intensity value over the AIF region of interest
# at each time point

aifMatrix <- timeseries2matrix( perfusionImage, aifMaskImage )
Saif <- rowMeans( aifMatrix, na.rm = TRUE )

# Automatically find start of the AIF signal by finding the point at which the signal
# intensity curve exceeds 10% of the maximum

SaifMaxIndex = which( Saif == max( Saif ) )
SaifStartIndex = tail( which( Saif[1:SaifMaxIndex] < 0.1 * ( max( Saif ) - min( Saif ) ) ), n = 1 ) + 1

S0aif <- mean( Saif[1:( SaifStartIndex - 1 )], na.rm = TRUE )

# See http://www.ncbi.nlm.nih.gov/pubmed/16261573, page 711, equation (3).  Note that
# we exclude the constant of proportionality, k, and the TE parameters as they cancel
# out in estimating the residue function.

Caif <- -log( Saif / S0aif )

# If we can estimate the product CBF * R(t) we obtain an estimate for CBF because R(0) = 1.
# CBF * dT * R = ( V * L^-1 * U^T ) * Cvoi (cf http://www.ncbi.nlm.nih.gov/pubmed/8916023,
# page 713, equation (9)).

dSVD <- deconvolutionSVD( Caif, thresholdSVD )

# Measured signal over the entire region of interest

Svoi <- timeseries2matrix( perfusionImage, voiMaskImage )
numberOfTimePoints <- nrow( Svoi )

# Baseline signal

S0voi <- colMeans( Svoi[1:SaifStartIndex,], na.rm = TRUE )
S0voi <- matrix( rep( S0voi, numberOfTimePoints ), nrow = numberOfTimePoints, byrow = TRUE )

# See http://www.ncbi.nlm.nih.gov/pubmed/16261573, page 711, equation (3).  Note that
# we exclude the constant of proportionality, k, and the TE parameters as they cancel
# out in estimating the residue function.

Cvoi <- -log( Svoi / S0voi )

residueFunction <- dSVD %*% Cvoi / deltaTime
residueFunction[residueFunction < 0.0] <- 0.0

# cbf is the maximum of the residue function at each voxel

.colMax <- function( data ) apply( data, 2, max, na.rm = TRUE )
cbf <- .colMax( residueFunction )
cbfOutputImage <- matrixToImages( as.matrix( t( cbf ) ), antsImageClone( voiMaskImage, 'float' ) )[[1]]

# cbv is area under the curve at each voxel using the trapezoidal rule

cbv <- trapz( seq( from = 0.0, by = deltaTime, length.out = nrow( residueFunction ) ), residueFunction )
cbvOutputImage <- matrixToImages( as.matrix( cbv ), antsImageClone( voiMaskImage, 'float' ) )[[1]]

mtt <- cbv / cbf
mtt[which( is.na( mtt ) )] <- 0.0
mttOutputImage <- matrixToImages( as.matrix( mtt ), antsImageClone( voiMaskImage, 'float' ) )[[1]]

return( list( cbfImage = cbfOutputImage,
              cbvImage = cbvOutputImage,
              mttImage = mttOutputImage,
              aifSignal = Saif,
              aifConcentration = Caif ) )
}

#' Calculate the area under a sampled curve (or set of curves).
#'
#' Given a vector (or a matrix) representing a curve (or set of curves, columnwise),
#' the area (or set of areas) is calculated using the trapezoidal rule.
#'
#' @param x vector of samples for the dependent variable.
#' @param y vector or matrix of samples for the independent variable.  In the case of the
#'        latter, curves are organized column-wise.
#'
#' @return area (areas) under the sampled curve (curves).
#'
#' @author Tustison NJ
#'
#' @examples
#'
#' x <- seq( 0, 1, by = 0.0001 )
#' y <- exp( x )
#'
#' # Compare with true area of exp( 1 ) - 1 = 1.718282...
#' areaEstimate <- trapz( x, y )
#'

trapz <- function( x, y )
{
idx = 2:length( x )
if( is.vector( y ) )
  {
  return ( 0.5 * ( ( x[idx] - x[idx-1] ) %*% ( y[idx] + y[idx-1] ) ) )
  } else {
  return ( 0.5 * ( ( x[idx] - x[idx-1] ) %*% ( y[idx,] + y[idx-1,] ) ) )
  }
}

#' Regularize SVD (deconvolution) solution of cerebral blood flow.
#'
#' Ostergaard's regularization approach for a model independent solution of cerebral
#' blood flow using a blood pool contrast agent (http://www.ncbi.nlm.nih.gov/pubmed/8916023).
#'
#' @param arterialInputFunction vector specifying the arterial input function over time.
#' @param thresholdSVD is used to threshold the smaller elements of the diagonal matrix
#'        during the SVD regularization.  0.2 is a common choice (cf. page 571, end of
#'        column 2 in http://www.ncbi.nlm.nih.gov/pubmed/16971140).
#'
#' @return regularized residue function, i.e. the right-hand side of
#'         CBF * dT * R = ( V * L^-1 * U^T ) * Cvoi.
#'
#' @author Tustison NJ
#'
#' @examples
#'
#' S0aif <- 17.62
#' Saif <- c( 16.25, 16.37, 20.22, 78.96, 230.5, 249.79, 198.58, 147.76, 110.39, 111.64, 129.43 )
#' Caif <- -log( Saif / S0aif )
#'
#' dSVD <- deconvolutionSVD( Caif, 0.2 )
#'

deconvolutionSVD <- function( arterialInputFunction, thresholdSVD = 0.2 )
{
  if( ! is.vector( arterialInputFunction ) )
    {
    stop( "Expecting a vector." )
    }

  N <- length( arterialInputFunction )
  Caif <- mat.or.vec( N, N )

  for( j in 1:N )
    {
    Caif[j:N,j] <- arterialInputFunction[1:(N-j+1)]
    }
  S <- svd( Caif )
  Dinv <- 1.0 / S$d

  Dinv[which( S$d < thresholdSVD * max( S$d ) )] <- 0.0

  dSVD <- S$v %*% diag( Dinv ) %*% t( S$u )
}
