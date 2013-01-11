library( ANTsR )

################################################################################
#
# User variables to change
#
# Notes:
#   * the control and experimental images are expected to be found in \code{inputPath}
#   * Mask is assumed to be defined by background voxels = 0, foreground voxels != 0
#

dimensionality <- 2
inputPath <- "./example_images/"
controlsFileNamePrefix <- "phantomtemplate_CONTROL"
experimentalsFileNamePrefix <- "phantomtemplate_EXP"
maskFileName <- paste( inputPath, "phantomtemplate_mask.nii.gz", sep = '' )
outputPath <- "./test_output/"
outputPrefix <- "ANTsR_"

################################################################################

# Get the image files
controlsFileNames <- list.files( path = inputPath, pattern =
  glob2rx( paste( controlsFileNamePrefix, "*", sep = '' ) ), full.names = TRUE, recursive = FALSE )
experimentalsFileNames <- list.files( path = inputPath, pattern =
  glob2rx( paste( experimentalsFileNamePrefix, "*", sep='' ) ), full.names = TRUE, recursive = FALSE )

# Check to see if there are more than one image per group
numberOfControls <- length( controlsFileNames )
numberOfExperimentals <- length( experimentalsFileNames )

if( numberOfControls < 2 )
  {
  cat( "The number of control files is less than 2.\n", sep = "" )
  return;
  }
if( numberOfExperimentals < 2 )
  {
  cat( "The number of experimental files is less than 2.\n", sep = "" )
  return;
  }

cat( "******* Conducting simple voxel-based analysis (controls = ", numberOfControls,
  ", experimentals = ", numberOfExperimentals, "). *******\n\n", sep = '' )

# Read the mask and place the masked voxels in the images in a matrix

cat( "Reading mask file ", maskFileName, "\n\n", sep = '' )
mask <- antsImageRead( maskFileName, 'unsigned int', dimensionality )
numberOfForegroundVoxels <- sum( c( as.array( mask ) ) )

dataMatrix <- matrix( data = NA, nrow = numberOfControls+numberOfExperimentals,
  ncol = numberOfForegroundVoxels )

allFileNames <- c( controlsFileNames, experimentalsFileNames )
for( i in seq( 1, length( allFileNames ) ) )
  {
  if( i <= numberOfControls )
    {
    cat( "Reading control image ", allFileNames[i], " (", i, " of ",
      numberOfControls, ").\n", sep = '' )
    }
  else
    {
    cat( "Reading experimental image ", allFileNames[i], " (", i - numberOfControls,
      " of ", numberOfExperimentals, ").\n", sep = '' )
    }
  subjectImage <- antsImageRead( allFileNames[i], 'float', dimensionality )
  dataMatrix[i,] <- as.array( subjectImage[mask != 0] )
  }

# Perform the t-testing.  Monitor progress.
predictor <- c( rep( 1, numberOfControls ), rep( -1, numberOfExperimentals ) )
testFormula <- ( values ~ 1 + predictor )

tValues<-rep( NA, numberOfForegroundVoxels )
pValues<-rep( NA, numberOfForegroundVoxels )

cat( "\nTesting...\n" );

progress <- txtProgressBar( min = 0, max = numberOfForegroundVoxels, style = 3 )
for( i in 1:numberOfForegroundVoxels )
  {
  values <- dataMatrix[,i]
  results <- summary( lm( testFormula ) )
  tValues[i] <- results$coef[2,3]
  pValues[i] <- results$coef[2,4]
  if( i %% 100 )
    {
    setTxtProgressBar( progress, i )
    }
  }
close( progress );
cat( "Done.\n", sep = '' );

cat( "\nWriting output.\n" );

tImage <- antsImageClone( mask, "float" )
pImage <- antsImageClone( mask, "float" )
qImage <- antsImageClone( mask, "float" )

tImage[mask != 0] <- tValues
pImage[mask != 0] <- 1.0 - pValues
qImage[mask != 0] <- 1.0 - p.adjust( pValues, method = "fdr" );

dir.create( outputPath, showWarnings = TRUE, recursive = TRUE )

antsImageWrite( tImage, paste( outputPath, '/', outputPrefix, 'tValues.nii.gz', sep = '' ) )
antsImageWrite( pImage, paste( outputPath, '/', outputPrefix, '1minuspValues.nii.gz', sep = '' ) )
antsImageWrite( qImage, paste( outputPath, '/', outputPrefix, '1minuspValues_corrected.nii.gz', sep = '' ) )
