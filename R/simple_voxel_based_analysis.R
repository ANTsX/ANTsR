simple_voxel_based_analysis <- function( dimensionality = 3, controlFileNames = c(),
  experimentalFileNames = c(), maskFileName = "", outputPrefix = "./ANTsR_" )
{

# Check to see if there are more than one image per group
numberOfControls <- length( controlFileNames )
numberOfExperimentals <- length( experimentalFileNames )

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
mask <- antsImageRead( maskFileName, dimensionality , pixeltype = 'unsigned int' )
numberOfForegroundVoxels <- sum( c( as.array( mask ) ) )

dataMatrix <- matrix( data = NA, nrow = numberOfControls+numberOfExperimentals,
  ncol = numberOfForegroundVoxels )

allFileNames <- c( controlFileNames, experimentalFileNames )
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
  subjectImage <- antsImageRead( allFileNames[i], dimensionality )
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
  if( i %% 50 == 0 )
    {
    setTxtProgressBar( progress, i )
    }
  }
close( progress );
cat( "Done.\n", sep = '' )

cat( "\nWriting output.\n" )

tImage <- antsImageClone( mask, "float" )
pImage <- antsImageClone( mask, "float" )
qImage <- antsImageClone( mask, "float" )

tImage[mask != 0] <- tValues
pImage[mask != 0] <- 1.0 - pValues
qImage[mask != 0] <- 1.0 - p.adjust( pValues, method = "fdr" )

outputPath <- dirname( outputPrefix )
filePrefix <- basename( outputPrefix )

dir.create( outputPath, showWarnings = TRUE, recursive = TRUE )

antsImageWrite( tImage, paste( outputPath, '/', filePrefix, 'tValues.nii.gz', sep = '' ) )
antsImageWrite( pImage, paste( outputPath, '/', filePrefix, '1minuspValues.nii.gz', sep = '' ) )
antsImageWrite( qImage, paste( outputPath, '/', filePrefix, '1minuspValues_corrected.nii.gz', sep = '' ) )
}
