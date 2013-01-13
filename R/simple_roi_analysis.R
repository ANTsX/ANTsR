simple_roi_analysis <- function( dimensionality = 3, inputPath = "./", controlsFileNamePrefix = "CONTROL",
  experimentalsFileNamePrefix = "EXP", roiLabelsFileName = "" )
{

	# check if called with no arguments and print usage
	if( nchar( roiLabelsFileName ) == 0 )
	  {
		 print( "No ROI labels file name specified." ) ;
		 return;
  	}

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
if( !file.exists( roiLabelsFileName ) )
  {
  cat( "ROI labels file ", roiLabelsFileName, " does not exist.\n", sep = '' )
  }

cat( "******* Conducting simple ROI analysis (controls = ", numberOfControls,
  ", experimentals = ", numberOfExperimentals, "). *******\n\n", sep = '' )

# Read the mask and place the masked voxels in the images in a matrix

cat( "Reading ROI labels file ", roiLabelsFileName, "\n\n", sep = '' )
roiLabelsMask <- antsImageRead( roiLabelsFileName, 'unsigned int', dimensionality )
roiLabels <- sort( unique( c( as.array( roiLabelsMask ) ) ) )
roiLabels <- roiLabels[which( roiLabels != 0 )]

cat( "Unique ROI labels =", roiLabels, "\n\n", sep = ' ' )

numberOfForegroundVoxels <- length( roiLabelsMask[roiLabelsMask != 0] )

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
  dataMatrix[i,] <- as.array( subjectImage[roiLabelsMask != 0] )
  }

roiLabelsMask <- c( roiLabelsMask[roiLabelsMask != 0] );

# Perform the t-testing.  Monitor progress.
predictor <- c( rep( 1, numberOfControls ), rep( -1, numberOfExperimentals ) )
testFormula <- ( values ~ 1 + predictor )

tValues <- rep( NA, length( roiLabels ) )
pValues <- rep( NA, length( roiLabels ) )

cat( "\nTesting...\n" );
for( i in 1:length( roiLabels ) )
  {
  values <- rowMeans( dataMatrix[,which( roiLabelsMask == roiLabels[i] )], na.rm = TRUE );
  results <- summary( lm( testFormula ) )
  tValues[i] <- results$coef[2,3]
  pValues[i] <- results$coef[2,4]
  }
cat( "Done.\n", sep = '' );

return( list( roi.labels = roiLabels, t.values = tValues, p.values = pValues ) )
}
