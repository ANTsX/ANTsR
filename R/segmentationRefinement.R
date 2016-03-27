#' Segmentation refinement using corrective learning (training)
#'
#' A random forest implementation of the corrective learning wrapper introduced
#' in Wang, et al., Neuroimage 2011 (http://www.ncbi.nlm.nih.gov/pubmed/21237273).
#' The training process involves building two sets of models from training data
#' for each label in the initial segmentation data.  The two sets of models are
#' for the two stages of refinement:
#'     Stage 1:  Learn potentially misclassified voxels per label
#'     Stage 2:  Differentiate background/foreground voxels per label
#' It is important to note that building the models for the two stages can be
#' done in parallel.
#'
#' @param featureImages a list of lists of feature images.  Each list of feature images
#'        corresponds to a single subject.  Possibilities are outlined in the above-cited
#'        paper.
#' @param truthLabelImages a list of "ground-truth" segmentation images, one for each
#'        set of feature images.
#' @param featureImageNames a vector of character strings naming the set of features.
#'        This parameter is optional but does help in investigating the relative
#'        importance of specific features.
#' @param labelSet a vector specifying the labels of interest.  If not specified,
#'         the full set is determined from the truthLabelImages.
#' @param maximumNumberOfSamplesPerClass specified the maximum number of samples
#'        used to build the model for each element of the labelSet.
#' @param dilationRadius specifies the dilation radius for determining the ROI for
#'        each label.
#' @param neighborhoodRadius specifies which voxel neighbors should be included in
#'        building the model.  The user can specify a scalar or vector.
#' @param whichStage Stage1:  voxel misclassification.  Stage2:  background/foreground
#' @param useLabelDistances if TRUE, Euclidean distance maps for each label are
#'        created and added to the list of featureImages.  This feature image type
#'        was recommended in Wang et al.
#' @param normalizeSamplesPerLabel if TRUE, the samples from each ROI are normalized
#'        by the mean of the voxels in that ROI.
#'
#' @return list with the models per label (LabelModels), the label set (LabelSet), and
#'         the feature image names (FeatureImageNames).
#'
#' @author Tustison NJ
#'
#' @examples
#' \dontrun{
#'
#'  library( ANTsR )
#'  library( ggplot2 )
#'
#'  imageIDs <- c( "r16", "r27", "r30", "r62", "r64", "r85" )
#'
#'  # Perform simple 3-tissue segmentation.  For convenience we are going to use
#'  # atropos segmentation to define the "ground-truth" segmentations and the kmeans
#'  # to define the segmentation we want to "correct".  We collect feature images for
#'  # each image.  The gradient and laplacian images chosen below as feature images
#'  # are simply selected for convenience.
#'
#'  segmentationLabels <- c( 1, 2, 3 )
#'
#'  featureImageNames <- c( 'T1', 'Gradient', 'Laplacian' )
#'
#'  images <- list()
#'  kmeansSegs <- list()
#'  atroposSegs <- list()
#'  featureImages <- list()
#'
#'  for( i in 1:length( imageIDs ) )
#'    {
#'    cat( "Processing image", imageIDs[i], "\n" )
#'    images[[i]] <- antsImageRead( getANTsRData( imageIDs[i] ) )
#'    mask <- getMask( images[[i]] )
#'    kmeansSegs[[i]] <- kmeansSegmentation( images[[i]], length( segmentationLabels ), mask, mrf = 0.0 )$segmentation
#'    atroposSegs[[i]] <- atropos( images[[i]], mask, i = "KMeans[3]", m = "[0.25,1x1]", c = "[5,0]" )$segmentation
#'
#'    featureImageSetPerImage <- list()
#'    featureImageSetPerImage[[1]] <- images[[i]]
#'    featureImageSetPerImage[[2]] <- iMath( images[[i]], "Grad", 1.0 )
#'    featureImageSetPerImage[[3]] <- iMath( images[[i]], "Laplacian", 1.0 )
#'    featureImages[[i]] <- featureImageSetPerImage
#'    }
#'
#'  # Perform training for both stages (can be performed simultaneously).  We train
#'  # on images "r27", "r30", "r62", "r64", "r85" and test/predict on image "r16".
#'
#'  cat( "\nTraining Stage 1\n\n" )
#'
#'  segRefineStage1 <- segmentationRefinement.train( featureImages = featureImages[2:6],
#'    truthLabelImages = atroposSegs[2:6], segmentationImages = kmeansSegs[2:6],
#'    featureImageNames = featureImageNames, labelSet = segmentationLabels,
#'    maximumNumberOfSamplesPerClass = 100, dilationRadius = 1,
#'    neighborhoodRadius = 0, whichStage = 1,
#'    useLabelDistances = TRUE, normalizeSamplesPerLabel = TRUE )
#'
#'  cat( "\nTraining Stage 2\n\n" )
#'
#'  segRefineStage2 <- segmentationRefinement.train( featureImages = featureImages[2:6],
#'    truthLabelImages = atroposSegs[2:6], segmentationImages = kmeansSegs[2:6],
#'    featureImageNames = featureImageNames, labelSet = segmentationLabels,
#'    maximumNumberOfSamplesPerClass = 100, dilationRadius = 1,
#'    neighborhoodRadius = c( 1, 1 ), whichStage = 2,
#'    useLabelDistances = TRUE, normalizeSamplesPerLabel = TRUE )
#'
#'  cat( "\nGenerating importance plots for Stage 2.\n\n" )
#'
#'  for( m in 1:length( segRefineStage2$LabelModels ) )
#'    {
#'    forestImp <- importance( segRefineStage2$LabelModels[[m]], type = 1 )
#'    forestImp.df <- data.frame( Statistic = names( forestImp[,1] ), Importance = as.numeric( forestImp[,1] )  )
#'    forestImp.df <- forestImp.df[order( forestImp.df$Importance ),]
#'
#'    forestImp.df$Statistic <- factor( x = forestImp.df$Statistic, levels = forestImp.df$Statistic )
#'
#'    iPlot <- ggplot( data = forestImp.df, aes( x = Importance, y = Statistic ) ) +
#'             geom_point( aes( color = Importance ) ) +
#'             labs( title = paste0( 'Label ', segRefineStage2$LabelSet[m] ) ) +
#'             ylab( "" ) +
#'             xlab( "MeanDecreaseAccuracy" ) +
#'             scale_color_continuous( low = "navyblue", high = "darkred" ) +
#'             theme( axis.text.y = element_text( size = 10 ) ) +
#'             theme( plot.margin = unit( c( 0.1, 0.1, 0.1, -0.5 ), "cm" ) ) +
#'             theme( legend.position = "none" )
#'    ggsave( file = paste0( 'importancePlotsStage2Label', segRefineStage2$LabelSet[m], '.pdf' ), plot = iPlot, width = 4, height = 6 )
#'    }
#'
#' }

segmentationRefinement.train <- function( featureImages, truthLabelImages,
  segmentationImages, featureImageNames = c(), labelSet = c(),
  maximumNumberOfSamplesPerClass = 500, dilationRadius = 2,
  neighborhoodRadius = 0, whichStage = c( '1', '2' ),
  useLabelDistances = TRUE, normalizeSamplesPerLabel = TRUE )
{

# check inputs

if ( ! usePkg( "randomForest" ) )
  {
  stop( "Please install the randomForest package." )
  }

totalNumberOfSubjects <- length( truthLabelImages )

if( length( featureImages ) != totalNumberOfSubjects )
  {
  stop( "The number of feature image sets does not equal the number of truth label images." );
  }

if( length( segmentationImages ) != totalNumberOfSubjects )
  {
  stop( "The number of segmentation image sets does not equal the number of truth label images." );
  }

dimension <- truthLabelImages[[1]]@dimension

if( length( neighborhoodRadius ) != dimension )
  {
  neighborhoodRadius <- rep( neighborhoodRadius, dimension )
  }
numberOfNeighborhoodVoxels <- 1
for( i in 1:dimension )
  {
  numberOfNeighborhoodVoxels <- numberOfNeighborhoodVoxels * ( 2 * neighborhoodRadius[i] + 1 )
  }

if( length( featureImageNames ) == 0 )
  {
  for( i in 1:length( featureImages[[1]][[1]] ) )
    {
    featureImageNames <- append( featureImageNames, paste0( "FeatureImage.", i ) );
    }
  }


if( useLabelDistances )
  {
  featureImageNames <- append( featureImageNames, "LabelDistance" )
  }

featureNeighborhoodNames <- c()
for( i in 1:length( featureImageNames ) )
  {
  for( j in 1:numberOfNeighborhoodVoxels )
    {
    featureName <- paste0( featureImageNames[i], 'Neighbor', j )
    featureNeighborhoodNames <- append( featureNeighborhoodNames, featureName )
    }
  }

# Get the unique label set over the entire set of truth label images

if( length( labelSet ) == 0 )
  {
  for( i in 1:totalNumberOfSubjects )
    {
    truthLabelArray <- as.array( truthLabelImages[[i]] )
    labelSetPerSubject <- unique( truthLabelArray )
    labelSet <- unique( append( labelSetPerSubject, labelSet ) )
    }
  }
labelSet <- sort( labelSet )

## Create the models per label

labelModels <- list()

for( l in 1:length( labelSet ) )
  {
  label <- labelSet[l]

  message( "Doing label ", label );

  modelDataPerLabel <- matrix()
  for( i in 1:totalNumberOfSubjects )
    {
    message( "  Sampling data from subject ", i )

    # Get the ROI mask from the segmentation image for the current label.  Within that ROI,
    # find which voxels are mislabeled.  Within that ROI mask, the mislabeled voxels are
    # given the '1' label while correctly labeled voxels are given the '2' label.  These
    # mislabeled and correctly labeled voxels are given in the variable "mislabeledVoxelsMaskArray"

    segmentationSingleLabelImage <- thresholdImage( antsImageClone( segmentationImages[[i]], 'float' ), label, label, 1, 0 )
    segmentationSingleLabelArray <- as.array( segmentationSingleLabelImage )

    maskImage <- iMath( segmentationSingleLabelImage, "MD", dilationRadius )
    maskArray <- as.array( maskImage )

    if( length( which( maskArray != 0 ) ) == 0 )
      {
      warning( "Warning:  No voxels exist for label ", label, " of subject ", i )
      next
      }

    if( useLabelDistances )
      {
      distanceSingleLabelImage <- iMath( segmentationSingleLabelImage, 'MaurerDistance' )
      featureImages[[i]][[length( featureImageNames )]] <- distanceSingleLabelImage
      }

    truthSingleLabelImage <- thresholdImage( antsImageClone( truthLabelImages[[i]], 'float' ), label, label, 1, 0 )
    truthSingleLabelArray <- as.array( truthSingleLabelImage )

    binaryLabelSet <- c( 1, 2 )

    mislabeledVoxelsMaskArray <- ( truthSingleLabelArray - segmentationSingleLabelArray )
    mislabeledVoxelsMaskArray[which( mislabeledVoxelsMaskArray != 0 )] <- binaryLabelSet[1]
    mislabeledVoxelsMaskArray[which( mislabeledVoxelsMaskArray == 0 )] <- binaryLabelSet[2]
    mislabeledVoxelsMaskArray <- mislabeledVoxelsMaskArray * maskArray

    if( whichStage == 1 )
      {

      binaryMaskArray <- maskArray
      binaryTruthArray <- mislabeledVoxelsMaskArray

      } else {

      whichGroundMaskArray <- rep( 0, length( maskArray ) )
      whichGroundMaskArray[mislabeledVoxelsMaskArray == binaryLabelSet[1] & truthSingleLabelArray == 0] <- binaryLabelSet[1]
      whichGroundMaskArray[mislabeledVoxelsMaskArray == binaryLabelSet[1] & truthSingleLabelArray == 1] <- binaryLabelSet[2]

      binaryMaskArray <- rep( 0, length( maskArray ) )
      binaryMaskArray[mislabeledVoxelsMaskArray == binaryLabelSet[1]] <- 1
      binaryTruthArray <- whichGroundMaskArray

      }

    # Ensure that the samples per label are balanced in each subject
    minimumNumberOfSamplesInSubjectData <- maximumNumberOfSamplesPerClass
    for( n in 1:length( binaryLabelSet ) )
      {
      labelIndices <- which( binaryTruthArray == binaryLabelSet[n] & binaryMaskArray == 1 )
      numberOfLabelIndices <- length( labelIndices )
      message( "    Number of label indices (label ", binaryLabelSet[n], ") = ", numberOfLabelIndices )
      if( numberOfLabelIndices < minimumNumberOfSamplesInSubjectData )
        {
        minimumNumberOfSamplesInSubjectData <- numberOfLabelIndices
        }
      }
    message( "    Number of samples per label = ",  minimumNumberOfSamplesInSubjectData )

    truthLabelIndices <- list()
    numberOfSamplesPerLabelInSubjectData <- rep( 0, length( binaryLabelSet ) )
    for( n in 1:length( binaryLabelSet ) )
      {
      labelIndices <- which( binaryTruthArray == binaryLabelSet[n] & binaryMaskArray == 1 )
      numberOfLabelIndices <- length( labelIndices )

      numberOfSamplesPerLabelInSubjectData[n] <- min( minimumNumberOfSamplesInSubjectData, numberOfLabelIndices )
      if( numberOfLabelIndices > 0 )
        {
        truthLabelIndices[[n]] <- labelIndices[sample.int( numberOfLabelIndices, numberOfSamplesPerLabelInSubjectData[n], replace = FALSE )]
        }
      }

    # read in the voxel values (including neighbors)

    wholeMaskImage <- segmentationSingleLabelArray
    wholeMaskImage[which( wholeMaskImage != 1 )] <- 1
    wholeMaskImage <- as.antsImage( wholeMaskImage )

    subjectDataPerLabel <- matrix( NA, nrow = sum( numberOfSamplesPerLabelInSubjectData ), ncol = length( featureImageNames ) * numberOfNeighborhoodVoxels + 1 )
    for( j in 1:length( featureImageNames ) )
      {
      featureImageNeighborhoodValues <- getNeighborhoodInMask( featureImages[[i]][[j]], wholeMaskImage, neighborhoodRadius, boundary.condition = "image" )

      for( n in 1:length( binaryLabelSet ) )
        {
        if( numberOfSamplesPerLabelInSubjectData[n] == 0 )
          {
          next
          }

        startIndex <- 1
        if( n > 1 )
          {
          startIndex <- sum( numberOfSamplesPerLabelInSubjectData[1:(n-1)] ) + 1
          }
        endIndex <- startIndex + length( truthLabelIndices[[n]] ) - 1

        values <- featureImageNeighborhoodValues[,truthLabelIndices[[n]]]
        if( normalizeSamplesPerLabel )
          {
          featureImagesArray <- as.array( featureImages[[i]][[j]] )
          meanValue <- mean( featureImagesArray[which( segmentationSingleLabelArray != 0 )], na.rm = TRUE )
          if( meanValue != 0 )
            {
            values <- values / meanValue
            }
          }

        subjectDataPerLabel[startIndex:endIndex, ( ( j - 1 ) * numberOfNeighborhoodVoxels + 1 ):( j * numberOfNeighborhoodVoxels )] <- t( values )

        if( j == 1 )
          {
          subjectDataPerLabel[startIndex:endIndex, length( featureImageNames ) * numberOfNeighborhoodVoxels + 1] <- rep.int( binaryLabelSet[n], length( truthLabelIndices[[n]] ) )
          }
        }
      }
    if( i == 1 )
      {
      modelDataPerLabel <- subjectDataPerLabel
      }
    else
      {
      modelDataPerLabel <- rbind( modelDataPerLabel, subjectDataPerLabel )
      }
    }

  colnames( modelDataPerLabel ) <- c( featureNeighborhoodNames, "Labels" )
  modelDataPerLabel <- as.data.frame( modelDataPerLabel )
  modelDataPerLabel$Labels <- as.factor( modelDataPerLabel$Labels )

  ## Create the random forest model

  message( "  \nCreating the RF model for label ", label, ".  ", sep = "" )

  modelFormula <- as.formula( "Labels ~ . " )

  # Start the clock
  ptm <- proc.time()

  modelForest <- randomForest( modelFormula, modelDataPerLabel, ntree = 1000,
    type = classification, importance = TRUE, na.action = na.omit )

  # Stop the clock
  elapsedTime <- proc.time() - ptm
  message( "  Done (", as.numeric( elapsedTime[3] ), " seconds).\n", sep = "" )

  labelModels[[l]] <- modelForest;
  }

return ( list( LabelModels = labelModels, LabelSet = labelSet, FeatureImageNames = featureImageNames ) )
}

#' Segmentation refinement using corrective learning (prediction)
#'
#' A random forest implementation of the corrective learning wrapper introduced
#' in Wang, et al., Neuroimage 2011 (http://www.ncbi.nlm.nih.gov/pubmed/21237273).
#' The prediction process involves using the two-stage training models (per label)
#' to refine an initial segmentation.  The two stages of refinement are:
#'     Stage 1:  Predict potentially misclassified voxels per label
#'     Stage 2:  Predict background/foreground voxels per label
#'
#' @param segmentationImage image to refine via corrective learning.
#' @param labelSet a vector specifying the labels of interest.  Must be specified.
#' @param labelModelsStage1 a list of models for stage 1 (prediction of misclassifed voxels).
#'        Each element of the labelSet requires a model.
#' @param labelModelsStage1 a list of models for stage 2 (prediction of background/foreground).
#'        Each element of the labelSet requires a model.
#' @param featureImagesStage1 a list of lists of feature images for Stage 1.  Each list of
#'        feature images corresponds to a single subject.  Possibilities are outlined in
#'        the above-cited paper.
#' @param featureImagesStage2 a list of lists of feature images for Stage 2.  Each list of
#'        feature images corresponds to a single subject.  Possibilities are outlined in
#'        the above-cited paper.
#' @param featureImageNamesStage1 a vector of character strings naming the set of features.
#'        Must be specified.
#' @param featureImageNamesStage2 a vector of character strings naming the set of features.
#'        Must be specified.
#' @param dilationRadius specifies the dilation radius for determining the ROI for
#'        each label.
#' @param neighborhoodRadiusStage1 specifies which voxel neighbors should be included in
#'        prediction for Stage 1.  The user can specify a scalar or vector but it must match
#'        with what was used for training.
#' @param neighborhoodRadiusStage2 specifies which voxel neighbors should be included in
#'        prediction for Stage 2.  The user can specify a scalar or vector but it must match
#'        with what was used for training.
#' @param useLabelDistances if TRUE, Euclidean distance maps for each label are
#'        created and added to the list of featureImages.  This feature image type
#'        was recommended in Wang et al.
#' @param normalizeSamplesPerLabel if TRUE, the samples from each ROI are normalized
#'        by the mean of the voxels in that ROI
#'
#' @return a list consisting of the refined segmentation estimate (RefinedSegmentationImage) 
#'         and a list of the foreground probability images (ForegroundProbabilityImages).
#'
#' @author Tustison NJ
#'
#' @examples
#' \dontrun{
#'
#' \dontrun{
#'
#'  library( ANTsR )
#'  library( ggplot2 )
#'
#'  imageIDs <- c( "r16", "r27", "r30", "r62", "r64", "r85" )
#'
#'  # Perform simple 3-tissue segmentation.  For convenience we are going to use
#'  # atropos segmentation to define the "ground-truth" segmentations and the kmeans
#'  # to define the segmentation we want to "correct".  We collect feature images for
#'  # each image.  The gradient and laplacian images chosen below as feature images
#'  # are simply selected for convenience.
#'
#'  segmentationLabels <- c( 1, 2, 3 )
#'
#'  featureImageNames <- c( 'T1', 'Gradient', 'Laplacian' )
#'
#'  images <- list()
#'  kmeansSegs <- list()
#'  atroposSegs <- list()
#'  featureImages <- list()
#'
#'  for( i in 1:length( imageIDs ) )
#'    {
#'    cat( "Processing image", imageIDs[i], "\n" )
#'    images[[i]] <- antsImageRead( getANTsRData( imageIDs[i] ) )
#'    mask <- getMask( images[[i]] )
#'    kmeansSegs[[i]] <- kmeansSegmentation( images[[i]], length( segmentationLabels ), mask, mrf = 0.0 )$segmentation
#'    atroposSegs[[i]] <- atropos( images[[i]], mask, i = "KMeans[3]", m = "[0.25,1x1]", c = "[5,0]" )$segmentation
#'
#'    featureImageSetPerImage <- list()
#'    featureImageSetPerImage[[1]] <- images[[i]]
#'    featureImageSetPerImage[[2]] <- iMath( images[[i]], "Grad", 1.0 )
#'    featureImageSetPerImage[[3]] <- iMath( images[[i]], "Laplacian", 1.0 )
#'    featureImages[[i]] <- featureImageSetPerImage
#'    }
#'
#'  # Perform training for both stages (can be performed simultaneously).  We train
#'  # on images "r27", "r30", "r62", "r64", "r85" and test/predict on image "r16".
#'
#'  cat( "\nTraining Stage 1\n\n" )
#'
#'  segRefineStage1 <- segmentationRefinement.train( featureImages = featureImages[2:6],
#'    truthLabelImages = atroposSegs[2:6], segmentationImages = kmeansSegs[2:6],
#'    featureImageNames = featureImageNames, labelSet = segmentationLabels,
#'    maximumNumberOfSamplesPerClass = 100, dilationRadius = 1,
#'    neighborhoodRadius = 0, whichStage = 1,
#'    useLabelDistances = TRUE, normalizeSamplesPerLabel = TRUE )
#'
#'  cat( "\nTraining Stage 2\n\n" )
#'
#'  segRefineStage2 <- segmentationRefinement.train( featureImages = featureImages[2:6],
#'    truthLabelImages = atroposSegs[2:6], segmentationImages = kmeansSegs[2:6],
#'    featureImageNames = featureImageNames, labelSet = segmentationLabels,
#'    maximumNumberOfSamplesPerClass = 100, dilationRadius = 1,
#'    neighborhoodRadius = c( 1, 1 ), whichStage = 2,
#'    useLabelDistances = TRUE, normalizeSamplesPerLabel = TRUE )
#'
#'  cat( "\nPrediction\n\n" )
#'
#'  refinement <- segmentationRefinement.predict(
#'    segmentationImage = kmeansSegs[[1]], labelSet = segmentationLabels,
#'    segRefineStage1$LabelModels, featureImages[[1]], featureImageNames,
#'    segRefineStage2$LabelModels, featureImages[[1]], featureImageNames,
#'    dilationRadius = 1, neighborhoodRadiusStage1 = 0, neighborhoodRadiusStage2 = c( 1, 1 ),
#'    useLabelDistances = TRUE, normalizeSamplesPerLabel = TRUE )
#'
#'  # Compare "ground truth" = atroposSegs[[1]] with refinement$RefinedSegmentationImage
#'
#'   antsImageWrite( refinement$RefinedSegmentationImage, "r16RefinedSegmentation.nii.gz" )
#'
#' }

segmentationRefinement.predict <- function( segmentationImage, labelSet,
  labelModelsStage1, featureImagesStage1, featureImageNamesStage1,
  labelModelsStage2, featureImagesStage2, featureImageNamesStage2,
  dilationRadius = 2, neighborhoodRadiusStage1 = 0, neighborhoodRadiusStage2 = 0,
  useLabelDistances = TRUE, normalizeSamplesPerLabel = TRUE )
{

if ( ! usePkg( "randomForest" ) )
  {
  stop( "Please install the randomForest package." )
  }

if( missing( segmentationImage ) )
  {
  stop( "The segmentation image to be refined is missing." )
  }
if( missing( labelSet ) )
  {
  stop( "The label set is missing." )
  }

if( missing( labelModelsStage1 ) )
  {
  stop( "The label models for stage 1 are missing." )
  }
if( missing( featureImagesStage1 ) )
  {
  stop( "The list of features images for stage 1 are missing." )
  }
if( missing( featureImageNamesStage1 ) )
  {
  stop( "The list of feature image names for stage 1 are missing." )
  }
if( length( labelSet ) != length( labelModelsStage1 ) )
  {
  stop( "The number of labels must match the number of stage 1 models." )
  }

if( missing( labelModelsStage2 ) )
  {
  stop( "The label models for stage 2 are missing." )
  }
if( missing( featureImagesStage2 ) )
  {
  stop( "The list of features images for stage 2 are missing." )
  }
if( missing( featureImageNamesStage2 ) )
  {
  stop( "The list of feature image names for stage 2 are missing." )
  }
if( length( labelSet ) != length( labelModelsStage2 ) )
  {
  stop( "The number of labels must match the number of stage 2 models." )
  }

dimension <- segmentationImage@dimension

if( length( neighborhoodRadiusStage1 ) != dimension )
  {
  neighborhoodRadiusStage1 <- rep( neighborhoodRadiusStage1, dimension )
  }
numberOfNeighborhoodVoxelsStage1 <- 1
for( i in 1:dimension )
  {
  numberOfNeighborhoodVoxelsStage1 <- numberOfNeighborhoodVoxelsStage1 * ( 2 * neighborhoodRadiusStage1[i] + 1 )
  }
if( length( neighborhoodRadiusStage2 ) != dimension )
  {
  neighborhoodRadiusStage2 <- rep( neighborhoodRadiusStage2, dimension )
  }
numberOfNeighborhoodVoxelsStage2 <- 1
for( i in 1:dimension )
  {
  numberOfNeighborhoodVoxelsStage2 <- numberOfNeighborhoodVoxelsStage2 * ( 2 * neighborhoodRadiusStage2[i] + 1 )
  }


if( useLabelDistances )
  {
  featureImageNamesStage1 <- append( featureImageNamesStage1, "LabelDistance" )
  featureImageNamesStage2 <- append( featureImageNamesStage2, "LabelDistance" )
  }

featureNeighborhoodNamesStage1 <- c()
for( i in 1:length( featureImageNamesStage1 ) )
  {
  for( j in 1:numberOfNeighborhoodVoxelsStage1 )
    {
    featureName <- paste0( featureImageNamesStage1[i], 'Neighbor', j )
    featureNeighborhoodNamesStage1 <- append( featureNeighborhoodNamesStage1, featureName )
    }
  }

featureNeighborhoodNamesStage2 <- c()
for( i in 1:length( featureImageNamesStage2 ) )
  {
  for( j in 1:numberOfNeighborhoodVoxelsStage2 )
    {
    featureName <- paste0( featureImageNamesStage2[i], 'Neighbor', j )
    featureNeighborhoodNamesStage2 <- append( featureNeighborhoodNamesStage2, featureName )
    }
  }

segmentationArray <- as.array( segmentationImage )

# we add a row of zeros to use with max.col to stand in for a '0' label (i.e. background)
foregroundProbabilitiesPerLabel <- matrix( 0, nrow = length( labelSet ) + 1, ncol = length( as.array( segmentationImage ) ) )
foregroundProbabilityImages <- list()

for( l in 1:length( labelSet ) )
  {
  label <- labelSet[l]

  message( "Predicting for label ", label, "." );

  segmentationSingleLabelImage <- thresholdImage( antsImageClone( segmentationImage, 'float' ), label, label, 1, 0 )
  segmentationSingleLabelArray <- as.array( segmentationSingleLabelImage )

  maskImage <- iMath( segmentationSingleLabelImage, "MD" , dilationRadius )
  maskArray <- as.array( maskImage )
  maskArrayIndices <- which( maskArray != 0 )

  if( length( maskArrayIndices ) == 0 )
    {
    warning( "No voxels exist for label ", label, ".\n" )
    # we initialized the foreground probabilities to 0 so we simply go on to the next label.
    next;
    }

  if( useLabelDistances )
    {
    distanceSingleLabelImage <- iMath( segmentationSingleLabelImage, 'MaurerDistance' )
    featureImagesStage1[[length( featureImageNamesStage1 )]] <- distanceSingleLabelImage
    featureImagesStage2[[length( featureImageNamesStage2 )]] <- distanceSingleLabelImage
    }

  wholeMaskImage <- segmentationSingleLabelArray
  wholeMaskImage[which( wholeMaskImage != 1 )] <- 1
  wholeMaskImage <- as.antsImage( wholeMaskImage )

  # Accumulate data for stage 1

  subjectDataPerLabel <- matrix( NA, nrow = length( maskArrayIndices ), ncol = length( featureImagesStage1 ) * numberOfNeighborhoodVoxelsStage1 )
  for( j in 1:length( featureImagesStage1 ) )
    {
    featureImageNeighborhoodValues <- getNeighborhoodInMask( featureImagesStage1[[j]], wholeMaskImage, neighborhoodRadiusStage1, boundary.condition = "image" )
    values <- featureImageNeighborhoodValues[, maskArrayIndices]
    if( normalizeSamplesPerLabel )
      {
      featureImagesArray <- as.array( featureImagesStage1[[j]] )
      meanValue <- mean( featureImagesArray[which( segmentationSingleLabelArray != 0 )], na.rm = TRUE )
      if( meanValue != 0 )
        {
        values <- values / meanValue
        }
      }
    subjectDataPerLabel[,( ( j - 1 ) * numberOfNeighborhoodVoxelsStage1 + 1 ):( j * numberOfNeighborhoodVoxelsStage1 )] <- t( values )
    }
  colnames( subjectDataPerLabel ) <- c( featureNeighborhoodNamesStage1 )
  subjectDataPerLabel <- as.data.frame( subjectDataPerLabel )

  # Do prediction for stage 1 to generate the mask for stage 2

  subjectProbabilitiesPerLabelStage1 <- predict( labelModelsStage1[[l]], subjectDataPerLabel, type = "prob" )

  mislabeledVoxelsMaskArray <- maskArray
  mislabeledVoxelsMaskArray[maskArrayIndices] <- subjectProbabilitiesPerLabelStage1[,1]
  mislabeledVoxelsMaskArray[mislabeledVoxelsMaskArray >= 0.5] <- 1
  mislabeledVoxelsMaskArray[mislabeledVoxelsMaskArray < 0.5] <- 0
  mislabeledVoxelsMaskArrayIndices <- which( mislabeledVoxelsMaskArray != 0 )

  # Accumulate data for stage 2

  subjectDataPerLabel <- matrix( NA, nrow = length( mislabeledVoxelsMaskArrayIndices ), ncol = length( featureImagesStage2 ) * numberOfNeighborhoodVoxelsStage2 )
  for( j in 1:length( featureImagesStage2 ) )
    {
    featureImageNeighborhoodValues <- getNeighborhoodInMask( featureImagesStage2[[j]], wholeMaskImage, neighborhoodRadiusStage2, boundary.condition = "image" )
    values <- featureImageNeighborhoodValues[, mislabeledVoxelsMaskArrayIndices]
    if( normalizeSamplesPerLabel )
      {
      featureImagesArray <- as.array( featureImagesStage2[[j]] )
      meanValue <- mean( featureImagesArray[which( segmentationSingleLabelArray != 0 )], na.rm = TRUE )
      if( meanValue != 0 )
        {
        values <- values / meanValue
        }
      }
    subjectDataPerLabel[,( ( j - 1 ) * numberOfNeighborhoodVoxelsStage2 + 1 ):( j * numberOfNeighborhoodVoxelsStage2 )] <- t( values )
    }
  colnames( subjectDataPerLabel ) <- c( featureNeighborhoodNamesStage2 )
  subjectDataPerLabel <- as.data.frame( subjectDataPerLabel )

  # Do prediction for stage 2

  subjectProbabilitiesPerLabelStage2 <- predict( labelModelsStage2[[l]], subjectDataPerLabel, type = "prob" )

  backgroundProbabilityPerLabelArray <- array( 0, dim = dim( segmentationArray ) )
  backgroundProbabilityPerLabelArray[mislabeledVoxelsMaskArrayIndices] <- subjectProbabilitiesPerLabelStage2[, 1]

  foregroundProbabilityPerLabelArray <- array( 0, dim = dim( segmentationArray ) )
  foregroundProbabilityPerLabelArray[mislabeledVoxelsMaskArrayIndices] <- subjectProbabilitiesPerLabelStage2[, 2]

  foregroundProbabilitiesPerLabel[l, mislabeledVoxelsMaskArrayIndices] <- foregroundProbabilityPerLabelArray[mislabeledVoxelsMaskArrayIndices]

  maxBackgroundIndices <- which( backgroundProbabilityPerLabelArray > foregroundProbabilitiesPerLabel[length( labelSet ) + 1, ] )

  foregroundProbabilitiesPerLabel[length( labelSet ) + 1, maxBackgroundIndices] <- backgroundProbabilityPerLabelArray[maxBackgroundIndices]

  foregroundProbabilityImages[[l]] <- as.antsImage( array( foregroundProbabilitiesPerLabel[l,], dim = dim( segmentationArray ) ), reference = segmentationImage )
  }

appendedLabelSet <- append( labelSet, 0 )

# Before we pick the max label, we set all the probabilities to zero if the
# foreground probabilities are < 0.5.  Since we're doing a binary comparison,
# if the foreground < 0.5, that means the label at that voxel should be background.
# foregroundProbabilitiesPerLabel[foregroundProbabilitiesPerLabel < 0.5] <- 0.0

refinedSegmentationArray <- appendedLabelSet[max.col( t( foregroundProbabilitiesPerLabel ), ties.method = 'last' )]

refinedSegmentationArray[which( refinedSegmentationArray == 0 )] <- segmentationArray[which( refinedSegmentationArray == 0 )]
refinedSegmentationImage <- as.antsImage( array( refinedSegmentationArray, dim = dim( segmentationArray ) ), reference = segmentationImage )

return ( list( RefinedSegmentationImage = refinedSegmentationImage, ForegroundProbabilityImages = foregroundProbabilityImages ) )
}

