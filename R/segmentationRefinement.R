#' Segmentation refinement using corrective learning (training)
#'
#' A random forest implementation of the corrective learning wrapper introduced
#' in Wang, et al., Neuroimage 2011 (http://www.ncbi.nlm.nih.gov/pubmed/21237273).
#' The training process involves building two sets of models from training data
#' for each label in the initial segmentation data.  For each label, we build
#' two models:  one model is for misclassification of 'positive' or 'foreground'
#' voxels (false vs. true) and one model for misclassification of 'negative' or
#' 'background' voxels (false vs. true).
#'
#' @param featureImages a list of lists of feature images.  Each list of feature images
#'        corresponds to a single subject.  Possibilities are outlined in the above-cited
#'        paper.
#' @param truthLabelImages a list of "ground-truth" segmentation images, one for each
#'        set of feature images.
#' @param segmentationImages a list of estimated segmentation images, one for each set of
#'        feature images
#' @param featureImageNames a vector of character strings naming the set of features.
#'        This parameter is optional but does help in investigating the relative
#'        importance of specific features.
#' @param labelSet a vector specifying the labels of interest.  If not specified,
#'         the full set is determined from the truthLabelImages.
#' @param maximumNumberOfSamplesOrProportionPerClass specified the maximum number of samples
#'        used to build the model for each element of the labelSet.  If <= 1, we use it as
#'        as a proportion of the total number of voxels.
#' @param dilationRadius specifies the dilation radius for determining the ROI for
#'        each label using binary morphology.  Alternatively, the user can specify a
#'        float distance value, e.g., "dilationRadius = '2.75mm'", to employ an isotropic
#'        dilation based on physical distance.  For the latter, the distance value followed
#'        by the character string 'mm' (for millimeters) is necessary.
#' @param neighborhoodRadius specifies which voxel neighbors should be included in
#'        building the model.  The user can specify a scalar or vector.
#' @param normalizeSamplesPerLabel if TRUE, the samples from each ROI are normalized
#'        by the mean of the voxels in that ROI.  Can also specify as a vector to normalize
#'        per feature image.
#'
#' @return list with two models per label (LabelForegroundModels and labelBackgroundModels),
#'         the label set (LabelSet), and the feature image names (FeatureImageNames).
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
#'  # Perform training.  We train on images "r27", "r30", "r62", "r64", "r85" and test/predict
#'    on image "r16".
#'
#'  cat( "\nTraining\n\n" )
#'
#'  segLearning <- segmentationRefinement.train( featureImages = featureImages[2:6],
#'    truthLabelImages = atroposSegs[2:6], segmentationImages = kmeansSegs[2:6],
#'    featureImageNames = featureImageNames, labelSet = segmentationLabels,
#'    maximumNumberOfSamplesOrProportionPerClass = 100, dilationRadius = 1,
#'    normalizeSamplesPerLabel = TRUE )
#'
#'  cat( "\nGenerating importance plots.\n\n" )
#'
#'  for( m in 1:length( segLearning$LabelModels ) )
#'    {
#'    forestImp <- importance( segLearning$LabelForegroundModels[[m]], type = 1 )
#'    forestImp.df <- data.frame( Statistic = names( forestImp[,1] ), Importance = as.numeric( forestImp[,1] )  )
#'    forestImp.df <- forestImp.df[order( forestImp.df$Importance ),]
#'
#'    forestImp.df$Statistic <- factor( x = forestImp.df$Statistic, levels = forestImp.df$Statistic )
#'
#'    iPlot <- ggplot( data = forestImp.df, aes( x = Importance, y = Statistic ) ) +
#'             geom_point( aes( color = Importance ) ) +
#'             labs( title = paste0( 'Label ', segLearning$LabelSet[m] ) ) +
#'             ylab( "" ) +
#'             xlab( "MeanDecreaseAccuracy" ) +
#'             scale_color_continuous( low = "navyblue", high = "darkred" ) +
#'             theme( axis.text.y = element_text( size = 10 ) ) +
#'             theme( plot.margin = unit( c( 0.1, 0.1, 0.1, -0.5 ), "cm" ) ) +
#'             theme( legend.position = "none" )
#'    ggsave( file = paste0( 'importancePlotsLabel', segLearning$LabelSet[m], 'Foreground.pdf' ), plot = iPlot, width = 4, height = 6 )
#'    }
#'
#' }

segmentationRefinement.train <- function( featureImages, truthLabelImages,
  segmentationImages, featureImageNames = c(), labelSet = c(),
  maximumNumberOfSamplesOrProportionPerClass = 1, dilationRadius = 2,
  neighborhoodRadius = 0, normalizeSamplesPerLabel = TRUE )
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

if( length( normalizeSamplesPerLabel ) == 1 )
  {
  normalizeSamplesPerLabel <- rep( normalizeSamplesPerLabel[1], length( featureImageNames ) )
  } else if ( length( normalizeSamplesPerLabel ) > 1 && length( normalizeSamplesPerLabel ) != length( featureImageNames ) ) {
  stop( "The size of the variable normalizeSamplesPerLabel does not match the number of features." );
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

labelBackgroundModels <- list()
labelForegroundModels <- list()

for( l in 1:length( labelSet ) )
  {
  label <- labelSet[l]

  message( "Doing label ", label );

  modelDataPerLabel <- matrix()
  for( i in 1:totalNumberOfSubjects )
    {
    message( "  Sampling data from subject ", i )

    # Get the ROI mask from the segmentation image for the current label.  Within that ROI,
    # find which voxels are mislabeled.  These mislabeled and correctly labeled voxels are
    # given in the variable "mislabeledVoxelsMaskArray"

    truePositiveLabel <- 2
    trueNegativeLabel <- 1
    falseNegativeLabel <- -1       # type II
    falsePositiveLabel <- -2       # type I

    segmentationSingleLabelImage <- thresholdImage( antsImageClone( segmentationImages[[i]], 'float' ), label, label, 1, 0 )
    segmentationSingleLabelArray <- as.array( segmentationSingleLabelImage )

    if( length( which( segmentationSingleLabelArray == 1 ) ) == 0 )
      {
      warning( "Warning:  No voxels exist for label ", label, " of subject ", i )
      next
      }

    roiMaskImage <- antsImageClone( segmentationSingleLabelImage, 'float' )
    if( ! is.character( dilationRadius ) )
      {
      roiDilationMaskImage <- iMath( segmentationSingleLabelImage, "MD" , dilationRadius )
      roiErosionMaskImage <- iMath( segmentationSingleLabelImage, "ME" , dilationRadius )
      roiMaskImage <- roiDilationMaskImage - roiErosionMaskImage
      } else {
      dilationRadiusValue <- as.numeric( gsub( 'mm', '', dilationRadius ) )
      distanceImage <- iMath( segmentationSingleLabelImage, "MaurerDistance" )
      minSpacing <- min( antsGetSpacing( distanceImage ) )

      segmentationSingleLabelInverseImage <- thresholdImage( segmentationSingleLabelImage, 0, 0, 1, 0 )
      distanceInverseImage <- iMath( segmentationSingleLabelInverseImage, "MaurerDistance" )

      roiMaskImage <- thresholdImage( distanceImage, 0.1 * minSpacing, dilationRadiusValue, 1, 0 ) +
                      thresholdImage( distanceInverseImage, 0.1 * minSpacing, dilationRadiusValue, 1, 0 )
      }
    roiMaskArray <- as.array( roiMaskImage )

    truthSingleLabelImage <- thresholdImage( antsImageClone( truthLabelImages[[i]], 'float' ), label, label, 1, 0 )
    truthSingleLabelArray <- as.array( truthSingleLabelImage )

    mislabeledVoxelsMaskArray <- ( truthSingleLabelArray - segmentationSingleLabelArray )
    falseNegativeIndices <- which( mislabeledVoxelsMaskArray > 0 )
    falsePositiveIndices <- which( mislabeledVoxelsMaskArray < 0 )
    truePositiveIndices <- which( mislabeledVoxelsMaskArray == 0 & truthSingleLabelArray == 1 )
    trueNegativeIndices <- which( mislabeledVoxelsMaskArray == 0 & truthSingleLabelArray == 0 )

    mislabeledVoxelsMaskArray[falseNegativeIndices] <- falseNegativeLabel
    mislabeledVoxelsMaskArray[falsePositiveIndices] <- falsePositiveLabel
    mislabeledVoxelsMaskArray[trueNegativeIndices] <- trueNegativeLabel
    mislabeledVoxelsMaskArray[truePositiveIndices] <- truePositiveLabel
    mislabeledVoxelsMaskArray <- mislabeledVoxelsMaskArray * roiMaskArray

    binaryLabelSet <- c( falsePositiveLabel, falseNegativeLabel, trueNegativeLabel, truePositiveLabel )
    binaryLabelSetNames <- c( "false positive", "false negative", "true negative", "true positive" )

    numberOfSamplesPerLabelInSubjectData <- rep( 0, length( binaryLabelSet ) )

    if( maximumNumberOfSamplesOrProportionPerClass <= 1 )
      {

      for( n in 1:length( binaryLabelSet ) )
        {
        labelIndices <- which( mislabeledVoxelsMaskArray == binaryLabelSet[n] & roiMaskArray == 1 )
        numberOfLabelIndices <- length( labelIndices )
        numberOfSamplesPerLabelInSubjectData[n] <- floor( numberOfLabelIndices * maximumNumberOfSamplesOrProportionPerClass )
        message( "    Number of ", binaryLabelSetNames[n], " voxels = ", numberOfLabelIndices, "  (n samples = ", numberOfSamplesPerLabelInSubjectData[n], ")" )
        }

      } else {

      # Ensure that the samples per label are balanced in each subject
      minimumNumberOfSamplesInSubjectData <- maximumNumberOfSamplesOrProportionPerClass

      for( n in 1:length( binaryLabelSet ) )
        {
        labelIndices <- which( mislabeledVoxelsMaskArray == binaryLabelSet[n] & roiMaskArray == 1 )
        numberOfLabelIndices <- length( labelIndices )
        message( "    Number of ", binaryLabelSetNames[n], " voxels = ", numberOfLabelIndices )
        if( numberOfLabelIndices < minimumNumberOfSamplesInSubjectData )
          {
          minimumNumberOfSamplesInSubjectData <- numberOfLabelIndices
          }
        }

      for( n in 1:length( binaryLabelSet ) )
        {
        numberOfSamplesPerLabelInSubjectData[n] <- min( minimumNumberOfSamplesInSubjectData, numberOfLabelIndices )
        }

      message( "    Number of samples per class = ",  minimumNumberOfSamplesInSubjectData )
      }

    truthLabelIndices <- list()
    for( n in 1:length( binaryLabelSet ) )
      {
      labelIndices <- which( mislabeledVoxelsMaskArray == binaryLabelSet[n] & roiMaskArray == 1 )
      numberOfLabelIndices <- length( labelIndices )

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
        if( normalizeSamplesPerLabel[j] )
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

  modelBackgroundDataPerLabel <-
    modelDataPerLabel[which( modelDataPerLabel$Labels == falseNegativeLabel | modelDataPerLabel$Labels == trueNegativeLabel ),]
  modelBackgroundDataPerLabel$Labels <- factor( modelBackgroundDataPerLabel$Labels )
  modelBackgroundForest <- randomForest::randomForest( modelFormula, modelBackgroundDataPerLabel,
    ntree = 1000, type = "classification", importance = TRUE, na.action = na.omit )

  labelBackgroundModels[[l]] <- modelBackgroundForest;

  modelForegroundDataPerLabel <-
    modelDataPerLabel[which( modelDataPerLabel$Labels == falsePositiveLabel | modelDataPerLabel$Labels == truePositiveLabel ),]
  modelForegroundDataPerLabel$Labels <- factor( modelForegroundDataPerLabel$Labels )
  modelForegroundForest <- randomForest::randomForest( modelFormula, modelForegroundDataPerLabel,
    ntree = 1000, type = "classification", importance = TRUE, na.action = na.omit )

  labelForegroundModels[[l]] <- modelForegroundForest;

#   modelForest <- randomForest::randomForest( modelFormula, modelDataPerLabel,
#     ntree = 1000, type = "classification", importance = TRUE, na.action = na.omit )
#   labelModels[[l]] <- modelForest;

  # Stop the clock
  elapsedTime <- proc.time() - ptm
  message( "  Done (", as.numeric( elapsedTime[3] ), " seconds).\n", sep = "" )
  }

# return ( list( LabelModels = labelModels, LabelSet = labelSet, FeatureImageNames = featureImageNames ) )

  return ( list( LabelForegroundModels = labelForegroundModels,
                 LabelBackgroundModels = labelBackgroundModels,
                 LabelSet = labelSet, FeatureImageNames = featureImageNames ) )
}

#' Segmentation refinement using corrective learning (prediction)
#'
#' A random forest implementation of the corrective learning wrapper introduced
#' in Wang, et al., Neuroimage 2011 (http://www.ncbi.nlm.nih.gov/pubmed/21237273).
#' The prediction process involves using the label-specific training models
#' to refine an initial segmentation.
#'
#' @param segmentationImage image to refine via corrective learning.
#' @param labelSet a vector specifying the labels of interest.  Must be specified.
#' @param labelForegroundModels a list of all foreground models.
#'        Each element of the labelSet requires a foreground model.
#' @param labelBackgroundModels a list of all background models.
#'        Each element of the labelSet requires a background model.
#' @param featureImages a list of feature images.
#' @param featureImageNames is a vector of character strings naming the set of features.
#'        Must be specified.
#' @param dilationRadius specifies the dilation radius for determining the ROI for
#'        each label using binary morphology.  Alternatively, the user can specify a
#'        float distance value, e.g., "dilationRadius = '2.75mm'", to employ an isotropic
#'        dilation based on physical distance.  For the latter, the distance value followed
#'        by the character string 'mm' (for millimeters) is necessary.
#' @param neighborhoodRadius specifies which voxel neighbors should be included in
#'        prediction.  The user can specify a scalar or vector but it must match
#'        with what was used for training.
#' @param normalizeSamplesPerLabel if TRUE, the samples from each ROI are normalized
#'        by the mean of the voxels in that ROI.  Can be a vector (one element per feature).
#'
#' @return a list consisting of the refined segmentation estimate (RefinedSegmentationImage)
#'         and a list of the foreground probability images (ForegroundProbabilityImages).
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
#'  # Perform training.  We train on images "r27", "r30", "r62", "r64", "r85" and
#'  # test/predict on image "r16".
#'
#'  cat( "\nTraining\n\n" )
#'
#'  segLearning <- segmentationRefinement.train( featureImages = featureImages[2:6],
#'    truthLabelImages = atroposSegs[2:6], segmentationImages = kmeansSegs[2:6],
#'    featureImageNames = featureImageNames, labelSet = segmentationLabels,
#'    maximumNumberOfSamplesOrProportionPerClass = 100, dilationRadius = 1,
#'    neighborhoodRadius = c( 1, 1 ), normalizeSamplesPerLabel = TRUE )
#'
#'  cat( "\nPrediction\n\n" )
#'
#'  refinement <- segmentationRefinement.predict(
#'    segmentationImage = kmeansSegs[[1]], labelSet = segmentationLabels,
#'    segLearning$LabelForegroundModels, segLearning$LabelBackgroundModels,
#'    featureImages[[1]], featureImageNames,
#'    dilationRadius = 1, neighborhoodRadius = c( 1, 1 ),
#'    normalizeSamplesPerLabel = TRUE )
#'
#'  # Compare "ground truth" = atroposSegs[[1]] with refinement$RefinedSegmentationImage
#'
#'   antsImageWrite( refinement$RefinedSegmentationImage, "r16RefinedSegmentation.nii.gz" )
#'
#' }

segmentationRefinement.predict <- function( segmentationImage, labelSet,
  labelForegroundModels, labelBackgroundModels,
  featureImages, featureImageNames, dilationRadius = 2,
  neighborhoodRadius = 0, normalizeSamplesPerLabel = TRUE )
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

if( missing( labelForegroundModels ) )
  {
  stop( "The label models for false/true positive classification are missing." )
  }
if( missing( labelBackgroundModels ) )
  {
  stop( "The label models for false/true positive classification are missing." )
  }
if( length( labelForegroundModels ) != length( labelBackgroundModels ) )
  {
  stop( "The number of label models (positive vs. negative) are not equal." )
  }

if( missing( featureImages ) )
  {
  stop( "The list of features images is missing." )
  }
if( missing( featureImageNames ) )
  {
  stop( "The list of feature image names is missing." )
  }
if( length( labelSet ) != length( labelForegroundModels ) )
  {
  stop( "The number of labels must match the number of models." )
  }

dimension <- segmentationImage@dimension

if( length( neighborhoodRadius ) != dimension )
  {
  neighborhoodRadius <- rep( neighborhoodRadius, dimension )
  }
numberOfNeighborhoodVoxels <- 1
for( i in 1:dimension )
  {
  numberOfNeighborhoodVoxels <- numberOfNeighborhoodVoxels * ( 2 * neighborhoodRadius[i] + 1 )
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

if( length( normalizeSamplesPerLabel ) == 1 )
  {
  normalizeSamplesPerLabel <- rep( normalizeSamplesPerLabel[1], length( featureImageNames ) )
  } else if ( length( normalizeSamplesPerLabel ) > 1 && length( normalizeSamplesPerLabel ) != length( featureImageNames ) ) {
  stop( "The size of the variable normalizeSamplesPerLabel does not match the number of features." );
  }

segmentationArray <- as.array( segmentationImage )

# we add a row of zeros to use with max.col to stand in for a '0' label (i.e. background)
foregroundProbabilitiesPerLabel <- matrix()
if( ! is.element( 0, labelSet ) )
  {
  foregroundProbabilitiesPerLabel <- matrix( 0, nrow = length( labelSet ) + 1, ncol = length( as.array( segmentationImage ) ) )
  foregroundProbabilitiesPerLabel[, length( labelSet ) + 1] <- 0.5
  } else {
  foregroundProbabilitiesPerLabel <- matrix( 0, nrow = length( labelSet ), ncol = length( as.array( segmentationImage ) ) )
  }

foregroundProbabilityImages <- list()

for( l in 1:length( labelSet ) )
  {
  label <- labelSet[l]

  message( "Predicting for label ", label, "." );

  segmentationSingleLabelImage <- thresholdImage( antsImageClone( segmentationImage, 'float' ), label, label, 1, 0 )
  segmentationSingleLabelArray <- as.array( segmentationSingleLabelImage )

  if( length( which( segmentationSingleLabelArray == 1 ) ) == 0 )
    {
    warning( "Warning:  No voxels exist for label ", label, " of subject ", i )
    next
    }

  roiMaskImage <- antsImageClone( segmentationSingleLabelImage, 'float' )
  if( ! is.character( dilationRadius ) )
    {
    roiDilationMaskImage <- iMath( segmentationSingleLabelImage, "MD" , dilationRadius )
    roiErosionMaskImage <- iMath( segmentationSingleLabelImage, "ME" , dilationRadius )
    roiMaskImage <- roiDilationMaskImage - roiErosionMaskImage
    } else {
    dilationRadiusValue <- as.numeric( gsub( 'mm', '', dilationRadius ) )
    distanceImage <- iMath( segmentationSingleLabelImage, "MaurerDistance" )
    minSpacing <- min( antsGetSpacing( distanceImage ) )

    segmentationSingleLabelInverseImage <- thresholdImage( segmentationSingleLabelImage, 0, 0, 1, 0 )
    distanceInverseImage <- iMath( segmentationSingleLabelInverseImage, "MaurerDistance" )

    roiMaskImage <- thresholdImage( distanceImage, 0.1 * minSpacing, dilationRadiusValue, 1, 0 ) +
                    thresholdImage( distanceInverseImage, 0.1 * minSpacing, dilationRadiusValue, 1, 0 )
    }
  roiMaskArray <- as.array( roiMaskImage )
  roiMaskArrayIndices <- which( roiMaskArray != 0 )

  roiMaskArrayForegroundIndices <- roiMaskArrayIndices[which( segmentationSingleLabelArray[roiMaskArrayIndices] == 1 )]
  roiMaskArrayBackgroundIndices <- roiMaskArrayIndices[which( segmentationSingleLabelArray[roiMaskArrayIndices] == 0 )]

  wholeMaskImage <- segmentationSingleLabelArray
  wholeMaskImage[which( wholeMaskImage != 1 )] <- 1
  wholeMaskImage <- as.antsImage( wholeMaskImage )

  # Accumulate data for prediction

  ## true/false positive
  subjectForegroundDataPerLabel <- matrix( NA, nrow = length( roiMaskArrayForegroundIndices ), ncol = length( featureImages ) * numberOfNeighborhoodVoxels )
  for( j in 1:length( featureImages ) )
    {
    featureImageNeighborhoodValues <- getNeighborhoodInMask( featureImages[[j]], wholeMaskImage, neighborhoodRadius, boundary.condition = "image" )
    values <- featureImageNeighborhoodValues[, roiMaskArrayForegroundIndices]
    if( normalizeSamplesPerLabel[j] )
      {
      featureImagesArray <- as.array( featureImages[[j]] )
      meanValue <- mean( featureImagesArray[which( segmentationSingleLabelArray != 0 )], na.rm = TRUE )
      if( meanValue != 0 )
        {
        values <- values / meanValue
        }
      }
    subjectForegroundDataPerLabel[,( ( j - 1 ) * numberOfNeighborhoodVoxels + 1 ):( j * numberOfNeighborhoodVoxels )] <- t( values )
    }
  colnames( subjectForegroundDataPerLabel ) <- c( featureNeighborhoodNames )
  subjectForegroundDataPerLabel <- as.data.frame( subjectForegroundDataPerLabel )

  ## true/false negative
  subjectBackgroundDataPerLabel <- matrix( NA, nrow = length( roiMaskArrayBackgroundIndices ), ncol = length( featureImages ) * numberOfNeighborhoodVoxels )
  for( j in 1:length( featureImages ) )
    {
    featureImageNeighborhoodValues <- getNeighborhoodInMask( featureImages[[j]], wholeMaskImage, neighborhoodRadius, boundary.condition = "image" )
    values <- featureImageNeighborhoodValues[, roiMaskArrayBackgroundIndices]
    if( normalizeSamplesPerLabel[j] )
      {
      featureImagesArray <- as.array( featureImages[[j]] )
      meanValue <- mean( featureImagesArray[which( segmentationSingleLabelArray != 0 )], na.rm = TRUE )
      if( meanValue != 0 )
        {
        values <- values / meanValue
        }
      }
    subjectBackgroundDataPerLabel[,( ( j - 1 ) * numberOfNeighborhoodVoxels + 1 ):( j * numberOfNeighborhoodVoxels )] <- t( values )
    }
  colnames( subjectBackgroundDataPerLabel ) <- c( featureNeighborhoodNames )
  subjectBackgroundDataPerLabel <- as.data.frame( subjectBackgroundDataPerLabel )

#   truePositiveLabel <- 2
#   trueNegativeLabel <- 1
#   falseNegativeLabel <- -1       # type II
#   falsePositiveLabel <- -2       # type I

#  binaryLabelSet <- c( falsePositiveLabel, falseNegativeLabel, trueNegativeLabel, truePositiveLabel )

  # Do prediction

  subjectForegroundProbabilitiesPerLabel <- predict( labelForegroundModels[[l]], subjectForegroundDataPerLabel, type = "prob" )
  subjectBackgroundProbabilitiesPerLabel <- predict( labelBackgroundModels[[l]], subjectBackgroundDataPerLabel, type = "prob" )

  # The foreground probability at a voxel is a 'false negative' if the initial estimate is
  # 'background' and 'true positive' if the initial estimate is 'foreground'.

  foregroundProbabilitiesPerLabel[l,] <- segmentationSingleLabelArray
  foregroundProbabilitiesPerLabel[l, roiMaskArrayIndices] <- 0
  foregroundProbabilitiesPerLabel[l, roiMaskArrayForegroundIndices] <- subjectForegroundProbabilitiesPerLabel[, 2]
  foregroundProbabilitiesPerLabel[l, roiMaskArrayBackgroundIndices] <- subjectBackgroundProbabilitiesPerLabel[, 1]

  foregroundProbabilityImages[[l]] <- as.antsImage( array( foregroundProbabilitiesPerLabel[l,], dim = dim( segmentationArray ) ), reference = segmentationImage )
  }

appendedLabelSet <- labelSet
if( ! is.element( 0, labelSet ) )
  {
  appendedLabelSet <- c( labelSet, 0 )
  }

refinedSegmentationArray <- appendedLabelSet[max.col( t( foregroundProbabilitiesPerLabel ), ties.method = 'last' )]
refinedSegmentationImage <- as.antsImage( array( refinedSegmentationArray, dim = dim( segmentationArray ) ), reference = segmentationImage )

return ( list( RefinedSegmentationImage = refinedSegmentationImage, ForegroundProbabilityImages = foregroundProbabilityImages ) )
}
