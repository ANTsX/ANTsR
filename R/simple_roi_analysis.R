#' Perform ROI population analysis between two groups.
#' 
#' Student's t-test performed for each labeled region within a specified mask
#' to determine difference between a control group and an experimental group.
#' 
#' 
#' @param dimensionality Dimension of images.
#' @param imageFileNames List of image files.
#' @param testType A value of 'lm', 'student.t', or 'wilcox'.  The latter two
#' test types are assumed to be two-sampled parametric and non-parametric,
#' respectively.
#' @param predictors A simple vector or single column matrix or data frame
#' specifying sample membership for student.t or wilcox testing.  For the 'lm'
#' option, a data frame must be specified whose column names match the
#' specified formula.
#' @param formula Used with the 'lm' option for more sophisticated modeling.
#' @param roiLabelsFileName File name of mask defining the region over which
#' testing is performed (foreground voxel != 0, background voxel = 0)
#' @return list consisting of unique roi labels, t-values, and p-values.
#' @author Tustison NJ
#' @examples
#' 
#' \dontrun{
#' # Get the image files
#' controlFileNames <- list.files( path = "./example_images/", pattern =
#'   glob2rx( "phantomtemplate_CONTROL*" ), full.names = TRUE, recursive = FALSE )
#' experimentalFileNames <- list.files( path = "./example_images/", pattern =
#'   glob2rx( "phantomtemplate_EXP*" ), full.names = TRUE, recursive = FALSE )
#' 
#' images <- c( controlFileNames, experimentalFileNames )
#' diagnosis <- c( rep( 1, length( controlFileNames ) ), rep( 0, length( experimentalFileNames ) ) )
#' age <- runif( length( diagnosis ), 25, 30 )
#' outputPath <- "./test_output/"
#' 
#' roiResults.ttest <- simple_roi_analysis( dimensionality = 2, imageFileNames = images,
#'   predictors = data.frame( diagnosis ),
#'   roiLabelsFileName = "./example_images/phantomtemplate_roi_labels.nii.gz", testType = 'student.t' )
#' 
#' roiResults.wilcox <- simple_roi_analysis( dimensionality = 2, imageFileNames = images,
#'   predictors = data.frame( diagnosis ),
#'   roiLabelsFileName = "./example_images/phantomtemplate_roi_labels.nii.gz", testType = 'wilcox' )
#' 
#' roiResults.lm <- simple_roi_analysis( dimensionality = 2, imageFileNames = images,
#'   predictors = data.frame( cbind( diagnosis, age ) ), formula = as.formula( value ~ 1 + diagnosis + age ),
#'   roiLabelsFileName = "./example_images/phantomtemplate_roi_labels.nii.gz", testType = 'lm' )
#' }
#' 
#' @export simple_roi_analysis
simple_roi_analysis <- function(dimensionality = 3, imageFileNames = c(), predictors, 
  formula, testType = c("lm", "student.t", "wilcox"), roiLabelsFileName = "") {
  
  ## Check input variables
  
  if (missing(testType)) {
    stop("'testType' missing")
  }
  testType <- match.arg(testType, c("lm", "student.t", "wilcox"))
  
  if (testType == "lm") {
    if (missing(formula)) {
      stop("A formula must be specified for testType = 'lm'.")
    }
    if (missing(predictors)) {
      stop("'predictors' missing")
    }
    if (!is.data.frame(predictors)) {
      stop("Expected data frame for 'predictors' with 'lm' testing.")
    }
  } else {
    if (missing(predictors)) {
      stop("'predictors' missing")
    }
    if (is.vector(predictors) || (is.matrix(predictors) && ncol(predictors) == 
      1)) {
      predictors <- as.data.frame(predictors)
    }
    colnames(predictors) <- c("diagnosis")
    formula <- as.formula(response ~ 1 + diagnosis)
  }
  predictorNames <- colnames(predictors)
  
  # Check to make sure that the predictor data frame has the same variable names as
  # the formula.
  responseVariableName <- all.vars(formula)[attr(terms(formula), "response")]
  variables <- attr(terms(formula), "variables")
  tmp <- c()
  for (i in 3:length(variables)) {
    tmp <- append(tmp, variables[[i]])
  }
  variables <- tmp
  
  if (!all(variables %in% predictorNames)) {
    stop("The predictor column names and formula names do not match.")
  }
  
  numberOfImages <- length(imageFileNames)
  if (numberOfImages != nrow(predictors)) {
    stop("The number of predictor values does not match the number of images.\n")
  }
  
  ## Do the actual data prep and testing
  
  cat("******* Conducting ", testType, " ROI analysis (number of images = ", numberOfImages, 
    "). *******\n\n", sep = "")
  
  # Read the mask and place the masked voxels in the images in a matrix
  
  cat("Reading ROI labels file ", roiLabelsFileName, "\n\n", sep = "")
  roiLabelsMask <- antsImageRead(roiLabelsFileName, dimensionality, "unsigned int")
  roiLabels <- sort(unique(c(as.array(roiLabelsMask))))
  roiLabels <- roiLabels[which(roiLabels != 0)]
  
  cat("Unique ROI labels =", roiLabels, "\n\n", sep = " ")
  
  numberOfForegroundVoxels <- length(roiLabelsMask[roiLabelsMask != 0])
  
  dataMatrix <- matrix(data = NA, nrow = numberOfImages, ncol = numberOfForegroundVoxels)
  
  for (i in 1:length(imageFileNames)) {
    predictorString <- paste(predictorNames[1], "=", predictors[i, 1], sep = "")
    if (ncol(predictors) >= 2) {
      for (j in 2:ncol(predictors)) {
        predictorString <- paste(predictorString, ", ", predictorNames[j], 
          "=", predictors[i, j], sep = "")
      }
    }
    
    cat("Reading image ", imageFileNames[i], " (", i, " of ", numberOfImages, 
      ", ", predictorString, ").\n", sep = "")
    subjectImage <- antsImageRead(imageFileNames[i], dimensionality)
    dataMatrix[i, ] <- as.array(subjectImage[roiLabelsMask != 0])
  }
  
  roiLabelsMask <- c(roiLabelsMask[roiLabelsMask != 0])
  
  # Perform the t-testing.  Monitor progress.
  
  tValues <- rep(NA, length(roiLabels))
  pValues <- rep(NA, length(roiLabels))
  
  cat("\nTesting...\n")
  for (i in 1:length(roiLabels)) {
    values <- rowMeans(dataMatrix[, which(roiLabelsMask == roiLabels[i])], na.rm = TRUE)
    
    testData <- cbind(rowMeans(dataMatrix[, which(roiLabelsMask == roiLabels[i])], 
      na.rm = TRUE), predictors)
    colnames(testData) <- c(responseVariableName, predictorNames)
    
    if (testType == "student.t") {
      testResults <- try(t.test(formula = formula, data = testData))
      if (inherits(testResults, "try-error")) {
        tValues[i] <- NA
        pValues[i] <- NA
      } else {
        tValues[i] <- testResults$statistic
        pValues[i] <- testResults$p.value
      }
    } else if (testType == "wilcox") {
      testResults <- try(wilcox.test(formula = formula, data = testData))
      if (inherits(testResults, "try-error")) {
        tValues[i] <- NA
        pValues[i] <- NA
      } else {
        tValues[i] <- testResults$statistic
        pValues[i] <- testResults$p.value
      }
    } else {
      # if( testType == 'lm' )
      testResults <- summary(lm(formula = formula, data = testData))
      tValues[i] <- testResults$coef[2, 3]
      pValues[i] <- testResults$coef[2, 4]
    }
    
    
    
  }
  cat("Done.\n", sep = "")
  
  return(list(roi.labels = roiLabels, t.values = tValues, p.values = pValues))
} 
