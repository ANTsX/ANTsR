#' Perform voxel-based population analysis between two groups.
#' 
#' Student's t-test performed at each voxel within a specified mask region to
#' determine difference between a control group and an experimental group.
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
#' @param maskFileName File name of mask defining the region over which testing
#' is performed (foreground voxel != 0, background voxel = 0)
#' @param outputPrefix Output directory and prefix to prepend to the resulting
#' image file names (t-value image, 'one minus' p-value image, and 'one minus'
#' (false discovery rate) corrected p-value image.
#' @return None.
#' @author Tustison NJ
#' @examples
#' 
#' \dontrun{
#' # Get the image files
#' controlFileNames <- list.files( path = './example_images/', pattern =
#'   glob2rx( 'phantomtemplate_CONTROL*' ), full.names = TRUE, recursive = FALSE )
#' experimentalFileNames <- list.files( path = './example_images/', pattern =
#'   glob2rx( 'phantomtemplate_EXP*' ), full.names = TRUE, recursive = FALSE )
#' 
#' images <- c( controlFileNames, experimentalFileNames )
#' diagnosis <- c( rep( 1, length( controlFileNames ) ), rep( 0, length( experimentalFileNames ) ) )
#' age <- runif( length( diagnosis ), 25, 30 )
#' outputPath <- './test_output/'
#' 
#' prefix <- 'ANTsR_t.test_'
#' simple_voxel_based_analysis( dimensionality = 2, imageFileNames = images,
#'   predictors = data.frame( diagnosis ),
#'   maskFileName = './example_images/phantomtemplate_mask.nii.gz',
#'   outputPrefix = paste( outputPath, prefix, sep = '' ), testType = 'student.t' )
#' 
#' prefix <- 'ANTsR_wilcox_'
#' simple_voxel_based_analysis( dimensionality = 2, imageFileNames = images,
#'   predictors = data.frame( diagnosis ),
#'   maskFileName = './example_images/phantomtemplate_mask.nii.gz',
#'   outputPrefix = paste( outputPath, prefix, sep = '' ), testType = 'wilcox' )
#' 
#' prefix <- 'ANTsR_lm_'
#' simple_voxel_based_analysis( dimensionality = 2, imageFileNames = images,
#'   predictors = data.frame( cbind( diagnosis, age ) ), formula = as.formula( value ~ 1 + diagnosis + age ),
#'   maskFileName = './example_images/phantomtemplate_mask.nii.gz',
#'   outputPrefix = paste( outputPath, prefix, sep = '' ), testType = 'lm' )
#' }
#' 
#' @export simple_voxel_based_analysis
simple_voxel_based_analysis <- function(dimensionality = 3, imageFileNames = c(), 
  predictors, formula, testType = c("lm", "student.t", "wilcox"), maskFileName = "", 
  outputPrefix = "./ANTsR") {
  
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
  
  cat("******* Conducting ", testType, " voxel-based analysis (number of images = ", 
    numberOfImages, "). *******\n\n", sep = "")
  
  # Read the mask and place the masked voxels in the images in a matrix
  
  cat("Reading mask file ", maskFileName, "\n\n", sep = "")
  mask <- antsImageRead(maskFileName, dimensionality, pixeltype = "unsigned int")
  numberOfForegroundVoxels <- sum(c(as.array(mask)))
  
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
    dataMatrix[i, ] <- as.array(subjectImage[mask != 0])
  }
  
  # Perform the t-testing.  Monitor progress.
  tValues <- rep(NA, numberOfForegroundVoxels)
  pValues <- rep(NA, numberOfForegroundVoxels)
  
  cat("\nTesting...\n")
  
  progress <- txtProgressBar(min = 0, max = numberOfForegroundVoxels, style = 3)
  for (i in 1:numberOfForegroundVoxels) {
    testData <- cbind(dataMatrix[, i], predictors)
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
    
    if (i%%50 == 0) {
      setTxtProgressBar(progress, i)
    }
  }
  close(progress)
  cat("Done.\n", sep = "")
  
  if (testType == "lm") {
    cat("\nWriting output on predictor = ", predictorNames[1], ".\n", sep = "")
  } else {
    cat("\nWriting output.\n")
  }
  
  tImage <- antsImageClone(mask, "float")
  pImage <- antsImageClone(mask, "float")
  qImage <- antsImageClone(mask, "float")
  
  tImage[mask != 0] <- tValues
  pImage[mask != 0] <- 1 - pValues
  qImage[mask != 0] <- 1 - p.adjust(pValues, method = "fdr")
  
  outputPath <- dirname(outputPrefix)
  filePrefix <- basename(outputPrefix)
  
  dir.create(outputPath, showWarnings = TRUE, recursive = TRUE)
  
  antsImageWrite(tImage, paste(outputPath, "/", filePrefix, "tValues.nii.gz", sep = ""))
  antsImageWrite(pImage, paste(outputPath, "/", filePrefix, "1minuspValues.nii.gz", 
    sep = ""))
  antsImageWrite(qImage, paste(outputPath, "/", filePrefix, "1minuspValues_corrected.nii.gz", 
    sep = ""))
} 
