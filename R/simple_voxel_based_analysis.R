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
