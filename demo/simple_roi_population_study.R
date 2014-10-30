library(ANTsR)

# Get the image files
controlFileNames <- list.files(path = "./example_images/", pattern = glob2rx("phantomtemplate_CONTROL*"), 
  full.names = TRUE, recursive = FALSE)
experimentalFileNames <- list.files(path = "./example_images/", pattern = glob2rx("phantomtemplate_EXP*"), 
  full.names = TRUE, recursive = FALSE)

images <- c(controlFileNames, experimentalFileNames)
diagnosis <- c(rep(1, length(controlFileNames)), rep(0, length(experimentalFileNames)))
age <- runif(length(diagnosis), 25, 30)
outputPath <- "./test_output/"

roiResults.ttest <- simple_roi_analysis(dimensionality = 2, imageFileNames = images, 
  predictors = data.frame(diagnosis), roiLabelsFileName = "./example_images/phantomtemplate_roi_labels.nii.gz", 
  testType = "student.t")

roiResults.wilcox <- simple_roi_analysis(dimensionality = 2, imageFileNames = images, 
  predictors = data.frame(diagnosis), roiLabelsFileName = "./example_images/phantomtemplate_roi_labels.nii.gz", 
  testType = "wilcox")

roiResults.lm <- simple_roi_analysis(dimensionality = 2, imageFileNames = images, 
  predictors = data.frame(cbind(diagnosis, age)), formula = as.formula(value ~ 
    1 + diagnosis + age), roiLabelsFileName = "./example_images/phantomtemplate_roi_labels.nii.gz", 
  testType = "lm") 
