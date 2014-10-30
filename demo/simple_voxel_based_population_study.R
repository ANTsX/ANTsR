library(ANTsR)

# Get the image files
controlFileNames <- list.files(path = "../../example_images/", pattern = glob2rx("phantomtemplate_CONTROL*"), 
  full.names = TRUE, recursive = FALSE)
experimentalFileNames <- list.files(path = "../..//example_images/", pattern = glob2rx("phantomtemplate_EXP*"), 
  full.names = TRUE, recursive = FALSE)

images <- c(controlFileNames, experimentalFileNames)
diagnosis <- c(rep(1, length(controlFileNames)), rep(0, length(experimentalFileNames)))
age <- runif(length(diagnosis), 25, 30)
outputPath <- "../../test_output/"

prefix <- "ANTsR_t.test_"
simple_voxel_based_analysis(dimensionality = 2, imageFileNames = images, predictors = data.frame(diagnosis), 
  maskFileName = "./example_images/phantomtemplate_mask.nii.gz", outputPrefix = paste(outputPath, 
    prefix, sep = ""), testType = "student.t")

prefix <- "ANTsR_wilcox_"
simple_voxel_based_analysis(dimensionality = 2, imageFileNames = images, predictors = data.frame(diagnosis), 
  maskFileName = "./example_images/phantomtemplate_mask.nii.gz", outputPrefix = paste(outputPath, 
    prefix, sep = ""), testType = "wilcox")

prefix <- "ANTsR_lm_"
simple_voxel_based_analysis(dimensionality = 2, imageFileNames = images, predictors = data.frame(cbind(diagnosis, 
  age)), formula = as.formula(value ~ 1 + diagnosis + age), maskFileName = "./example_images/phantomtemplate_mask.nii.gz", 
  outputPrefix = paste(outputPath, prefix, sep = ""), testType = "lm") 
