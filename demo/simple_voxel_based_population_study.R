library( ANTsR )

# Get the image files
controlFileNames <- list.files( path = "./example_images/", pattern =
  glob2rx( "phantomtemplate_CONTROL*" ), full.names = TRUE, recursive = FALSE )
experimentalFileNames <- list.files( path = "./example_images/", pattern =
  glob2rx( "phantomtemplate_EXP*" ), full.names = TRUE, recursive = FALSE )

outputPath <- "./test_output/"
prefix <- "ANTsR_"

simple_voxel_based_analysis( dimensionality = 2, controlFileNames,
  experimentalFileNames, maskFileName = "./example_images/phantomtemplate_mask.nii.gz",
  outputPrefix = paste( outputPath, prefix, sep = '' ) );
