library( ANTsR )

roiResults <- simple_roi_analysis( dimensionality = 2, inputPath = "./example_images/",
  controlsFileNamePrefix = "phantomtemplate_CONTROL",
  experimentalsFileNamePrefix = "phantomtemplate_EXP",
  roiLabelsFileName = "./example_images/phantomtemplate_roi_labels.nii.gz" )
