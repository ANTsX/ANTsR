\name{simple_voxel_based_analysis}
\alias{simple_voxel_based_analysis}

\title{
Perform voxel-based population analysis between two groups.
}

\description{
Student's t-test performed at each voxel within a specified mask region to determine difference between a control group and an experimental group.
}

\usage{
simple_voxel_based_analysis( dimensionality = 3, controlFileNames = c(),
  experimentalFileNames = c(), maskFileName = "", outputPrefix = "./ANTsR_" )
}

\arguments{
To be defined in the script:

\item{dimensionality}{
  Dimension of images.
}

\item{controlFileNames}{
  List of image files comprising the control group.
}

\item{experimentalFileNames}{
  List of image files comprising the experimental group.
}

\item{maskFileName}{
  File name of mask defining the region over which testing is performed (foreground voxel != 0, background voxel = 0)
}

\item{outputPrefix}{
  Output directory and prefix to prepend to the resulting image file names (t-value
  image, "one minus" p-value image, and "one minus" (false discovery rate) corrected p-value image.
}

}

\value{
None.
}

\author{
Tustison NJ
}

\examples{
\dontrun{
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
}
}
