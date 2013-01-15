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

\item{imageFileNames}{
  List of image files.
}

\item{testType}{
  A value of 'lm', 'student.t', or 'wilcox'.  The latter two test types are assumed to be two-sampled parametric and non-parametric, respectively.
}

\item{predictors}{
  A simple vector or single column matrix or data frame specifying sample membership for student.t or wilcox testing.  For the 'lm' option, a data frame must be specified whose column names match the specified formula.
}

\item{formula}{
  Used with the 'lm' option for more sophisticated modeling.
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

images <- c( controlFileNames, experimentalFileNames )
diagnosis <- c( rep( 1, length( controlFileNames ) ), rep( 0, length( experimentalFileNames ) ) )
age <- runif( length( diagnosis ), 25, 30 )
outputPath <- "./test_output/"

prefix <- "ANTsR_t.test_"
simple_voxel_based_analysis( dimensionality = 2, imageFileNames = images,
  predictors = data.frame( diagnosis ),
  maskFileName = "./example_images/phantomtemplate_mask.nii.gz",
  outputPrefix = paste( outputPath, prefix, sep = '' ), testType = 'student.t' )

prefix <- "ANTsR_wilcox_"
simple_voxel_based_analysis( dimensionality = 2, imageFileNames = images,
  predictors = data.frame( diagnosis ),
  maskFileName = "./example_images/phantomtemplate_mask.nii.gz",
  outputPrefix = paste( outputPath, prefix, sep = '' ), testType = 'wilcox' )

prefix <- "ANTsR_lm_"
simple_voxel_based_analysis( dimensionality = 2, imageFileNames = images,
  predictors = data.frame( cbind( diagnosis, age ) ), formula = as.formula( value ~ 1 + diagnosis + age ),
  maskFileName = "./example_images/phantomtemplate_mask.nii.gz",
  outputPrefix = paste( outputPath, prefix, sep = '' ), testType = 'lm' )
}
}
