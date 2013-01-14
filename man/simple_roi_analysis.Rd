\name{simple_roi_analysis}
\alias{simple_roi_analysis}

\title{
Perform ROI population analysis between two groups.
}

\description{
Student's t-test performed for each labeled region within a specified mask to determine difference between a control group and an experimental group.
}

\usage{
roiResults <- simple_roi_analysis( dimensionality = 3, controlFileNames = c(),
  experimentalFileNames = c(), roiLabelsFileName = "" )
}

\arguments{
\item{dimensionality}{
  Dimension of images.
}

\item{controlFileNames}{
  List of image files comprising the control group.
}

\item{experimentalFileNames}{
  List of image files comprising the experimental group.
}

\item{roiLabelsFileName}{
  File name of mask defining the region over which testing is performed (foreground voxel != 0, background voxel = 0)
}

}

\value{
list consisting of unique roi labels, t-values, and p-values.
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

roiResults <- simple_roi_analysis( dimensionality = 2, controlFileNames,
  experimentalFileNames, roiLabelsFileName = "./example_images/phantomtemplate_roi_labels.nii.gz" )
}
}
