\name{simple_roi_analysis}
\alias{simple_roi_analysis}

\title{
Perform ROI population analysis between two groups.
}

\description{
Student's t-test performed for each labeled region within a specified mask to determine difference between a control group and an experimental group.
}

\usage{
simple_roi_analysis
}

\arguments{
To be defined in the script:

\item{dimensionality}{
  Dimension of images.
}

\item{inputPath}{
  Location of experimental and control files.
}

\item{controlsFileNamePrefix}{
  Prefix of control image file names.
}

\item{experimentalsFileNamePrefix}{
  Prefix of experimental image file names.
}

\item{roiLabelsFileName}{
  File name of mask defining the region over which testing is performed (foreground voxel != 0, background voxel = 0)
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
  simple_roi_analysis
}
}
