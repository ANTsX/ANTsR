\name{simple_voxel_based_analysis}
\alias{simple_voxel_based_analysis}

\title{
Perform voxel-based population analysis between two groups.
}

\description{
Student's t-test performed at each voxel within a specified mask region to determine difference between a control group and an experimental group.
}

\usage{
simple_voxel_based_analysis
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

\item{maskFileName}{
  File name of mask defining the region over which testing is performed (foreground voxel != 0, background voxel = 0)
}

\item{outputPath}{
  Path where result files should be written (t-statistic image and 1 - p-value image).
}

\item{outputPrefix}{
  Prefix to prepend to the resulting image file names.
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
  simple_voxel_based_analysis
}
}
