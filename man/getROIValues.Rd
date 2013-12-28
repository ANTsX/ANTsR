\name{getROIValues}
\alias{getROIValues}

\title{
Compute mean value in each ROI label.
}

\description{
Computes the mean value in each label of an ROI after masking with MaskImage
}

\usage{
wmat<-getROIValues( valueImage, ROIImage, maskImage  )
}

\arguments{
\item{valueImage}{
  input image with values to average
}

\item{roiImage}{
  ROI lable image
}

\item{valueImage}{
  overall maskImage
}
}

\value{
A list with components: 
   \code{roiValues}, the (number) labels of each ROI;
   \code{roiMeans}, the mean value of each ROI; 
   \code{roiVolumes}, the volume (in voxels) of each ROI; and
   \code{roiSDs}, the standard deviation of each ROI.
}
\author{
Avants BB
}

\examples{
\dontrun{
data("aal", package = "ANTsR")
vals<-getROIValues( image, aal, mask  ) 
}
}
