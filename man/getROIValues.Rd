\name{getROIValue}
\alias{getROIValue}

\title{
Compute mean value in each ROI label.
}

\description{
Computes the mean value in each label of an ROI after masking with MaskImage
}

\usage{
wmat<-getROIValue( valueImage, ROIImage, maskImage  )
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
   \code{roiMeans}, the mean value of each ROI; and
   \code{roiVolumes}, the volume (in voxels) of each ROI.
}
\author{
Avants BB
}

\examples{
vals<-getROIValue( image, aal, mask  ) 
}
