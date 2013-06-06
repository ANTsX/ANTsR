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
\author{
Avants BB
}

\examples{
vals<-getROIValue( image, aal, mask  ) 
}
