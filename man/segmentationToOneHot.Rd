% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oneHotSegmentation.R
\name{segmentationToOneHot}
\alias{segmentationToOneHot}
\title{One-hot encoding function}
\usage{
segmentationToOneHot(
  segmentationsArray,
  segmentationLabels = NULL,
  channelFirstOrdering = FALSE
)
}
\arguments{
\item{segmentationsArray}{multi-label numpy array}

\item{segmentationLabels}{vector of segmentation labels.  Note that a
background label (typically 0) needs to be included.}

\item{channelFirstOrdering}{Specifies the ordering of the dimensions.}
}
\value{
an n-D array of shape where the segmentation labels are expanded in 
a one-hot fashion in the channels dimension.
}
\description{
Basic one-hot transformation of segmentations array.
}
\examples{

r16 <- antsImageRead( getANTsRData( "r16" ) )
seg <- kmeansSegmentation( r16, 3 )$segmentation
oneHot <- segmentationToOneHot( as.array( seg ) )

r16 <- antsImageRead( getANTsRData( "r16" ) )
seg <- kmeansSegmentation( r16, 3 )$segmentation
oneHot <- segmentationToOneHot( as.array( seg ),
                                channelFirstOrdering = TRUE )

}
\author{
Tustison NJ
}
