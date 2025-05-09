% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oneHotSegmentation.R
\name{oneHotToSegmentation}
\alias{oneHotToSegmentation}
\title{Inverse one-hot transformation}
\usage{
oneHotToSegmentation(oneHotArray, domainImage, channelFirstOrdering = FALSE)
}
\arguments{
\item{oneHotArray}{an array where the channel dimension contains the 
one-hot encoding.}

\item{domainImage}{image definining the geometry of the returned probability
images.}

\item{channelFirstOrdering}{Specifies the ordering of the dimensions.}
}
\value{
a list of probability images.
}
\description{
Inverse of basic one-hot transformation of segmentations array
}
\examples{

r16 <- antsImageRead( getANTsRData( "r16" ) )
seg <- kmeansSegmentation( r16, 3 )$segmentation
oneHot <- segmentationToOneHot( as.array( seg ) )
oneHotInv <- oneHotToSegmentation( oneHot, seg )

r16 <- antsImageRead( getANTsRData( "r16" ) )
seg <- kmeansSegmentation( r16, 3 )$segmentation
oneHot <- segmentationToOneHot( as.array( seg ),
                                channelFirstOrdering = TRUE )
oneHotInv <- oneHotToSegmentation( oneHot, seg,
                                   channelFirstOrdering = TRUE ) 

}
\author{
Tustison NJ
}
