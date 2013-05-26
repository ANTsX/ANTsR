\name{pairwiseImageDistanceMatrix}
\alias{pairwiseImageDistanceMatrix}

\title{
Simple pairwiseImageDistanceMatrix function for images
}

\description{
Output contains the NImages x NImages matrix of c("PearsonCorrelation","Mattes") or any Image Metric values available in ImageMath.  Similarity is computed after an affine registration is performed.  You can also cluster the images via the dissimilarity measurement, i.e. the negated similarity metric.  So, the estimated dissimilarity is returned in the matrix.
}

\usage{
wmat<-pairwiseImageDistanceMatrix( dim , myFileList, metrictype="PearsonCorrelation", nclusters = NA  )
}

\arguments{
\item{ dim }{
 imageDimension
}
\item{myFileList}{
  dd<-"MICCAI-2013-SATA-Challenge-Data/CAP/training-images/"
  myFileList<-list.files(path=dd, pattern = glob2rx("*nii.gz"),full.names = T,recursive = T)
}
}

\value{
raw dissimilarity matrix is output, symmetrized matrix and clustering (optional) in a list
}

\author{
Avants BB
}

\examples{
  dsimdata<-pairwiseImageDistanceMatrix( 3, imagefilelist, nclusters = 5 ) 
}
