\name{pairwiseSimilarityMatrix}
\alias{pairwiseSimilarityMatrix}

\title{
Simple pairwiseSimilarityMatrix function for images
}

\description{
PairwiseSimilarityMatrix contains the NImages x NImages matrix of c("PearsonCorrelation","Mattes") or any Image Metric values available in ImageMath.  Similarity is computed after an affine registration is performed.  You can also cluster the images via the similarity measurement.  In fact, dissimilarity is returned in the matrix.
}

\usage{
wmat<-pairwiseSimilarityMatrix( dim , myFileList, metrictype="PearsonCorrelation", nclusters = NA  )
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
raw similarity matrix is output, symmetrized matrix and clustering (optional) in a list
}

\author{
Avants BB
}

\examples{
  simdata<-pairwiseSimilarityMatrix( 3, imagefilelist, nclusters = 5 ) 
}
