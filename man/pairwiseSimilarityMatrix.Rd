\name{pairwiseSimilarityMatrix}
\alias{pairwiseSimilarityMatrix}

\title{
Simple pairwiseSimilarityMatrix function for images
}

\description{
PairwiseSimilarityMatrix contains the NImages x NImages matrix of PearsonCorrelation values after an affine registration is performed.
}

\usage{
wmat<-pairwiseSimilarityMatrix( dim, myFileList  )
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
similarity matrix is output
}

\author{
Avants BB
}

\examples{
  simmat<-pairwiseSimilarityMatrix( imagefilelist ) 
}
