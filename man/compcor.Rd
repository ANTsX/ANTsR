\name{compcor}
\alias{compcor}

\title{
Simple compcor function.
}

\description{
Compcors the input matrix using SVD and returns the result.
}

\usage{
compcorr_nuisance<-compcor( fmri, mask,  ncompcor=4, variance_extreme=0.975, randomSamples=1 )
}

\arguments{
\item{mat}{
  input fmri image or matrix 
}
}

\value{
dataframe is output
}

\author{
Avants BB
}

\examples{
mat <- matrix(c(rep(1,100),rep(0,200)),ncol=50)
compcorrdf<-compcor( mat ) 
}
