\name{compcor}
\alias{compcor}

\title{
Simple compcor function.
}

\description{
Compcors the input matrix using SVD and returns the result.
}

\usage{
compcorr_nuisance<-compcor( fmri, mask,  ncompcor=4, variance_extreme=0.975, returnhighvarmat=F, returnv=F )
}

\arguments{
\item{mat}{
  input fmri image or matrix 
}
\item{returnv}{
returns the singular vectors in v-space
}
\item{returnhighvarmat}{
returns the high variance matrix on which svd is performed
}
}

\value{
dataframe is output
}

\author{
Avants BB
}

\examples{
mat <- matrix( rnorm(50000) ,ncol=500)
compcorrdf<-compcor( mat ) 
}
