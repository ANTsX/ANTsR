\name{sparseDecom}
\alias{sparseDecom}

\title{
Convenience wrapper for eigenanatomy decomposition.
}

\description{
Decomposes a matrix into sparse eigenevectors to maximize explained variance.
}

\usage{
sparseDecom( inmatrix=NA,  inmask=NA , sparseness=0.01 , nvecs=50 , its=5 , cthresh=250 )
}

\arguments{
\item{inmatrix}{
  n by p input images , subjects or time points by row , spatial variable lies along columns
}

\item{inmask}{
optional antsImage mask
}

\item{other params}{
see sccan for other parameters
}

}

\value{
outputs a decomposition of a population or time series matrix
}

\author{
Avants BB
}

\examples{
\dontrun{
mat<-replicate(100, rnorm(20)) 
mydecom<-sparseDecom( mat ) 
}
}
