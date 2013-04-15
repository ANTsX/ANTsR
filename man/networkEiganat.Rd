\name{networkEiganat}
\alias{networkEiganat}

\title{
Convenience wrapper for eigenanatomy decomposition.
}

\description{
Decomposes a matrix into sparse eigenevectors to maximize explained variance.
}

\usage{
networkEiganat( Xin, sparam = c(0.1, 0.1), k = 5, its = 100, gradparam = 1, mask = NA, v, prior, pgradparam = 0.01)
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
mydecom<-networkEiganat( mat ) 
}
}
