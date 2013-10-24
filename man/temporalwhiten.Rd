\name{temporalwhiten}
\alias{temporalwhiten}

\title{
Simple autocorrelation-based temporal whitening function.
}

\description{
Temporally whitens the input matrix using autoregression and returns the result.
}

\usage{
wmat<-temporalwhiten( mat  )
}

\arguments{
\item{mat}{
  input matrix
}
}

\value{
matrix is output
}

\author{
Avants BB
}

\examples{
mat <- matrix(c(rep(1,100),rep(0,200)),ncol=50)
wmat<-temporalwhiten( mat ) 
}
