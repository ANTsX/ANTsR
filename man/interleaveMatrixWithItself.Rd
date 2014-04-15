\name{interleaveMatrixWithItself}
\alias{interleaveMatrixWithItself}

\title{
Simple interleaveMatrixWithItself function.
}

\description{
Replicate columns of input matrix n times as neighbors of columns in original matrix.
}

\usage{
imat<-interleaveMatrixWithItself( mat , 5 )
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
mat<-replicate(100, rnorm(20)) 
wmat<-interleaveMatrixWithItself( mat , 5 ) 
}
