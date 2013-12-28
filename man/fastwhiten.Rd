\name{fastwhiten}
\alias{fastwhiten}

\title{
Simple fastwhitening function.
}

\description{
whitens the input matrix using a fast SVD and returns the result.  can select number of principal components to use.  might want to scale the matrix before input.
}

\usage{
wmat<-fastwhiten( mat , 50 )
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
\dontrun{
mat<-replicate(400, rnorm(600)) 
print(mean(abs(cor(mat))))
wmat<-fastwhiten( scale( mat ) ) 
print(mean(abs(cor(wmat))))
wmat<-fastwhiten( scale( mat ), 20 ) 
print(mean(abs(cor(wmat))))
}
}
