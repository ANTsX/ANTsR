\name{joinEigenanatomy}
\alias{joinEigenanatomy}

\title{
Simple joinEigenanatomy function.
}

\description{
joinEigenanatomy joins the input matrix using a community membership approach.
}

\usage{
wmat<-joinEigenanatomy( datamatrix, mask , listEanatImages , gdensity = 0.65  )
}

\arguments{

\item{datamatrix}{
  input matrix before decomposition 
}

\item{mask}{
  mask used to create datamatrix 
}

\item{listEanatImages}{
  list containing pointers to eanat images 
}

\item{gdensity}{
  target graph density 
}

}

\value{
matrix is output
}

\author{
Avants BB
}

\examples{
# if you dont have images 
mat<-replicate(100, rnorm(20)) 
mydecom<-sparseDecom( mat ) 
kk<-joinEigenanatomy( mat, mask=NA, mydecom$eigenanatomyimages , 0.1 )
print(dim( mydecom$eigenanatomyimages ) )
print(dim( t(kk$fusedlist ) ) )
# something similar may be done with images 
}
