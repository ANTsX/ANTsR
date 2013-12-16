\name{sparseDecom2boot}
\alias{sparseDecom2boot}

\title{
Convenience wrapper for 2-view eigenanatomy decomposition w/bootstrap initialization.
}

\description{
Decomposes two matrices into paired sparse eigenevectors to maximize canonical correlation.
}

\usage{
sparseDecom2boot( inmatrix=c(NA,NA),  inmask=c(NA,NA) , sparseness=c(0.01,0.01) , nvecs=50 , cthresh=c(250,250),  its=5 , nboot = 20, nsamp = 0.9  )
}

\arguments{
\item{inmatrix}{
 input as inmatrix=list(mat1,mat2). n by p input matrix and n by q input matrix , spatial variable lies along columns. 
}

\item{inmask}{
optional pair of antsImage masks
}

\item{nboot}{
n bootstrap runs 
}

\item{nsamp}{
number of samples e.g. 0.9 indicates 90 percent of data 
}


\item{other params}{
see sccan for other parameters
}

}

\value{
outputs a decomposition of a pair of matrices
}

\author{
Avants BB
}

\examples{
\dontrun{
mat<-replicate(100, rnorm(20)) 
mat2<-replicate(100, rnorm(20)) 
mydecom<-sparseDecom2boot( inmatrix=list(mat,mat2), sparseness=c(0.1,0.3) , nvecs=3, its=3, perms=0) 
wt<-0.666
mat3<-mat*wt+mat2*(1-wt) 
mydecom<-sparseDecom2boot( inmatrix=list(mat,mat3), sparseness=c(0.2,0.2), nvecs=5, its=10, perms=200 ) 

}
}
