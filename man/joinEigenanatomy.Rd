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
  target graph density or densities to search over
}

}

\value{
  return(list(fusedlist = newelist, fusedproj = myproj, memberships = communitymembership , graph=gg, bestdensity=graphdensity ))
}

\author{
Avants BB
}

\examples{
# if you dont have images 
mat<-replicate(100, rnorm(20)) 
mydecom<-sparseDecom( mat ) 
kk<-joinEigenanatomy( mat, mask=NA, mydecom$eigenanatomyimages , 0.1 )
# or select optimal parameter from a list
kk<-joinEigenanatomy( mat, mask=NA, mydecom$eigenanatomyimages , c(1:10)/50 )
print(dim( mydecom$eigenanatomyimages ) )
print(dim( t(kk$fusedlist ) ) )
# something similar may be done with images 
mask<-as.antsImage( t(as.matrix(array(rep(1,ncol(mat)),ncol(mat)))) )
mydecom<-sparseDecom( mat, inmask=mask ) 
kki<-joinEigenanatomy( mat, mask=mask, mydecom$eigenanatomyimages , 0.1 )
print(length( mydecom$eigenanatomyimages ) )
print(length( kki$fusedlist ) )
mydecomf<-sparseDecom( mat, inmask=mask, initializationList=kki$fusedlist , 
  sparseness=0, nvecs=length(kki$fusedlist) ) 
}
