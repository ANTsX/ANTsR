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
mydecom<-networkEiganat( mat, nvecs=5 ) 
library(randomForest)
library(BGLR)
data(mice)
snps<-quantifySNPs( mice.X )
numericalpheno<-as.matrix( mice.pheno[,c(4,5,13,15) ] )
nfolds<-6
train<-sample( rep( c(1:nfolds), 1800/nfolds ) )
train<-( train < 4 )
projmat<-as.matrix( snpd$v ) 
snpdE<-sparseDecom( lowrankRowMatrix(as.matrix( snps[train,] ),40) , nvecs=40 , sparseness=( -0.001) )
snpdFast<-networkEiganat( as.matrix( snps[train,] ), nvecs=20 , sparseness=c( 1, -0.001 ) , downsample=10, verbose=T, its=20, gradparam=10 )
snpdSlow<-networkEiganat( as.matrix( snps[train,] ), nvecs=20 , sparseness=c( 1, -0.001 ) , downsample=0, verbose=T, its=20, gradparam=10 )
snpd<-snpdSlow
snpd<-snpdFast
projmat<-as.matrix( snpd$eig ) 
snpse<-as.matrix( snps[train, ]  ) %*% projmat 
traindf<-data.frame( bmi=numericalpheno[train,2] , snpse=snpse)
snpse<-as.matrix( snps[!train, ]  ) %*% projmat 
testdf <-data.frame( bmi=numericalpheno[!train,2] , snpse=snpse )
myrf<-randomForest( bmi ~ . , data=traindf )
preddf<-predict(myrf, newdata=testdf )
cor.test(preddf, testdf$bmi )
plot(preddf, testdf$bmi )
}
}
