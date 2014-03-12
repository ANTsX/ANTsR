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
# for prediction
library(spls)
library(randomForest)
data(lymphoma)
training<-sample( rep(c(TRUE,FALSE),31)  )
sp<-0.02 ; myz<-0
ldd<-sparseDecom( lymphoma$x[training,], nvecs=5 , sparseness=( sp ), mycoption=1, z=myz ) # NMF style
traindf<-data.frame( lclass=as.factor(lymphoma$y[ training  ]), eig = lymphoma$x[training,]  %*% as.matrix(ldd$eigenanatomyimages ))
testdf<-data.frame(  lclass=as.factor(lymphoma$y[ !training ]), eig = lymphoma$x[!training,] %*% as.matrix(ldd$eigenanatomyimages ))
myrf<-randomForest( lclass ~ . ,   data=traindf )
predlymp<-predict(myrf, newdata=testdf)
print(paste("N-errors:",sum(abs( testdf$lclass != predlymp ) )," non-zero ",sum(abs( ldd$eigenanatomyimages ) > 0 ) ) )
# compare to http://arxiv.org/pdf/0707.0701v2.pdf 
# now SNPs
library(randomForest)
library(BGLR)
data(mice)
snps<-mice.X
numericalpheno<-as.matrix( mice.pheno[,c(4,5,13,15) ] )
nfolds<-6
train<-sample( rep( c(1:nfolds), 1800/nfolds ) )
train<-( train < 4 )
snpd<-sparseDecom( as.matrix( snps[train,] ), nvecs=20 , sparseness=( 0.001), z=0.5 )
projmat<-as.matrix( snpd$eig ) 
traindf<-data.frame( bmi=numericalpheno[train,1] , snpse=as.matrix( snps[train, ]  ) %*% projmat )
testdf <-data.frame( bmi=numericalpheno[!train,1] , snpse=as.matrix( snps[!train, ] ) %*% projmat )
myrf<-randomForest( bmi ~ . , data=traindf )
preddf<-predict(myrf, newdata=testdf )
cor.test(preddf, testdf$bmi )
plot(preddf, testdf$bmi )
}
}

