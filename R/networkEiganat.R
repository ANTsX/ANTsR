#' Convenience wrapper for eigenanatomy decomposition.
#'
#' Decomposes a matrix into sparse eigenevectors to maximize explained
#' variance.
#'
#' @param Xin n by p input images , subjects or time points by row ,
#' spatial variable lies along columns
#' @param sparseness sparseness pair c( 0.1 , 0.1 )
#' @param nvecs number of vectors
#' @param its number of iterations
#' @param gradparam gradient descent parameter for data
#' @param mask optional antsImage mask
#' @param v the spatial solultion
#' @param prior the prior
#' @param pgradparam  gradient descent parameter for prior term
#' @param clustval integer greater than or equal to zero
#' @param downsample bool
#' @param doscale bool
#' @param domin bool
#' @param verbose bool
#' @param dowhite bool
#' @param timeme bool
#' @param addb bool
#' @param useregression bool
#' @return outputs a decomposition of a population or time series matrix
#' @author Avants BB
#' @examples
#'
#' \dontrun{
#' mat<-replicate(100, rnorm(20))
#' mydecom<-networkEiganat( mat, nvecs=5 )
#' ch1<-usePkg('randomForest')
#' ch2<-usePkg('BGLR')
#' if ( ch1 & ch2 ) {
#' data(mice)
#' snps<-quantifySNPs( mice.X )
#' numericalpheno<-as.matrix( mice.pheno[,c(4,5,13,15) ] )
#' numericalpheno<-residuals( lm( numericalpheno ~
#'    as.factor(mice.pheno$Litter) ) )
#' phind<-3
#' nfolds<-6
#' train<-sample( rep( c(1:nfolds), 1800/nfolds ) )
#' train<-( train < 4 )
#' lowr<-lowrankRowMatrix(as.matrix( snps[train,] ),900)
#' snpdS<-sparseDecom( lowr , nvecs=2 , sparseness=( -0.001), its=3  )
#' snpdF<-sparseDecom( lowrankRowMatrix(as.matrix( snps[train,] ),100),
#'   nvecs=2 , sparseness=( -0.001), its=3 )
#' projmat<-as.matrix( snpdS$eig )
#' projmat<-as.matrix( snpdF$eig )
#' snpdFast<-networkEiganat( as.matrix( snps[train,] ), nvecs=2 ,
#'   sparseness=c( 1, -0.001 ) , downsample=45, verbose=T, its=3,
#'   gradparam=10 )
#' snpdSlow<-networkEiganat( as.matrix( snps[train,] ), nvecs=2 ,
#'   sparseness=c( 1, -0.001 ) , downsample=0, verbose=T,
#'   its=3, gradparam=10 )
#' snpd<-snpdSlow
#' snpd<-snpdFast
#' projmat<-as.matrix( snpd$v )
#' snpdF<-sparseDecom( lowrankRowMatrix(as.matrix( snps[train,] ),10) ,
#'   nvecs=2 , sparseness=( -0.001), its=3  )
#' projmat<-as.matrix( snpdS$eig )
#' snpse<-as.matrix( snps[train, ]  ) %*% projmat
#' traindf<-data.frame( bmi=numericalpheno[train,phind] , snpse=snpse)
#' snpse<-as.matrix( snps[!train, ]  ) %*% projmat
#' testdf <-data.frame( bmi=numericalpheno[!train,phind] , snpse=snpse )
#' myrf<-glm( bmi ~ . , data=traindf )
#' preddf<-predict(myrf, newdata=testdf )
#' cor.test(preddf, testdf$bmi )
#' if ( usePkg('visreg') ) {
#' mydf<-data.frame( PredictedBMIfromSNPs=preddf, RealBMI=testdf$bmi )
#' mymdl<-lm( PredictedBMIfromSNPs ~ RealBMI, data=mydf)
#' visreg::visreg(mymdl) }
#' ###########
#' # vs glmnet #
#' ###########
#' haveglm<-usePkg('glmnet')
#' if ( haveglm ) {
#' kk<-glmnet(y=numericalpheno[train,phind],x=snps[train,] )
#' ff<-predict(kk,newx=snps[!train,])
#' cor.test(ff[,25],numericalpheno[!train,phind])
#' mydf<-data.frame( PredictedBMIfromSNPs=ff[,25], RealBMI=testdf$bmi )
#' mymdl<-lm( PredictedBMIfromSNPs ~ RealBMI, data=mydf)
#' } # glmnet check
#' } # ch1 and ch2
#' ###########
#' }
#'
#' @export networkEiganat
networkEiganat <- function(
  Xin,
  sparseness = c(0.1, 0.1),
  nvecs = 5,
  its = 5,
  gradparam = 1,
  mask = NA,
  v,
  prior,
  pgradparam = 0.1,
  clustval = 0,
  downsample = 0,
  doscale = T,
  domin = T,
  verbose = F,
  dowhite = 0,
  timeme = T,
  addb = T,
  useregression = T) {
  X <- Xin/norm(Xin, "F")
  if (dowhite > 0 & (nvecs * 2 < nrow(Xin)))
    X <- icawhiten(X, dowhite)
  if (downsample > 0 & (nvecs < nrow(Xin)))
    X <- lowrankRowMatrix(X, downsample)
  if (doscale) {
    X <- scale(X)
    X <- X/norm(X, "F")
  }
  if (domin)
    X <- X - min(X)
  fnorm <- norm(X, "F")
  if (verbose)
    print(paste("fNormOfX", fnorm))
  if (verbose)
    print(dim(X))
  if (verbose)
    print(paste("Implements: ||  X - U V ||  +   || XP -  XV ||^2 + ell0( V ) + ell0(U)"))
  ############################ gradient 1 # U^T ( X - U V^T ) # ( X - U V^T ) V # gradient 2 # X^T ( X * ( P -
  ############################ V ) ) #
  if (missing(v)) {
    v <- t((replicate(ncol(X), rnorm(nvecs))))
    v <- svd(Xin, nu = 0, nv = nvecs)$v
  }
  v <- .eanatsparsify(v, sparseness[2], mask, clustval = clustval)
  u <- (X %*% v)
  time1 <- (Sys.time())
  for (jj in 1:its) {
    myrecon <- (u %*% t(v))
    b <- apply(X, FUN = mean, MARGIN = 1) - apply(myrecon, FUN = mean, MARGIN = 1)
    if (addb)
      myrecon <- myrecon + b
    v <- v + t(t(u) %*% (X - myrecon)) * gradparam
    if (!missing(prior)) {
      v <- v + t(X) %*% (X %*% (prior - v)) * pgradparam
    }
    v <- .eanatsparsify(v, sparseness[2], mask, clustval = clustval)
    if (!useregression) {
      uupdate <- t(t(v) %*% t(X - myrecon))
      u <- u + uupdate * gradparam
    }
    if (useregression)
      for (a in 1:nrow(X)) {
        tt <- c(u[a, ])
        # if ( abs(sparseness[1]) < 1 ) usol <- .conjGradS(A = v, x_k = tt, b_in = c(X[a,
        # ]), sp = sparseness[1]) else
        usol <- as.numeric(coefficients(lm(c(X[a, ]) ~ v))[2:(ncol(v) + 1)])
        u[a, ] <- usol
      }

    if (abs(sparseness[1]) < 1) {
      u <- whiten(u)
      u <- .eanatsparsify(u, sparseness[1], verbose = F)
    }
    if (is.na(norm(u))) {
      if (verbose)
        print(paste("Warning: nan u-norm, resetting u. Advisable to decrease sparseness"))
      u <- t(X %*% v)
    }
    if (verbose) {
      if (missing(prior))
        print(paste(jj, "Data", (norm(X - (myrecon), "F")/fnorm)))
      if (!missing(prior))
        print(paste("Data", norm(X - (myrecon), "F")/fnorm, "Prior", norm(prior -
          v, "F")))
    }
  }
  myrecon <- (u %*% t(v))
  b <- apply(X, FUN = mean, MARGIN = 1) - apply(myrecon, FUN = mean, MARGIN = 1)
  imglist <- list()
  if (!is.na(mask)) {
    for (j in 1:ncol(v)) {
      img <- antsImageClone(mask)
      img[mask > 0.5] <- v[, j]
      imglist <- lappend(imglist, img)
    }
  }
  mytime <- (Sys.time() - time1)
  return(list(u = (u), v = (v), X = X, myrecon = (myrecon + b), eigenanatomyimages = imglist,
    computationtime = mytime))
}



#' Produces a low rank version of the input matrix
#'
#' @param A input matrix
#' @param k rank to use
#' @param faster boolean
#' @return matrix is output
#' @author Avants BB
#' @examples
#'
#' mat <- matrix(rnorm(300),ncol=50)
#' lrmat <- lowrankRowMatrix( mat , 2 )
#'
#' @export lowrankRowMatrix
lowrankRowMatrix <- function(A, k = 2, faster=FALSE ) {
  if (k > nrow(A))
    return(A)
  if ( usePkg("rsvd") & faster )
  {
    s <- rsvd::rsvd( A, k )
    K <- t(s$u) # %*% diag(s$D[1:k]))
  } else {
    s <- svd(A, nu = k, nv = 0)
    K <- t(s$u) # %*% diag(s$d[1:k]) )
  }
  X1 <- K %*% A
  return(X1)
}


.lowrank <- function(A, k = 1) {
  # Calculates the SVD
  sing <- svd(A, nu = k, nv = k)
  u <- as.matrix(sing$u[, 1:k])
  v <- as.matrix(sing$v[, 1:k])
  d <- as.matrix(diag(sing$d)[1:k, 1:k])
  # Create the new approximated matrix
  return(u %*% d %*% t(v))
}

.eanatcolMaxs <- function(v) {
  if (class(v) == "matrix") {
    return(apply(v, FUN = max, MARGIN = 2))
  } else return(v)
}

.eanatsparsify <- function(vin, sparam, mask = NA, clustval = 0, verbose = F) {
  if (abs(sparam) >= 1)
    return(vin)
  v <- vin
  v <- v * sign(.eanatcolMaxs(v))
  if (class(v)[[1]][1] == "antsImage" & !is.na(mask))
    v <- as.matrix(vin[mask > 1e-05])
  v <- as.matrix(v)
  vpos <- .eanatsparsifyv(v, sparam, mask, clustval = clustval, verbose = verbose)
  return(vpos)
}

.eanatsparsifyv <- function(vin, sparam, mask = NA, clustval = 0, verbose = F) {
  if (abs(sparam) >= 1)
    return(vin)
  if (nrow(vin) < ncol(vin))
    v <- t(vin) else v <- vin
  v <- v * sign(.eanatcolMaxs(v))
  b <- round(abs(as.numeric(sparam)) * nrow(v))
  if (b < 3)
    b <- 3
  if (b > nrow(v))
    b <- nrow(v)
  for (i in 1:ncol(v)) {
    sparsev <- c(v[, i])
    if (verbose)
      print(paste(" sparam ", sparam))
    if (sparam < 0)
      ord <- order(abs(sparsev)) else {
      sparsev[sparsev < 0] <- 0
      ord <- order(sparsev)
    }
    ord <- rev(ord)
    sparsev[ord[(b):length(ord)]] <- 0  # L0 penalty
    if (verbose)
      print(paste(sparsev))
    if (!is.na(mask)) {
      vecimg <- antsImageClone(mask)
      vecimg[mask > 0] <- sparsev
      temp <- antsImageClone(mask)
      temp<-smoothImage( vecimg, 1 )
      temp[ mask < 0.5 ]<-0
      temp2<-labelClusters( temp, 1000, minThresh = 0.1, maxThresh=0.5 )
      temp[ temp2 < 1 ]<-0
      sparsev <- c(temp[mask > 0.5])
    }
    v[, i] <- sparsev
  }
  return(v)
}
