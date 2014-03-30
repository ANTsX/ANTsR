networkEiganat <- function(Xin, sparseness = c(0.1, 0.1), nvecs = 5, its = 100, gradparam = 1, mask = NA, v, prior, pgradparam = 0.01, clustval=0, downsample=T, doscale=T, domin=F, verbose=F, dowhite=F) {
  X <- Xin
  if ( doscale ) X <- scale( X ) 
  if ( domin ) X <- X - min( X )
  if ( dowhite    &  ( nvecs*2 < nrow(Xin) ) ) X<-icawhiten( X, nvecs*2 )
  if ( downsample &  ( nvecs < nrow(Xin) )   ) X<-lowrankr( X, nvecs )
  fnorm<-norm(X,"F")
  if ( verbose ) print(paste('fNormOfX',fnorm))
  if ( verbose ) print(dim(X))
  print(paste("Implements: ||  X - U V ||  +   || XP -  XV ||^2 + ell1( V ) + ell1(U)"))
  ############################ gradient 1 # U^T ( X - U V^T ) # ( X - U V^T ) V # gradient 2 # X^T ( X * ( P - V ) ) #
  if (missing(v)) {
    v <- t((replicate(ncol(X), rnorm(nvecs))))
  }
  v <- eanatsparsify(v, sparseness[2], mask, clustval=clustval )
  u <- (X %*% v)
  for (jj in 1:its) {
    for (a in 1:nrow(X)) {
      tt <- c(u[a, ])
      if (jj == 1) 
        tt <- c(u[a, ] * 1e-08 )
      if ( abs(sparseness[1]) < 1 )
          usol <- conjGradS(A = v, x_k = tt, b_in = c(X[a, ]), sp = sparseness[1])
      else usol<-coefficients(  lm( c(X[a, ]) ~ v ) )[2:(ncol(v)+1)]
      u[a, ] <- usol
    }
    v <- v + t(t(u) %*% (X - u %*% t(v))) * gradparam
    if (!missing(prior)) {
      v <- v + t(X) %*% (X %*% (prior - v)) * pgradparam
    }
    v <- eanatsparsify(v, sparseness[2], mask, clustval=clustval)
    if ( verbose ) {
      myrecon<-(u %*% t(v))
      b<-apply(X,FUN=mean,MARGIN=1)-apply(myrecon,FUN=mean,MARGIN=1)
      if (missing(prior)) 
        print(paste(jj,"Data", ( norm(X - (myrecon+b), "F")/fnorm )   ))
      if (!missing(prior)) 
        print(paste("Data", norm(X - (u %*% t(v)), "F")/fnorm, "Prior", norm(prior - v, "F")))
    }
  }
  for (a in 1:nrow(X)) {
    if ( abs(sparseness[1]) < 1 )
        usol <- conjGradS(A = v, x_k = c(u[a, ]), b_in = c(X[a, ]), sp = sparseness[1])
    else usol<-coefficients(  lm( c(X[a, ]) ~ v ) )[2:(ncol(v)+1)]
    u[a, ] <- usol
  }
  myrecon<-(u %*% t(v))
  b<-apply(X,FUN=mean,MARGIN=1)-apply(myrecon,FUN=mean,MARGIN=1)
  imglist<-list()
  if ( ! is.na(mask) ) {
    for ( j in 1:ncol(v) ) {
      img<-antsImageClone(mask)
      img[mask>0.5]<-v[,j]
      imglist<-lappend(imglist,img)
    }
  }
  return(list(u = (u), v = (v), X=X, myrecon=(myrecon+b), imglist=imglist ))
}


lowrankr <- function(A,k=2) {    
  p <- ncol(A)
  s <- svd(A,nu=k,nv=0)
  K <- t(s$u)
  X1 <- K %*% A
  return( X1 )
}


lowrank <- function(A,k=1) {
    #Calculates the SVD
    sing <- svd(A,nu=k,nv=k)
    u<-as.matrix(sing$u[, 1:k])
    v<-as.matrix(sing$v[, 1:k])
    d<-as.matrix(diag(sing$d)[1:k, 1:k])
    #Create the new approximated matrix
    return(u%*%d%*%t(v))
}

eanatsparsify <- function(vin, sparam, mask = NA, clustval = 0) {
  v <- vin
  if (class(v)[[1]][1] == "antsImage" & !is.na(mask)) 
    v <- as.matrix(vin[mask > 1e-05])
  v <- as.matrix(v)
  vpos <- eanatsparsifyv(v, sparam, mask, clustval=clustval)
  vneg <- eanatsparsifyv(v * (-1), sparam, mask, clustval=clustval)
  if (norm(vneg) > norm(vpos)) 
    return(vneg)
  return(vpos)
}

eanatsparsifyv <- function(vin, sparam, mask = NA, clustval = 0) {
  if (abs(sparam) >= 1) 
    return(vin)
  if (nrow(vin) < ncol(vin)) 
    v <- t(vin) else v <- vin
  b <- round(abs(as.numeric(sparam)) * nrow(v))
  if (b < 1) 
    b <- 1
  if (b > nrow(v)) 
    b <- nrow(v)
  for (i in 1:ncol(v)) {
    sparsev <- c(v[, i])
    ord <- order(sparsev)
    ord <- rev(ord)
    sparsev[ord[(b):length(ord)]] <- 0  # L0 penalty
    if ( !is.na(mask) ) {
      vecimg <- antsImageClone(mask)
      vecimg[mask > 0] <- sparsev
      temp<-antsImageClone( mask )
      SmoothImage(mask@dimension,vecimg,1,temp)
      ImageMath(mask@dimension, temp, "ClusterThresholdVariate", vecimg, mask, clustval)
      sparsev <- c(temp[mask > 0.5 ])
    }
    v[, i] <- sparsev
  }
  return(v)
} 
