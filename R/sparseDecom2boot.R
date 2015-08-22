#' Convenience wrapper for 2-view eigenanatomy decomposition w/bootstrap
#' initialization.
#'
#' Decomposes two matrices into paired sparse eigenevectors to maximize
#' canonical correlation.
#'
#'
#' @param inmatrix input as inmatrix=list(mat1,mat2). n by p input matrix and n
#' by q input matrix , spatial variable lies along columns.
#' @param inmask optional pair of antsImage masks
#' @param sparseness a c(.,.) pair of values e.g c(0.01,0.1) enforces an
#' unsigned 99 percent and 90 percent sparse solution for each respective view
#' @param nvecs number of eigenvector pairs
#' @param its number of iterations, 10 or 20 usually sufficient
#' @param cthresh cluster threshold pair
#' @param statdir temporary directory if you want to look at full output
#' @param perms number of permutations
#' @param uselong enforce solutions of both views to be the same - requires
#'  matrices to be the same size
#' @param z subject space (low-dimensional space) sparseness value
#' @param smooth smooth the data (only available when mask is used)
#' @param robust rank transform input matrices
#' @param mycoption enforce 1 - spatial orthogonality, 2 - low-dimensional
#' orthogonality or 0 - both
#' @param initializationList initialization for first view
#' @param initializationList2 initialization for 2nd view
#' @param ell1 gradient descent parameter, if negative then l0 otherwise use l1
#' @param nboot n bootstrap runs
#' @param nsamp number of samples e.g. 0.9 indicates 90 percent of data
#' @param doseg boolean to control matrix orthogonality during bootstrap
#' @param priorWeight Scalar value weight on prior between 0 (prior is weak)
#' and 1 (prior is strong).  Only engaged if initialization is used
#' @param verbose activates verbose output to screen
#' @return outputs a decomposition of a pair of matrices
#' @author Avants BB
#' @examples
#'
#' \dontrun{
#' mat<-replicate(100, rnorm(20))
#' mat2<-replicate(100, rnorm(20))
#' mydecom<-sparseDecom2boot( inmatrix=list(mat,mat2),
#'   sparseness=c(0.1,0.3) , nvecs=3, its=3, perms=0)
#' wt<-0.666
#' mat3<-mat*wt+mat2*(1-wt)
#' mydecom<-sparseDecom2boot( inmatrix=list(mat,mat3),
#'   sparseness=c(0.2,0.2), nvecs=5, its=10, perms=200 )
#'
#' }
#'
#' @export sparseDecom2boot
sparseDecom2boot <- function(inmatrix, inmask = c(NA, NA),
  sparseness = c(0.01, 0.01),
  nvecs = 50, its = 5, cthresh = c(0, 0), statdir = NA, perms = 0,
  uselong = 0,
  z = 0, smooth = 0, robust = 0, mycoption = 1,
  initializationList = list(), initializationList2 = list(),
  ell1 = 0.05, nboot = 10, nsamp = 1, doseg = FALSE,
  priorWeight = 0.0, verbose=FALSE ) {
  numargs <- nargs()
  if (numargs < 1 | missing(inmatrix)) {
    print(args(sparseDecom2boot))
    return(0)
  }
  nsubj <- nrow(inmatrix[[1]])
  mysize <- round(nsamp * nsubj)
  mat1 <- inmatrix[[1]]
  mat2 <- inmatrix[[2]]
  mymask <- inmask
  cca1out <- 0
  cca2out <- 0
  cca1outAuto <- 0
  cca2outAuto <- 0
  bootccalist1 <- list()
  bootccalist2 <- list()
  nsubs = nrow(mat1)
  allmat1 = matrix( ncol=nvecs*nboot, nrow=ncol(mat1) )
  allmat2 = matrix( ncol=nvecs*nboot, nrow=ncol(mat2) )
  for (i in 1:nvecs) {
    makemat <- matrix(rep(0, nboot * ncol(mat1)), ncol = ncol(mat1))
    bootccalist1 <- lappend(bootccalist1, makemat)
    makemat <- matrix(rep(0, nboot * ncol(mat2)), ncol = ncol(mat2))
    bootccalist2 <- lappend(bootccalist2, makemat)
  }
  if (nsamp >= 0.999999999)
    doreplace <- TRUE else doreplace <- FALSE
  for (boots in 1:nboot) {
    mysample <- sample(1:nsubj, size = mysize, replace = doreplace)
    submat1 <- mat1[mysample, ]
    submat2 <- mat2[mysample, ]
    sublist <- list(submat1, submat2)
#    print(paste("boot", boots, "sample", mysize))
    (myres <- sparseDecom2(
      inmatrix = sublist,
      inmask = mymask,
      sparseness = sparseness,
      nvecs = nvecs,
      its = its,
      cthresh = cthresh,
      perms = 0,
      uselong = uselong,
      z = z,
      smooth = smooth,
      robust = robust,
      mycoption = mycoption,
      initializationList = initializationList,
      initializationList2 = initializationList2,
      ell1 = ell1,
      verbose = verbose ))
    myressum <- abs(diag(cor(myres$projections, myres$projections2)))
    cca1 <- (myres$eig1)
    cca2 <- (myres$eig2)
    if (boots > 1 & TRUE) {
      cca1copy <- cca1
      mymult <- matrix(rep(0, ncol(cca1) * ncol(cca1)), ncol = ncol(cca1))
      for (j in 1:ncol(cca1out)) {
        for (k in 1:ncol(cca1)) {
          temp1 <- abs(cca1out[, j])
          temp2 <- abs(cca1[, k])
          mymult[j, k] <- .cosineDist(temp1, temp2)
          # sum( abs( temp1/sum(temp1) - temp2/sum(temp2) ) ) mymult[j,k]<-( -1.0 * cor(
          # temp1, temp2 ) )
        }
      }
      for (ct in 1:(ncol(cca1))) {
        arrind <- which(mymult == min(mymult), arr.ind = T)
        cca1copy[, arrind[1]] <- cca1[, arrind[2]]
        mymult[arrind[1], ] <- 0
        mymult[, arrind[2]] <- 0
      }
      cca1 <- cca1copy
      ###### nextview######
      cca2copy <- cca2
      mymult <- matrix(rep(0, ncol(cca2) * ncol(cca2)), ncol = ncol(cca2))
      for (j in 1:ncol(cca2out)) {
        for (k in 1:ncol(cca2)) {
          temp1 <- abs(cca2out[, j])
          temp2 <- abs(cca2[, k])
          mymult[j, k] <- .cosineDist(temp1, temp2)
          # mymult[j,k]<-sum( abs( temp1/sum(temp1) - temp2/sum(temp2) ) ) mymult[j,k]<-(
          # -1.0 * cor( temp1, temp2 ) )
        }
      }
      for (ct in 1:(ncol(cca2))) {
        arrind <- which(mymult == min(mymult), arr.ind = T)
        cca2copy[, arrind[1]] <- cca2[, arrind[2]]
        mymult[arrind[1], ] <- 0
        mymult[, arrind[2]] <- 0
      }
      cca2 <- cca2copy
    }
    cca1out <- cca1out + (cca1) # * myressum
    cca2out <- cca2out + (cca2) # * myressum
    bootInds = (( boots - 1 )*nvecs+1):(boots*nvecs)
    allmat1[  ,  bootInds ] = (cca1)
    allmat2[  ,  bootInds ] = (cca2)
    for (nv in 1:nvecs) {
      # if (sparseness[1] > 0)
        bootccalist1[[nv]][boots, ] <- (cca1[, nv])
        # else bootccalist1[[nv]][boots, ] <- (cca1[, nv])
      # if (sparseness[2] > 0)
        bootccalist2[[nv]][boots, ] <- (cca2[, nv])
        # else bootccalist2[[nv]][boots, ] <- (cca2[, nv])
    }
  }

  if ( doseg )
  for ( k in 1:nvecs )
    {
    cca1out[,k] =
      .eanatsparsify( abs(cca1out[,k]), sparseness[1] )
    cca2out[,k] =
      .eanatsparsify( abs(cca2out[,k]), sparseness[2] )
#    zz = abs( cca1out[,k] ) < 0.2
#    cca1out[zz,k] = 0
#    zz = abs( cca2out[,k] ) < 0.2
#    cca2out[zz,k] = 0
    }
  init1 = initializeEigenanatomy( t( cca1out ), inmask[[1]] )
  init2 = initializeEigenanatomy( t( cca2out ), inmask[[2]] )
  ccaout = sparseDecom2(
    inmatrix = inmatrix,
    inmask=c( init1$mask, init2$mask ),
    sparseness = sparseness,
    nvecs = nvecs,
    its = its,
    cthresh = cthresh,
    perms = perms,
    uselong = uselong,
    z = z,
    smooth = smooth,
    robust = robust,
    mycoption = mycoption,
    initializationList=init1$initlist,
    initializationList2=init2$initlist,
    ell1 = ell1,
    priorWeight=priorWeight,
    verbose=verbose )
  jh=matrix( 0, nrow=ncol(inmatrix[[1]]), ncol=ncol(inmatrix[[2]]) )
  colnames(jh)=colnames(inmatrix[[2]])
  rownames(jh)=colnames(inmatrix[[1]])
  for ( i in 1:ncol(allmat1) )
    {
    wh1=which( abs( allmat1[,i] ) > 1.e-10 )
    wh2=which( abs( allmat2[,i] ) > 1.e-10 )
    jh[ wh1, wh2 ] = jh[ wh1, wh2 ] + 1
    }
  return(
    list(
      bootsccan=ccaout,
      cca1boot=cca1out,
      cca2boot=cca2out,
      bootccalist1=bootccalist1,
      bootccalist2=bootccalist2,
      allmat1=allmat1,
      allmat2=allmat2,
      init1=init1,
      init2=init2,
      jh=jh )
    )
##### old implementation below #####
  cca1outAuto <- cca1out
  cca2outAuto <- cca2out
  for (nv in 1:nvecs) {
    bootmat <- bootccalist1[[nv]]
    vec1 <- apply(bootmat, FUN = mean, MARGIN = 2)
    # mysd1 <- apply(bootmat,FUN=sd,MARGIN=2) vec1[ mysd1 > 0 ] <- vec1[ mysd1 > 0 ]
    # / mysd1[ mysd1 > 0 ] vec1 <- apply(bootmat,FUN=t.test,MARGIN=2) vec1 <-
    # as.numeric( do.call(rbind, vec1)[,1] )
    vec1[is.na(vec1)] <- 0
    if ((nv > 1) & (abs(sparseness[1] * nvecs) < 1) & TRUE) {
      for (j in 1:(nv - 1)) {
        prevec <- cca1out[, j]
        vec1[prevec > 0] <- 0
      }
      cca1out[, nv] <-.eanatsparsify(vec1, abs(sparseness[1]))
      cca1outAuto[, nv] <- cca1out[, nv] # vec1
    } else {
      cca1out[, nv] <-.eanatsparsify(vec1, abs(sparseness[1]))
      cca1outAuto[, nv] <- cca1out[, nv] # vec1
    }
    ### now vec 2 ###
    bootmat <- bootccalist2[[nv]]
    vec2 <- apply(bootmat, FUN = mean, MARGIN = 2)
    # mysd2 <- apply(bootmat,FUN=sd,MARGIN=2) vec2[ mysd2 > 0 ] <- vec2[ mysd2 > 0 ]
    # / mysd2[ mysd2 > 0 ] vec2 <- apply(bootmat,FUN=t.test,MARGIN=2) vec2 <-
    # as.numeric( do.call(rbind, vec2)[,1] )
    vec2[is.na(vec2)] <- 0
    if ((nv > 1) & (abs(sparseness[2] * nvecs) < 1) & TRUE) {
      for (j in 1:(nv - 1)) {
        prevec <- cca2out[, j]
        vec2[prevec > 0] <- 0
      }
      cca2out[, nv] <-.eanatsparsify(vec2, abs(sparseness[2]))
      cca2outAuto[, nv] <- vec2
    } else {
      cca2out[, nv] <-.eanatsparsify(vec2, abs(sparseness[2]))
      cca2outAuto[, nv] <- vec2
    }
  }
  for (i in 1:ncol(cca1outAuto)) {
    mynorm <- sqrt(sum(cca1outAuto[, i] * cca1outAuto[, i]))
    if (mynorm > 0)
      {
      cca1outAuto[, i] <- cca1outAuto[, i]/mynorm
        .eanatsparsify( cca1outAuto[, i]/mynorm, abs(sparseness[1]) )
      print(sum())
      }
    mynorm <- sqrt(sum(cca2outAuto[, i] * cca2outAuto[, i]))
    if (mynorm > 0)
      {
      cca2outAuto[, i] <-
        .eanatsparsify( cca2outAuto[, i]/mynorm, abs(sparseness[2]))
      }
  }
  fakemask1 <- makeImage(c(1, 1, ncol(mat1)), 1)
  fakemask2 <- makeImage(c(1, 1, ncol(mat2)), 1)
  usefakemask <- c((length(dim(myres$eig1)) == 2), (length(dim(myres$eig2)) ==
    2))
  locmask <- inmask
  if (doseg)
    cca1outAuto <- .matrixSeg(t(cca1outAuto))
  if (dim(cca1outAuto)[2] != nvecs)
    cca1outAuto <- t(cca1outAuto)
  cca1out <- (cca1outAuto)
  if (doseg)
    cca2outAuto <- .matrixSeg(t(cca2outAuto))
  if (dim(cca2outAuto)[2] != nvecs)
    cca2outAuto <- t(cca2outAuto)
  cca2out <- (cca2outAuto)
  ####################################################################################
  if (usefakemask[1])
    init1 <- matrixToImages(t(cca1out), fakemask1) else init1 <- matrixToImages(t(cca1out), locmask[[1]])
  if (usefakemask[2])
    init2 <- matrixToImages(t(cca2out), fakemask2) else init2 <- matrixToImages(t(cca2out), locmask[[2]])
  print("Get Final Results")
  if (!usefakemask[1] & !usefakemask[2])
    maskinit <- locmask
  if (usefakemask[1] & usefakemask[2])
    maskinit <- c(fakemask1, fakemask2)
  if (usefakemask[1] & !usefakemask[2])
    maskinit <- c(fakemask1, locmask[[2]])
  if (!usefakemask[1] & usefakemask[2])
    maskinit <- c(locmask[[1]], fakemask2)
  myres <- sparseDecom2(
    inmatrix = inmatrix,
    inmask = maskinit,
    sparseness = sparseness,
    nvecs = nvecs,
    its = its,
    cthresh = cthresh,
    perms = perms,
    uselong = uselong,
    z = z,
    smooth = smooth,
    robust = robust,
    mycoption = mycoption,
    initializationList = init1,
    initializationList2 = init2,
    ell1 = ell1 )
  ########################################################################
  return(
    list(
      projections = myres$projections,
      projections2 = myres$projections2,
      eig1 = myres$eig1,
      eig2 = myres$eig2,
      ccasummary = myres$ccasummary,
      bootccalist1 = bootccalist1,
      bootccalist2 = bootccalist2,
      cca1outAuto = (cca1outAuto),
      cca2outAuto = (cca2outAuto)
      )
    )
}

.cosineDist <- function(xin, yin) {
  x <- t(as.matrix(xin))
  y <- t(as.matrix(yin))
  return(as.numeric(1 - x %*% t(y)/(sqrt(rowSums(x^2) %*% t(rowSums(y^2))))))
}
