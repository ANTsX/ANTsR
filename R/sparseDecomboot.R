#' Convenience wrapper for eigenanatomy decomposition.
#'
#' Decomposes a matrix into sparse eigenevectors to maximize explained
#' variance.
#'
#' @param inmatrix n by p input images , subjects or time points by row ,
#' spatial variable lies along columns
#' @param inmask optional antsImage mask
#' @param sparseness lower values equal more sparse
#' @param nvecs number of vectors
#' @param its number of iterations
#' @param cthresh cluster threshold
#' @param z u penalty, experimental
#' @param smooth smoothness eg 0.5
#' @param initializationList see initializeEigenanatomy
#' @param mycoption 0, 1 or 2 all produce different output 0 is combination
#'   of 1 (spatial orthogonality) and 2 (subject space orthogonality)
#' @param nboot boostrap integer e.g. 10 equals 10 boostraps
#' @param nsamp value less than or equal to 1, e.g. 0.9 means 90 percent of data
#' will be used in each boostrap resampling
#' @param robust boolean
#' @param doseg orthogonalize bootstrap results
#' @author Avants BB
#' @examples
#'
#' mat<-replicate(100, rnorm(20))
#' mydecom<-sparseDecomboot( mat, nboot=5, nsamp=0.9, nvecs=2 )
#'
#' \dontrun{
#' # for prediction
#' if ( usePkg("randomForest") & usePkg("spls") ) {
#' data(lymphoma)
#' training<-sample( rep(c(TRUE,FALSE),31)  )
#' sp<-0.001 ; myz<-0 ; nv<-5
#' ldd<-sparseDecomboot( lymphoma$x[training,], nvecs=nv ,
#'   sparseness=( sp ), mycoption=1, z=myz , nsamp=0.9, nboot=50 ) # NMF style
#' outmat<-as.matrix(ldd$eigenanatomyimages )
#' # outmat<-t(ldd$cca1outAuto)
#' traindf<-data.frame( lclass=as.factor(lymphoma$y[ training  ]),
#'   eig = lymphoma$x[training,]  %*% t(outmat) )
#' testdf<-data.frame(  lclass=as.factor(lymphoma$y[ !training ]),
#'   eig = lymphoma$x[!training,] %*% t(outmat) )
#' myrf<-randomForest( lclass ~ . ,   data=traindf )
#' predlymp<-predict(myrf, newdata=testdf)
#' print(paste('N-errors:',sum(abs( testdf$lclass != predlymp ) ),
#'   'non-zero ',sum(abs( outmat ) > 0 ) ) )
#' for ( i in 1:nv )
#'   print(paste(' non-zero ',i,' is: ',sum(abs( outmat[i,] ) > 0 ) ) )
#' }
#' } # end dontrun
#'
#' @export sparseDecomboot
sparseDecomboot <- function(inmatrix = NA, inmask = NA, sparseness = 0.01,
  nvecs = 50,
  its = 5, cthresh = 250, z = 0, smooth = 0,
  initializationList = list(),
  mycoption = 0, nboot = 10, nsamp = 0.9, robust = 0, doseg = TRUE) {
  numargs <- nargs()
  nsubj <- nrow(inmatrix)
  mysize <- round(nsamp * nsubj)
  mat1 <- inmatrix
  mymask <- inmask
  cca1out <- 0
  cca1outAuto <- 0
  bootccalist1 <- list()
  for (i in 1:nvecs) {
    makemat <- matrix(rep(0, nboot * ncol(mat1)), ncol = ncol(mat1))
    bootccalist1 <- lappend(bootccalist1, makemat)
  }
  if (nsamp >= 0.999999999)
    doreplace <- TRUE else doreplace <- FALSE
  for (boots in 1:nboot) {
    mysample <- sample(1:nsubj, size = mysize, replace = doreplace)
    submat1 <- mat1[mysample, ]
    print(paste("boot", boots, "sample", mysize))
    myres <- sparseDecom(inmatrix = submat1, inmask = mymask, sparseness = sparseness,
      nvecs = nvecs, its = its, cthresh = cthresh, z = z,
      smooth = smooth, initializationList = initializationList, mycoption = mycoption,
      robust = robust)
    cca1 <- t(myres$eigenanatomyimages)
    if (boots > 1 & TRUE) {
      cca1copy <- cca1
      # compute the 'closest' eigenvector and store the difference in a difference
      # matrix called mymult
      mymult <- matrix(rep(0, ncol(cca1) * ncol(cca1)), ncol = ncol(cca1))
      for (j in 1:ncol(cca1out)) {
        for (k in 1:ncol(cca1)) {
          temp1 <- abs(cca1out[, j])
          temp2 <- abs(cca1[, k])
          mymult[j, k] <- .cosineDist(temp1, temp2)
          # sum( abs( temp1/sum(temp1) - temp2/sum(temp2) ) )
        }
      }
      # find the best match and reorder appropriately
      for (ct in 1:(ncol(cca1))) {
        arrind <- which(mymult == min(mymult), arr.ind = T)
        cca1copy[, arrind[1]] <- cca1[, arrind[2]]
        mymult[arrind[1], ] <- 0
        mymult[, arrind[2]] <- 0
      }
      cca1 <- cca1copy
    }
    cca1out <- cca1out + (cca1)
    for (nv in 1:nvecs) {
      bootccalist1[[nv]][boots, ] <- abs(cca1[, nv])
    }
  }
  cca1outAuto <- cca1out
  for (nv in 1:nvecs) {
    bootmat <- bootccalist1[[nv]]
    vec1 <- apply(bootmat, FUN = mean, MARGIN = 2)
    # mysd1 <- apply(bootmat,FUN=sd,MARGIN=2) vec1[ mysd1 > 0 ] <- vec1[ mysd1 > 0 ]
    # / mysd1[ mysd1 > 0 ] vec1 <- apply(bootmat,FUN=t.test,MARGIN=2) vec1 <-
    # as.numeric( do.call(rbind, vec1)[,1] )
    vec1[is.na(vec1)] <- 0
    if ((nv > 1) & (abs(sparseness * nvecs) < 1) & TRUE) {
      # for ( j in 1:(nv-1) ) { prevec <- cca1out[ , j ] vec1[ prevec > 0 ] <- 0 }
      cca1out[, nv] <-.eanatsparsify(vec1, abs(sparseness))
      cca1outAuto[, nv] <- vec1
    } else {
      cca1out[, nv] <-.eanatsparsify(vec1, abs(sparseness))
      cca1outAuto[, nv] <- vec1
    }
  }
  for (i in 1:ncol(cca1outAuto)) {
    mynorm <- sqrt(sum(cca1outAuto[, i] * cca1outAuto[, i]))
    if (mynorm > 0)
      cca1outAuto[, i] <- cca1outAuto[, i]/mynorm
  }
  if ( is.na( inmask ) ) usefakemask <- TRUE
  fakemask1 <- makeImage(c(1, 1, ncol(mat1)), 1)
  locmask <- inmask
  if (usefakemask) {
    locmask <- fakemask1
    if (doseg)
      cca1outAuto <- .matrixSeg(t(cca1outAuto)) else cca1outAuto <- t(cca1outAuto)
    cca1out <- cca1outAuto
  } else {
    cca1outAuto <- matrixToImages(t(cca1outAuto), locmask)
    autoseg1 <- eigSeg(locmask, cca1outAuto, doseg)
    cca1outAuto <- t(imageListToMatrix(cca1outAuto, locmask))
    cca1out <- t(cca1outAuto)
  }
  ##############################################################################
  finalinit <- matrixToImages(cca1out, locmask)
  myres <- sparseDecom(inmatrix = inmatrix, inmask = locmask,
    sparseness = sparseness,
    nvecs = nvecs, its = its, cthresh = cthresh, z = z, smooth = smooth,
    initializationList = finalinit, mycoption = mycoption, robust = robust)

  return(
      list(
        projections = myres$projections,
        eigenanatomyimages = myres$eigenanatomyimages,
        bootccalist1 = bootccalist1,
        cca1outAuto = cca1outAuto )
      )
}

.cosineDist <- function(xin, yin) {
  x <- t(as.matrix(xin))
  y <- t(as.matrix(yin))
  return(as.numeric(1 - x %*% t(y)/(sqrt(rowSums(x^2) %*% t(rowSums(y^2))))))
}
