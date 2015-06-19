#' Convenience wrapper for eigenanatomy decomposition.
#'
#' Decomposes a matrix into sparse eigenevectors to maximize explained
#' variance. Note: we do not scale the matrices internally.
#' We leave scaling choices to the user.
#'
#' @param inmatrix n by p input images , subjects or time points by row ,
#' spatial variable lies along columns
#' @param inmask optional antsImage mask
#' @param sparseness lower values equal more sparse
#' @param nvecs number of vectors
#' @param its number of iterations
#' @param cthresh cluster threshold
#' @param statdir place on disk to save results
#' @param z u penalty, experimental
#' @param smooth smoothness eg 0.5
#' @param initializationList see initializeEigenanatomy
#' @param mycoption 0, 1 or 2 all produce different output 0 is combination
#'   of 1 (spatial orthogonality) and 2 (subject space orthogonality)
#' @param robust rank transform input data - good for data checking
#' @param ell1 the ell1 grad descent param
#' @param getSmall try to get smallest evecs (bool)
#' @param verbose activates verbose output
#' @param powerit alternative implementation
#' @return outputs a decomposition of a population or time series matrix
#' @author Avants BB
#' @examples
#'
#' mat<-replicate(100, rnorm(20))
#' mydecom<-sparseDecom( mat )
#' \dontrun{
#' # for prediction
#' if ( usePkg("randomForest") & usePkg("spls")  & usePkg('BGLR') ) {
#' data(lymphoma) # from spls
#' training<-sample( rep(c(TRUE,FALSE),31)  )
#' sp<-0.02 ; myz<-0
#' ldd<-sparseDecom( lymphoma$x[training,], nvecs=5 , sparseness=( sp ),
#'   mycoption=1, z=myz ) # NMF style
#' traindf<-data.frame( lclass=as.factor(lymphoma$y[ training  ]),
#'   eig = lymphoma$x[training,]  %*% as.matrix(ldd$eigenanatomyimages ))
#' testdf<-data.frame(  lclass=as.factor(lymphoma$y[ !training ]),
#'  eig = lymphoma$x[!training,] %*% as.matrix(ldd$eigenanatomyimages ))
#' myrf<-randomForest( lclass ~ . ,   data=traindf )
#' predlymp<-predict(myrf, newdata=testdf)
#' print(paste('N-errors:',sum(abs( testdf$lclass != predlymp ) ),
#'   ' non-zero ',sum(abs( ldd$eigenanatomyimages ) > 0 ) ) )
#' # compare to http://arxiv.org/pdf/0707.0701v2.pdf
#' # now SNPs
#' data(mice)
#' snps<-quantifySNPs( mice.X, shiftit = TRUE )
#' numericalpheno<-as.matrix( mice.pheno[,c(4,5,13,15) ] )
#' nfolds<-6
#' train<-sample( rep( c(1:nfolds), 1800/nfolds ) )
#' train<-( train < 4 )
#' lrmat<-lowrankRowMatrix( as.matrix( snps[train,] ) ,  50 )
#' snpd<-sparseDecom( lrmat, nvecs=20 , sparseness=( 0.001), z=-1 )
#' projmat<-as.matrix( snpd$eig )
#' snpse<-as.matrix( snps[train, ]  ) %*% projmat
#' traindf<-data.frame( bmi=numericalpheno[train,3] , snpse=snpse)
#' snpse<-as.matrix( snps[!train, ]  ) %*% projmat
#' testdf <-data.frame( bmi=numericalpheno[!train,3] , snpse=snpse )
#' myrf<-randomForest( bmi ~ . , data=traindf )
#' preddf<-predict(myrf, newdata=testdf )
#' cor.test(preddf, testdf$bmi )
#' plot(preddf, testdf$bmi )
#' }
#' }
#' @export sparseDecom
sparseDecom <- function(inmatrix = NA, inmask = 0,
  sparseness = 0.01,
  nvecs = 50,
  its = 5, cthresh = 250,
  statdir = NA, z = 0, smooth = 0, initializationList = list(),
  mycoption = 0, robust = 0, ell1 = 1, getSmall = 0, verbose=0,
  powerit=0 ) {
  numargs <- nargs()
  if (numargs < 1 | missing(inmatrix)) {
    cat(" sparseDecom( inmatrix=NA,  inmask=NA , sparseness=0.01 , nvecs=50 , its=5 , cthresh=250 ) \n")
    return(0)
  }
  if (is.na(statdir))
    statdir <- paste(tempdir(), "/", sep = "")
  outfn <- paste(statdir, "spca.nii.gz", sep = "")
  decomp <- paste(statdir, "spcaprojectionsView1vec.csv", sep = "")
  matname <- paste(statdir, "spcamatrix.mha", sep = "")
  antsImageWrite( as.antsImage(inmatrix), matname )
  if ( ! file.exists( matname) ) stop("sparseDecom cannot write image")
  mfn <- NA
  maskdim <- 0
  if (class(inmask)[[1]] == "antsImage") {
    if ( sum( inmask > 0.5 ) != ncol(inmatrix) )
      stop("dimensions of mask and matrix do not match")
  }
  if (class(inmask)[[1]][1] == "antsImage") {
    maskdim <- inmask@dimension
    mfn <- paste(statdir, "spcamask.nii.gz", sep = "")
    antsImageWrite(inmask, mfn)
    if ( ! file.exists( mfn ) ) stop("sparseDecom cannot write mask")
  }
  sccaname <- "recon["
  if ( powerit ) sccaname <- "derka["
  if (maskdim == 4)
    sccaname <- "recon4d["
  args <- list("--svd",
    paste(sccaname, matname, ",", mfn, ",", sparseness, "]",sep = ""),
    "--l1", ell1,
    "-i", its,
    "--PClusterThresh", cthresh,
    "-n", nvecs,
    "-o", outfn,
    "-z", z,
    "-s", smooth,
    "-c", mycoption,
    "--mask", inmask,
    "-r", robust, "--get-small", getSmall,"-v",1 )
  if (length(initializationList) > 0) {
    ct <- 1
    initfns <- c()
    for (img in initializationList) {
      initfn <- paste(statdir, "init", ct, ".nii.gz", sep = "")
      initfns <- c(initfns, initfn)
      antsImageWrite(img, initfn)
      ct <- ct + 1
    }
    initlistfn <- paste(statdir, "init.txt", sep = "")
    fileConn <- file(initlistfn)
    writeLines(initfns, fileConn)
    close(fileConn)
    args <- list("--svd",
    paste(sccaname, matname, ",", mfn, ",", sparseness,"]", sep = ""),
    "--l1", 1,
    "-i", its,
    "--PClusterThresh", cthresh,
    "-n",  nvecs,
    "-o", outfn,
    "-z", z,
    "-s", smooth,
    "-c", mycoption,
    "-r", robust,
    "--mask", mfn,
    "--initialization", initlistfn,
    "--get-small", getSmall,"-v",1)
    print(initlistfn)
  }
  time1 <- (Sys.time())
  .Call("sccan", .int_antsProcessArguments(c(args)), PACKAGE = "ANTsR")
  time2 <- (Sys.time())
  if ( ! file.exists(decomp) ) stop("failure - output not written")
  mydecomp <- read.csv(decomp)
  glb <- paste("spca_Umatrix_View1vec.csv", sep = "")
  fnu <- list.files(path = statdir, pattern = glob2rx(glb), full.names = T, recursive = T)
  if ( powerit == 0 ) fnu <- read.csv(fnu) else fnu=NA
  if (class(inmask)[[1]][1] == "antsImage") {
    glb <- paste("spca*View1vec*.nii.gz", sep = "")
    fnl <- list.files(path = statdir, pattern = glob2rx(glb), full.names = T,
      recursive = T)[1:nvecs]
    fnll <- list()
    for (i in 1:length(fnl)) {
      img <- antsImageRead(fnl[i], length(dim(inmask)))
      fnll <- lappend(fnll, img)
    }
    fnl <- fnll
  }
  if (class(inmask)[[1]][1] != "antsImage") {
    glb <- paste("spca*_Variate_View1vec.csv", sep = "")
    fnl <- list.files(path = statdir, pattern = glob2rx(glb), full.names = T,
      recursive = T)
    fnl <- read.csv(fnl)
  }
  return(list(projections = mydecomp, eigenanatomyimages = fnl, umatrix = fnu,
    computationtime = (time2 - time1)))
}
