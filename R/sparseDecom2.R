#' Convenience wrapper for 2-view eigenanatomy decomposition.
#'
#' Decomposes two matrices into paired sparse eigenevectors to maximize
#' canonical correlation.
#'
#'
#' @param inmatrix input as inmatrix=list(mat1,mat2). n by p input matrix and n
#' by q input matrix , spatial variable lies along columns.
#' @param inmask optional pair of antsImage masks
#' @param priorWeight Scalar value weight on prior between 0 (prior is weak)
#' and 1 (prior is strong).  Only engaged if initialization is used
#' @param otherparams see sccan for other parameters
#' @return outputs a decomposition of a pair of matrices
#' @author Avants BB
#' @examples
#'
#' \dontrun{
#' mat<-replicate(100, rnorm(20))
#' mat2<-replicate(100, rnorm(20))
#' mydecom<-sparseDecom2( inmatrix=list(mat,mat2), sparseness=c(0.1,0.3) , nvecs=3, its=3, perms=0)
#' wt<-0.666
#' mat3<-mat*wt+mat2*(1-wt)
#' mydecom<-sparseDecom2( inmatrix=list(mat,mat3), sparseness=c(0.2,0.2), nvecs=5, its=10, perms=200 )
#'
#' # a masked example
#' im<-antsImageRead( getANTsRData('r64') ,2)
#' dd<- im > 250
#' mask<-antsImageClone( im )
#' mask[ !dd ]<-0
#' mask[ dd ]<-1
#' mat1<-matrix( rnorm(sum(dd)*10) , nrow=10 )
#' mat2<-matrix( rnorm(sum(dd)*10) , nrow=10 )
#' initlist<-list()
#' for ( nvecs in 1:2 ) {
#'   init1<-antsImageClone( mask )
#'   init1[dd]<-rnorm(sum(dd))
#'   initlist<-lappend( initlist, init1 )
#' }
#' ff<-sparseDecom2( inmatrix=list(mat1,mat2), inmask=list(mask,mask),
#'   sparseness=c(0.1,0.1) ,nvecs=length(initlist) , smooth=1, cthresh=c(0,0), initializationList = initlist ,ell1 = 11 )
#' ### now SNPs ###
#' usePkg('randomForest')
#' usePkg('BGLR')
#' data(mice)
#' snps<-mice.X
#' numericalpheno<-as.matrix( mice.pheno[,c(4,5,13,15) ] )
#' numericalpheno<-residuals( lm( numericalpheno ~ as.factor(mice.pheno$Litter) ) )
#' nfolds<-6
#' train<-sample( rep( c(1:nfolds), 1800/nfolds ) )
#' train<-( train < 4 )
#' snpd<-sparseDecom2( inmatrix=list( ( as.matrix(snps[train,]) ), numericalpheno[train,] ), nvecs=20, sparseness=c( 0.001, -0.5 ), its=3, ell1=0.1 , z=-1 )
#' for ( j in 3:3) {
#' traindf<-data.frame( bmi=numericalpheno[ train,j] ,
#'    snpse=as.matrix( snps[train, ] ) %*% as.matrix( snpd$eig1 ) )
#' testdf <-data.frame( bmi=numericalpheno[!train,j] ,
#'    snpse=as.matrix( snps[!train,] ) %*% as.matrix( snpd$eig1 ) )
#' myrf<-randomForest( bmi ~ . , data=traindf )
#' preddf<-predict(myrf, newdata=testdf )
#' print( cor.test(preddf, testdf$bmi ) )
#' plot( preddf, testdf$bmi )
#' }
#' }
#'
#' @export sparseDecom2
sparseDecom2 <- function(inmatrix, inmask = c(NA, NA), sparseness = c(0.01, 0.01), 
  nvecs = 3, its = 2, cthresh = c(0, 0), statdir = NA, perms = 0, uselong = 0, 
  z = 0, smooth = 0, robust = 0, mycoption = 0, initializationList = list(), initializationList2 = list(), 
  ell1 = 0.05, priorWeight = 0) {
  numargs <- nargs()
  if (numargs < 1 | missing(inmatrix)) {
    cat(" sparseDecom( inmatrix=NA,  inmask=NA , sparseness=c(0.01,0.01) , nvecs=50 , cthresh=c(250,250),  its=5  ) \n")
    cat(" each input should be a list with 2 entries e.g. sparseness=c(0.01,0.02) \n")
    return(0)
  }
  if (is.na(inmask[1]) & length(initializationList) > 0) {
    cat("You should set pass a mask in mask slot 1 if you are using initializationList\n")
    return(0)
  }
  if (is.na(inmask[2]) & length(initializationList2) > 0) {
    cat("You should set pass a mask in mask slot 2 if you are using initializationList2\n")
    return(0)
  }
  if (length(cthresh) < 2) {
    cat("You should set a length=2 cthresh value even if it is c(0,0)\n")
    return(0)
  }
  post <- c(1, 2)
  if (is.na(statdir)) 
    statdir <- paste(tempdir(), "/", sep = "")
  mfn <- paste(statdir, "sccamask", post, ".nii.gz", sep = "")
  outfn <- paste(statdir, "scca.nii.gz", sep = "")
  decomp <- paste(statdir, "sccaprojectionsView", post, "vec.csv", sep = "")
  matname <- paste(statdir, "sccamatrix", post, ".mha", sep = "")
  antsImageWrite(as.antsImage(inmatrix[[1]]), matname[1])
  antsImageWrite(as.antsImage(inmatrix[[2]]), matname[2])
  sccaname <- "two-view["
  if (class(inmask[[1]])[[1]] == "antsImage") {
    m1 <- inmask[[1]]
    dim <- as.numeric(m1@dimension)
    if (dim[1] == 4) 
      sccaname <- "dynsccan["
    antsImageWrite(inmask[[1]], mfn[1])
  } else mfn[1] <- NA
  if (class(inmask[[2]])[[1]] == "antsImage") {
    m2 <- inmask[[2]]
    dim2 <- as.numeric(m2@dimension)
    if (dim2[1] == 4) 
      sccaname <- "dynsccan["
    antsImageWrite(inmask[[2]], mfn[2])
  } else mfn[2] <- NA
  args <- list("--scca", paste(sccaname, matname[1], ",", matname[2], ",", mfn[1], 
    ",", mfn[2], ",", sparseness[1], ",", sparseness[2], "]", sep = ""), "--l1", 
    ell1, "-i", its, "--PClusterThresh", cthresh[1], "-p", perms, "--QClusterThresh", 
    cthresh[2], "-n", nvecs, "-o", outfn, "-g", uselong, "-z", z, "-s", smooth, 
    "-r", robust, "-c", mycoption, "--prior-weight", priorWeight)
  
  if (length(initializationList) > 0) {
    ct <- 1
    outfns <- c()
    for (img in initializationList) {
      tempfn <- paste(statdir, "init", ct, ".nii.gz", sep = "")
      outfns <- c(outfns, tempfn)
      antsImageWrite(img, tempfn)
      ct <- ct + 1
    }
    initlistfn <- paste(statdir, "init.txt", sep = "")
    fileConn <- file(initlistfn)
    writeLines(outfns, fileConn)
    close(fileConn)
    args <- list("--scca", paste(sccaname, matname[1], ",", matname[2], ",", 
      mfn[1], ",", mfn[2], ",", sparseness[1], ",", sparseness[2], "]", sep = ""), 
      "--l1", ell1, "-i", its, "--PClusterThresh", cthresh[1], "-p", perms, 
      "--QClusterThresh", cthresh[2], "-n", nvecs, "-o", outfn, "-g", uselong, 
      "-z", z, "-s", smooth, "-r", robust, "-c", mycoption, "--mask", mfn[1], 
      "--initialization", initlistfn, "--prior-weight", priorWeight)
    if (length(initializationList2) > 0) {
      ct <- 1
      outfns <- c()
      for (img in initializationList2) {
        tempfn <- paste(statdir, "init2_", ct, ".nii.gz", sep = "")
        outfns <- c(outfns, tempfn)
        antsImageWrite(img, tempfn)
        ct <- ct + 1
      }
      initlistfn2 <- paste(statdir, "init2.txt", sep = "")
      fileConn <- file(initlistfn2)
      writeLines(outfns, fileConn)
      close(fileConn)
      args <- list("--scca", paste(sccaname, matname[1], ",", matname[2], ",", 
        mfn[1], ",", mfn[2], ",", sparseness[1], ",", sparseness[2], "]", 
        sep = ""), "--l1", ell1, "-i", its, "--PClusterThresh", cthresh[1], 
        "-p", perms, "--QClusterThresh", cthresh[2], "-n", nvecs, "-o", outfn, 
        "-g", uselong, "-z", z, "-s", smooth, "-r", robust, "-c", mycoption, 
        "--mask", mfn[1], "--initialization", initlistfn, "--mask2", mfn[2], 
        "--initialization2", initlistfn2, "--prior-weight", priorWeight)
    }
  }
  .Call("sccan", int_antsProcessArguments(c(args)), PACKAGE = "ANTsR")
  mydecomp <- read.csv(decomp[1])
  if (!is.na(inmask[[1]])) {
    glb <- paste("scca*View1vec*.nii.gz", sep = "")
    fnl <- list.files(path = statdir, pattern = glob2rx(glb), full.names = T, 
      recursive = T)[1:nvecs]
    fnll <- list()
    for (i in 1:length(fnl)) {
      img <- antsImageRead(fnl[i], dim[1])
      fnll <- lappend(fnll, img)
    }
    fnl <- fnll
  }
  mydecomp2 <- read.csv(decomp[2])
  if (!is.na(inmask[[2]])) {
    glb <- paste("scca*View2vec*.nii.gz", sep = "")
    fnl2 <- list.files(path = statdir, pattern = glob2rx(glb), full.names = T, 
      recursive = T)[1:nvecs]
    fnll2 <- list()
    for (i in 1:length(fnl2)) {
      img <- antsImageRead(fnl2[i], dim2[1])
      fnll2 <- lappend(fnll2, img)
    }
    fnl2 <- fnll2
  }
  if (is.na(inmask[[1]])) {
    glb <- paste("scca*_Variate_View1vec.csv", sep = "")
    fnl <- list.files(path = statdir, pattern = glob2rx(glb), full.names = T, 
      recursive = T)
    fnl <- read.csv(fnl)
  }
  if (is.na(inmask[[2]])) {
    glb <- paste("scca*_Variate_View2vec.csv", sep = "")
    fnl2 <- list.files(path = statdir, pattern = glob2rx(glb), full.names = T, 
      recursive = T)
    fnl2 <- read.csv(fnl2)
  }
  pvfn <- paste(statdir, "scca_summary.csv", sep = "")
  ccasummary <- NA
  if (file.exists(pvfn)) {
    ccasummary <- read.csv(pvfn)
  }
  return(list(projections = mydecomp, projections2 = mydecomp2, eig1 = fnl, eig2 = fnl2, 
    ccasummary = ccasummary))
} 
