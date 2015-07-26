#' Convenience wrapper for 2-view eigenanatomy decomposition.
#'
#' Decomposes two matrices into paired sparse eigenevectors to maximize
#' canonical correlation. Note: we do not scale the matrices internally.
#' We leave scaling choices to the user.
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
#' @param priorWeight Scalar value weight on prior between 0 (prior is weak)
#' and 1 (prior is strong).  Only engaged if initialization is used
#' @param verbose activates verbose output to screen
#' @return outputs a decomposition of a pair of matrices
#' @author Avants BB
#' @examples
#'
#' mat<-replicate(100, rnorm(20))
#' mat2<-replicate(100, rnorm(20))
#' mat<-scale(mat)
#' mat<-mat-min(mat)
#' mat2<-scale(mat2)
#' mat2<-mat2-min(mat2)
#' mydecom<-sparseDecom2( inmatrix=list(mat,mat2),
#'   sparseness=c(0.1,0.3) , nvecs=3, its=3, perms=0)
#' wt<-0.666
#' mat3<-mat*wt+mat2*(1-wt)
#' mat3<-mat3-min(mat3)
#' mydecom<-sparseDecom2( inmatrix=list(mat,mat3),
#'   sparseness=c(0.2,0.2), nvecs=5, its=10, perms=5 )
#'
#' \dontrun{
#' # a masked example
#' im<-antsImageRead( getANTsRData("r64"))
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
#'   sparseness=c(0.1,0.1) ,nvecs=length(initlist) , smooth=1,
#'   cthresh=c(0,0), initializationList = initlist ,ell1 = 11 )
#' ### now SNPs ###
#' rf<-usePkg('randomForest')
#' bg<-usePkg('BGLR')
#' if ( bg & rf ) {
#' data(mice)
#' snps<-mice.X
#' numericalpheno<-as.matrix( mice.pheno[,c(4,5,13,15) ] )
#' numericalpheno<-residuals( lm( numericalpheno ~
#'   as.factor(mice.pheno$Litter) ) )
#' nfolds<-6
#' train<-sample( rep( c(1:nfolds), 1800/nfolds ) )
#' train<-( train < 4 )
#' snpd<-sparseDecom2( inmatrix=list( ( as.matrix(snps[train,]) ),
#'   numericalpheno[train,] ), nvecs=20, sparseness=c( 0.001, -0.5 ),
#'   its=3, ell1=0.1 , z=-1 )
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
#' } # check bg and rf
#' }
#'
#' @export sparseDecom2
sparseDecom2 <- function(
  inmatrix,
  inmask = c(NA, NA),
  sparseness = c(0.01, 0.01),
  nvecs = 3,
  its = 2,
  cthresh = c(0, 0),
  statdir = NA,
  perms = 0,
  uselong = 0,
  z = 0,
  smooth = 0,
  robust = 0,
  mycoption = 1,
  initializationList = list(),
  initializationList2 = list(),
  ell1 = 0.05,
  priorWeight = 0,
  verbose = FALSE  ) {
  if (class(inmask[[1]])[[1]] != "antsImage")
     maskx = new("antsImage", "float", 3) else maskx = antsImageClone( inmask[[1]] )
  if (class(inmask[[2]])[[1]] != "antsImage")
     masky = new("antsImage", "float", 3) else masky = antsImageClone( inmask[[2]] )
  inmask = c( maskx, masky )
  verbose = as.numeric( verbose )
  if ( robust > 0 )
    {
    inputMatrices = list(
      robustMatrixTransform( inmatrix[[1]] ),
      robustMatrixTransform( inmatrix[[2]] )
    )
    } else inputMatrices = inmatrix
  # helper function allows easier R-based permutation
  sccaner=.sparseDecom2helper2(
    inputMatrices,
    inmask,
    sparseness,
    nvecs,
    its,
    cthresh,
    statdir,
    uselong,
    z,
    smooth,
    robust,
    mycoption,
    initializationList,
    initializationList2,
    ell1,
    priorWeight,
    verbose
    )
  ccasummary = data.frame(
    corrs = sccaner$corrs,
    pvalues = rep(NA,nvecs)
    )
  if ( perms >  0 )
  {
  ccasummary$pvalues = rep(0 , nvecs )
  for ( permer in 1:perms )
  {
  permmatrix = list(
    inputMatrices[[1]][ sample( 1:nrow(inputMatrices[[1]]) ) ,  ],
    inputMatrices[[2]][ sample( 1:nrow(inputMatrices[[2]]) ) ,  ]
    )
  sccanerp=.sparseDecom2helper2(
    permmatrix,
    inmask,
    sparseness,
    nvecs,
    its,
    cthresh,
    statdir,
    uselong,
    z,
    smooth,
    robust,
    mycoption,
    initializationList,
    initializationList2,
    ell1,
    priorWeight,
    verbose
    )
    counter = as.numeric( abs(ccasummary$corrs) < abs(sccanerp$corrs)   )
    ccasummary$pvalues = ccasummary$pvalues + counter
    }
    ccasummary$pvalues = ccasummary$pvalues / perms
  }
  return(
    list(
      projections = sccaner$projections,
      projections2 = sccaner$projections2,
      eig1 = sccaner$eig1,
      eig2 = sccaner$eig2,
      ccasummary = ccasummary
      )
    )
}


.sparseDecom2helper2 <- function(
  inputMatrices,
  inmask,
  sparseness,
  nvecs,
  its,
  cthresh,
  statdir,
  uselong,
  z,
  smooth,
  robust,
  mycoption,
  initializationList,
  initializationList2,
  ell1,
  priorWeight,
  verbose) {
  outval = .Call( "sccanCpp",
    inputMatrices[[1]],
    inputMatrices[[2]],
    inmask[[1]],
    inmask[[2]],
    sparseness[1],
    sparseness[2],
    nvecs,
    its,
    cthresh[1],
    cthresh[2],
    z,
    smooth,
    initializationList,
    initializationList2,
    mycoption,
    ell1,
    verbose,
    priorWeight,
    PACKAGE="ANTsR" )
  p1 = inputMatrices[[1]] %*% t(outval$eig1)
  p2 = inputMatrices[[2]] %*% t(outval$eig2)
  return(
      list(
        projections = p1,
        projections2 = p2,
        eig1 = t(outval$eig1),
        eig2 = t(outval$eig2),
        corrs = diag( cor( p1 , p2  ) )
        )
      )
}

.sparseDecom2helper <- function(
  inputMatrices,
  inmask,
  sparseness,
  nvecs,
  its,
  cthresh,
  statdir,
  uselong,
  z,
  smooth,
  robust,
  mycoption,
  initializationList,
  initializationList2,
  ell1,
  priorWeight,
  verbose) {
  numargs <- nargs()
  if (numargs < 1 | missing(inputMatrices)) {
    cat(" sparseDecom( inputMatrices=NA,  inmask=NA , sparseness=c(0.01,0.01) , nvecs=50 , cthresh=c(250,250),  its=5  ) \n")
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
  if (class(inmask[[1]])[[1]] == "antsImage") {
    if ( sum( inmask[[1]] > 0.5 ) != ncol(inputMatrices[[1]]) )
      stop("dimensions of view 1 mask and view 1 matrix do not match")
  }
  if (class(inmask[[2]])[[1]] == "antsImage") {
    if ( sum( inmask[[2]] > 0.5 ) != ncol(inputMatrices[[2]]) )
      stop("dimensions of view 2 mask and view 2 matrix do not match")
  }
  if (is.na(statdir))
    statdir <- paste(tempdir(), "/", sep = "")
  mfn <- paste(statdir, "sccamask", post, ".nii.gz", sep = "")
  outfn <- paste(statdir, "scca.nii.gz", sep = "")
  decomp <- paste(statdir, "sccaprojectionsView", post, "vec.csv", sep = "")
  matname <- paste(statdir, "sccamatrix", post, ".mha", sep = "")
  antsImageWrite(as.antsImage(inputMatrices[[1]]), matname[1])
  antsImageWrite(as.antsImage(inputMatrices[[2]]), matname[2])
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
    ell1, "-i", its, "--PClusterThresh", cthresh[1], "-p", 0, "--QClusterThresh",
    cthresh[2], "-n", nvecs, "-o", outfn, "-g", uselong, "-z", z, "-s", smooth,
    "-r", 0, "-c", mycoption, "--prior-weight", priorWeight,"-v", verbose)

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
      "--l1", ell1, "-i", its, "--PClusterThresh", cthresh[1], "-p", 0,
      "--QClusterThresh", cthresh[2], "-n", nvecs, "-o", outfn, "-g", uselong,
      "-z", z, "-s", smooth, "-r", 0, "-c", mycoption, "--mask", mfn[1],
      "--initialization", initlistfn, "--prior-weight", priorWeight,"-v", verbose)
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
        "-p", 0, "--QClusterThresh", cthresh[2], "-n", nvecs, "-o", outfn,
        "-g", uselong, "-z", z, "-s", smooth, "-r", 0, "-c", mycoption,
        "--mask", mfn[1], "--initialization", initlistfn, "--mask2", mfn[2],
        "--initialization2", initlistfn2, "--prior-weight", priorWeight,
        "-v", verbose)
    }
  }
  .Call("sccan", .int_antsProcessArguments(c(args)), PACKAGE = "ANTsR")
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
    projmat1=t( imageListToMatrix( fnl, inmask[[1]] ) )
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
    projmat2=t( imageListToMatrix( fnl2, inmask[[2]] ) )
  }
  if (is.na(inmask[[1]])) {
    glb <- paste("scca*_Variate_View1vec.csv", sep = "")
    fnl <- list.files(path = statdir, pattern = glob2rx(glb), full.names = T,
      recursive = T)
    fnl <- read.csv(fnl)
    projmat1 = data.matrix( fnl )
  }
  if (is.na(inmask[[2]])) {
    glb <- paste("scca*_Variate_View2vec.csv", sep = "")
    fnl2 <- list.files(path = statdir, pattern = glob2rx(glb), full.names = T,
      recursive = T)
    fnl2 <- read.csv(fnl2)
    projmat2 = data.matrix( fnl2 )
  }
  return(
    list(
        projections = mydecomp,
        projections2 = mydecomp2,
        eig1 = fnl,
        eig2 = fnl2,
        corrs = diag(
          cor( inputMatrices[[1]] %*% projmat1 ,
               inputMatrices[[2]] %*% projmat2 ) )
        )
      )
}
