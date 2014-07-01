sparseDecom <- function(inmatrix = NA, inmask = 0, sparseness = 0.01, nvecs = 50, its = 5, cthresh = 250, statdir = NA, 
  z = 0, smooth = 0, initializationList = list(), mycoption = 0, robust = 0, ell1=1 ) {
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
  antsImageWrite(as.antsImage(inmatrix), matname)
  mfn <- NA
  maskdim<-0
  if ( class(inmask)[[1]][1] == "antsImage" ) {
    maskdim<-inmask@dimension
    mfn <- paste(statdir, "spcamask.nii.gz", sep = "")
    antsImageWrite(inmask, mfn)
  }
  sccaname<-"recon[" 
  if ( maskdim == 4 ) sccaname<-"recon4d[" 
  args <- list("--svd", paste(sccaname, matname, ",", mfn, ",", sparseness, "]", sep = ""), "--l1", ell1, "-i", 
    its, "--PClusterThresh", cthresh, "-n", nvecs, "-o", outfn, "-z", z, "-s", smooth, "-c", mycoption, "--mask", 
    inmask,"-r",robust)
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
    args <- list("--svd", paste(sccaname, matname, ",", mfn, ",", sparseness, "]", sep = ""), "--l1", 1, "-i", 
      its, "--PClusterThresh", cthresh, "-n", nvecs, "-o", outfn, "-z", z, "-s", smooth, "-c", mycoption, "-r",robust, "--mask", mfn, "--initialization", initlistfn)
    print(initlistfn)
  }
  time1<-( Sys.time() )
  .Call("sccan", int_antsProcessArguments(c(args)), PACKAGE = "ANTsR")
  time2<-( Sys.time() )
  mydecomp <- read.csv(decomp)
  glb <- paste("spca_Umatrix_View1vec.csv", sep = "")
  fnu <- list.files(path = statdir, pattern = glob2rx(glb), full.names = T, recursive = T)
  fnu <- read.csv(fnu)
  if ( class(inmask)[[1]][1] == "antsImage" ) {
    glb <- paste("spca*View1vec*.nii.gz", sep = "")
    fnl <- list.files(path = statdir, pattern = glob2rx(glb), full.names = T, recursive = T)[1:nvecs]
    fnll <- list()
    for (i in 1:length(fnl)) {
      img <- antsImageRead(fnl[i], length(dim(inmask)))
      fnll <- lappend(fnll, img)
    }
    fnl <- fnll
  }
  if ( class(inmask)[[1]][1] != "antsImage" ) {
    glb <- paste("spca*_Variate_View1vec.csv", sep = "")
    fnl <- list.files(path = statdir, pattern = glob2rx(glb), full.names = T, recursive = T)
    fnl <- read.csv(fnl)
  }
  return(list(projections = mydecomp, eigenanatomyimages = fnl, umatrix = fnu, computationtime=(time2-time1) ))
  
} 
