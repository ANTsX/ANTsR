antsRegistration <- function(fixed = NA, moving = NA, typeofTransform = "", outprefix = "",
  ...) {
  numargs <- nargs()
  if (numargs == 1 & typeof(fixed) == "list") {
    .Call("antsRegistration", int_antsProcessArguments(c(fixed)), PACKAGE = "ANTsR")
    return(0)
  }
  if (numargs < 1 | missing(fixed) | missing(moving) | missing(typeofTransform) |
    missing(outprefix)) {
    cat("for simplified mode: \n")
    cat(" antsRegistration( fixed , moving , typeofTransform = c(\"Rigid\",\"Affine\",\"AffineFast\",\"SyN\"),  outputPrefix=\"./antsRegOut\" \n")
    cat("")
    cat("For full mode: use standard ants call , e.g. : \n")
    cat(" ANTsR::antsRegistration( list( d=2,m=\"mi[r16slice.nii.gz,r64slice.nii.gz,1,20,Regular,0.05]\", t=\"affine[1.0]\", c=\"2100x1200x1200x0\",  s=\"3x2x1x0\", f=\"4x3x2x1\", u=\"1\", o=\"[xtest,xtest.nii.gz,xtest_inv.nii.gz]\" ) )\n")
    cat("full help: \n")
    .Call("antsRegistration", int_antsProcessArguments(c(list("--help"))), PACKAGE = "ANTsR")
    return(0)
  }
  args <- list(fixed, moving, typeofTransform, outprefix, ...)
  myiterations <- "2100x1200x1200x10"
  if (typeofTransform == "AffineFast") {
    typeofTransform <- "Affine"
    myiterations <- "2100x1200x0x0"
  }
  if (!is.character(fixed)) {
    if (fixed@class[[1]] == "antsImage" & moving@class[[1]] == "antsImage") {
      inpixeltype <- fixed@pixeltype
      ttexists <- FALSE
      if (typeofTransform == "Rigid"  |
          typeofTransform == "Affine" |
          typeofTransform == "SyN"    |
          typeofTransform == "SyNBold" ) {
        ttexists <- TRUE
      }
      if (ttexists) {
        cat("use simple parameterization \n")
        moving <- antsImageClone(moving, "double")
        fixed <- antsImageClone(fixed, "double")
        warpedfixout <- antsImageClone(moving)
        warpedmovout <- antsImageClone(fixed)
        f <- antsrGetPointerName(fixed)
        m <- antsrGetPointerName(moving)
        wfo <- antsrGetPointerName(warpedfixout)
        wmo <- antsrGetPointerName(warpedmovout)
        if (typeofTransform == "SyNBold") {
          args <- list("-d", as.character(fixed@dimension),
          "-r", paste("[",f, ",", m, ",1]", sep = ""),
          "-m", paste("mattes[", f, ",", m,
          ",1,32,regular,0.2]", sep = ""), "-t", "Rigid[0.25]",
          "-c","[1200x1200x100,1e-6,5]", "-s", "2x1x0",
          "-f", "2x1","-m", paste("cc[",
            f, ",", m, ",1,2]", sep = ""), "-t",
            paste("SyN[0.1,3,0]", sep = ""),
            "-c", "[200x10,1e-6,5]", "-s", "1x0",
          "-f", "2x1", "-u", "1", "-z", "1", "--float", "0", "-o",
          paste("[", outprefix, ",", wmo, ",", wfo, "]", sep = ""))
          fwdtransforms <- c(paste(outprefix, "1Warp.nii.gz", sep = ""),
          paste(outprefix, "0GenericAffine.mat", sep = ""))
          invtransforms <- c(paste(outprefix, "0GenericAffine.mat", sep = ""),
          paste(outprefix, "1InverseWarp.nii.gz", sep = ""))
        }
        if (typeofTransform == "SyN") {
          args <- list("-d", as.character(fixed@dimension), "-r", paste("[",
          f, ",", m, ",1]", sep = ""), "-m", paste("mattes[", f, ",", m,
          ",1,32,regular,0.2]", sep = ""), "-t", "Affine[0.25]", "-c",
          "2100x1200x1200x0", "-s", "3x2x1x0", "-f", "4x3x2x1", "-m", paste("mattes[",
            f, ",", m, ",1,32]", sep = ""), "-t", paste(typeofTransform,
            "[0.25,3,0]", sep = ""), "-c", "2100x1200x1200x0", "-s", "3x2x1x0",
          "-f", "4x3x2x1", "-u", "1", "-z", "1", "--float", "0", "-o",
          paste("[", outprefix, ",", wmo, ",", wfo, "]", sep = ""))
          fwdtransforms <- c(paste(outprefix, "1Warp.nii.gz", sep = ""),
          paste(outprefix, "0GenericAffine.mat", sep = ""))
          invtransforms <- c(paste(outprefix, "0GenericAffine.mat", sep = ""),
          paste(outprefix, "1InverseWarp.nii.gz", sep = ""))
        }
        if (typeofTransform == "Rigid" | typeofTransform == "Affine") {
          args <- list("-d", as.character(fixed@dimension), "-r", paste("[",
          f, ",", m, ",1]", sep = ""), "-m", paste("mattes[", f, ",", m,
          ",1,32,regular,0.2]", sep = ""), "-t", paste(typeofTransform,
          "[0.25]", sep = ""), "-c", myiterations, "-s", "3x2x1x0", "-f",
          "6x4x2x1", "-u", "1", "-z", "1", "--float", "0", "-o", paste("[",
            outprefix, ",", wmo, ",", wfo, "]", sep = ""))
          fwdtransforms <- c(paste(outprefix, "0GenericAffine.mat", sep = ""))
          invtransforms <- c(paste(outprefix, "0GenericAffine.mat", sep = ""))
        }
        .Call("antsRegistration", int_antsProcessArguments(c(args)), PACKAGE = "ANTsR")
        # unlink(ffn) unlink(mfn) outvar<-basename(outprefix) outpath<-dirname(outprefix)
        # txlist<-list.files( path = outpath, pattern = glob2rx( paste(outvar,'*',sep='')
        # ), full.names = TRUE, recursive = FALSE )
        gc()  # trigger garbage collection
        return(list(warpedmovout = antsImageClone(warpedmovout, inpixeltype),
          warpedfixout = antsImageClone(warpedfixout, inpixeltype), fwdtransforms = fwdtransforms,
          invtransforms = invtransforms))
      }
      if (!ttexists)
        cat("Problem in arg list \n see usage by calling antsRegistration() w/o arguments \n")
    }
    return(0)
  }
  .Call("antsRegistration", int_antsProcessArguments(c(args)), PACKAGE = "ANTsR")
  gc()  # trigger garbage collection
}

############################################################### antsrmakeRandomString(n, length) function generates a random string random
############################################################### string of the length (length), made up of numbers, small and capital letters
############################################################### helper function
antsrmakeRandomString <- function(n = 1, mylength = 12) {
  randomString <- c(1:n)  # initialize vector
  for (i in 1:n) {
    randomString[i] <- paste(sample(c(0:9, letters, LETTERS), mylength, replace = TRUE),
      collapse = "")
  }
  return(randomString)
}

antsrGetPointerName <- function(img) {
  # if ( Sys.info()['sysname'] == 'Linux' ) endofpointer<-20 if (
  # Sys.info()['sysname'] == 'Darwin' ) endofpointer<-21 pname<- substr(
  # int_antsProcessArguments( list( img ) ) , 11 , endofpointer )
  splitptrname <- strsplit(int_antsProcessArguments(list(img)), " ")[[1]][2]
  pname <- strsplit(splitptrname, ">")
  return(pname[[1]])
}
