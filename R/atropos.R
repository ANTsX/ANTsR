#' FMM Segmentation
#'
#' A finite mixture modeling (FMM) segmentation approach with possibilities for
#' specifying prior constraints. These prior constraints include the
#' specification of a prior label image, prior probability images (one for each
#' class), and/or an MRF prior to enforce spatial smoothing of the labels.
#' Similar algorithms include FAST and SPM.  atropos can also perform
#' multivariate segmentation if you pass a list of images in: e.g.
#' a=c(img1,img2).
#'
#' @param a One or more scalar images is specified for segmentation using the
#' -a/--intensity-image option. For segmentation scenarios with no prior
#' information, the first scalar image encountered on the command line is used
#' to order labelings such that the class with the smallest intensity signature
#' is class '1' through class 'N' which represents the voxels with the largest
#' intensity values. The optional adaptive smoothing weight parameter is
#' applicable only when using prior label or probability images. This scalar
#' parameter is to be specified between [0,1] which smooths each labeled region
#' separately and modulates the intensity measurement at each voxel in each
#' intensity image between the original intensity and its smoothed counterpart.
#' The smoothness parameters are governed by the -b/--bspline option.
#' @param x mask image
#' @param i initialization see atropos in ANTs for full set of options
#' @param m mrf parameters see atropos for format of these
#' @param c convergence parameters see atropos for format of these
#' @param priorweight usually 0, 0.25 or 0.5
#' @param ... more parameters, see atropos help in ANTs
#' @return 0 -- Success\cr 1 -- Failure
#' @author Shrinidhi KL, B Avants
#' @examples
#'
#' img<-antsImageRead( getANTsRData("r16") , 2 )
#' img<-resampleImage( img, c(64,64), 1, 0 )
#' mask<-getMask(img)
#' segs1<-atropos( d = 2, a = img, m = '[0.2,1x1]',
#'    c = '[2,0]',  i = 'kmeans[3]', x = mask )
#' @export atropos
atropos <- function( a, x,
  i = "kmeans[3]",
  m = "[0.2,1x1]",
  c = "[5,0]",
  priorweight = 0.5,
  ...) {
  if ( missing(x)) {
    .Call("atropos", .int_antsProcessArguments(c(a)), PACKAGE = "ANTsR")
    return(NULL)
  }
  # define the output temp files
  tdir <- tempdir()
  probs <- tempfile(pattern = "antsr", tmpdir = tdir, fileext = "prob%02d.nii.gz")
  probsbase <- basename(probs)
  searchpattern <- sub("%02d", "*", probsbase)
  # categorize the initialization type - kmeans or spatial priors
  ct <- 1
  if (length(i) > 1) {
    # then spatial priors

    while (ct <= length(i)) {
      probchar <- paste(ct, sep = "")
      if (ct < 10)
        probchar <- paste("0", probchar, sep = "")
      tempfn <- sub("%02d", probchar, probs)
      antsImageWrite(i[[ct]], tempfn)
      ct <- ct + 1
    }
    i <- paste("PriorProbabilityImages[", length(i), ",", probs, ",", priorweight,
      "]", sep = "")
    print(i)
  }
  if (typeof(a) == "list")
    outimg <- antsImageClone(a[[1]], "unsigned int") else outimg <- antsImageClone(a, "unsigned int")
  mydim <- as.numeric(outimg@dimension)
  outs <- paste("[", .antsrGetPointerName(outimg), ",", probs, "]", sep = "")
  mymask <- antsImageClone(x, "unsigned int")
  if (length(a) == 1)
    myargs <- list(d = mydim, a = a, m = m, o = outs, c = c, m = m, i = i, x = mymask,
      ...)
  if (length(a) == 2)
    myargs <- list(d = mydim, a = a[[1]], a = a[[2]], m = m, o = outs, c = c, m = m,
      i = i, x = mymask, ...)
  if (length(a) == 3)
    myargs <- list(d = mydim, a = a[[1]], a = a[[2]], a = a[[3]], m = m, o = outs,
      c = c, m = m, i = i, x = mymask, ...)
  if (length(a) == 4) {
    myargs <- list(d = mydim, a = a[[1]], a = a[[2]], a = a[[3]],
      a = a[[4]], m = m, o = outs,
      c = c, m = m, i = i, x = mymask, ...)
  }
  if (length(a) == 5) {
    myargs <- list(d = mydim, a = a[[1]], a = a[[2]], a = a[[3]],
      a = a[[4]], a = a[[5]], m = m, o = outs,
      c = c, m = m, i = i, x = mymask, ...)
  }
  if (length(a) >= 6) {
    myargs <- list(d = mydim, a = a[[1]], a = a[[2]], a = a[[3]],
      a = a[[4]], a = a[[5]], a = a[[6]], m = m, o = outs,
      c = c, m = m, i = i, x = mymask, ...)
  }
  if ( length(a) > 6)
    print(" more than 6 input images not really supported, using first 6 ")
  .Call("atropos", .int_antsProcessArguments(c(myargs)), PACKAGE = "ANTsR")
  probsout <- list.files(path = tdir,
    pattern = glob2rx(searchpattern), full.names = TRUE,
    recursive = FALSE)
  pimg <- antsImageRead(probsout[1], mydim)
  probimgs <- c(pimg)
  for (x in c(2:length(probsout))) {
    probimgs <- c(probimgs, antsImageRead(probsout[x], mydim))
  }
  outimg=antsImageClone( outimg, 'float' )
  return(list( segmentation = outimg, probabilityimages = probimgs ))
}
