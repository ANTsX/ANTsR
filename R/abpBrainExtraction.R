#' An ants-based brain extraction script.
#'
#' Brain extraction based on mapping a template image and its mask to the input
#' image.  Should be preceded by abpN4.
#'
#' @param img whole head image to which we map a brain mask
#' @param tem Template image (the whole head) which has an associated label mask.
#' @param temmask Template's antsImage brain mask.
#' @param temregmask Template's registration mask including skull but not the face
#' @param regtype registration type: 'SyN' (fast, default), 'SyNabp' (better, slower)
#' @param tdir temporary directory (optional)
#' @return outputs a brain image and brain mask.
#' @author Tustison N, Avants BB
#' @examples
#'
#' fn<-getANTsRData("r16")
#' img<-antsImageRead(fn)
#' img<-resampleImage(img,c(128,128),1,0)
#' tf<-getANTsRData("r27")
#' tem<-antsImageRead(tf)
#' tem<-resampleImage(tem,c(128,128),1,0)
#' temmask<-antsImageClone( tem )
#' temmask[ tem  > 20 ] <- 1
#' temmask[ tem  <= 20 ] <- 0
#' bm<-abpBrainExtraction(img=img,tem=tem,temmask=temmask)
#'
#' @export abpBrainExtraction
abpBrainExtraction <- function(img = NA, tem = NA, temmask = NA,
                               temregmask = NA, regtype='SyN', tdir = NA) {
  if (missing(img) | missing(tem) | missing(temmask)) {
    cat("usage: abpBrainExtraction( img=imgToBExtract, tem = template, temmask = mask ) \n")
    cat(" if no priors are passed, or a numerical prior is passed, then use kmeans \n")
    return(NULL)
  }
  tempriors <- 3
  npriors <- 3

  # file I/O - all stored in temp dir
  if (is.na(tdir)) {
    tdir <- tempdir()
    initafffn <- tempfile(pattern = "antsr", tmpdir = tdir, fileext = "_InitialAff.mat")
    EXTRACTION_WARP_OUTPUT_PREFIX <- tempfile(pattern = "antsr", tmpdir = tdir,
                                              fileext = "_PriorMap")
  } else {
    initafffn <- paste(tdir, "antsr", "_InitialAff.mat", sep = "")
    EXTRACTION_WARP_OUTPUT_PREFIX <- paste(tdir, "antsr", "_PriorMap", sep = "")
  }
#   # ANTs parameters begin
#   ANTS_MAX_ITERATIONS <- "100x100x70x20"
#   ANTS_TRANSFORMATION <- "SyN[0.1,3,0]"
#   ANTS_LINEAR_METRIC_PARAMS <- "1,32,Regular,0.25"
#   ANTS_LINEAR_CONVERGENCE <- "[1000x1000x1000x10,1e-7,15]"
#   ANTS_LINEAR_CONVERGENCEFAST <- "[10x0x0x0,1e-7,10]"
#   ANTS_METRIC <- "CC"
#   ANTS_METRIC_PARAMS <- "1,4"
#   # ANTs parameters end

  # atropos params
  locmrf<-paste(rep(1,img@dimension),collapse='x')
  ATROPOS_BRAIN_EXTRACTION_INITIALIZATION <- "kmeans[3]"
  ATROPOS_BRAIN_EXTRACTION_LIKELIHOOD <- "Gaussian"
  ATROPOS_BRAIN_EXTRACTION_CONVERGENCE <- "[3,0.0001]"
  ATROPOS_BRAIN_EXTRACTION_MRF <- paste("[0.2,",locmrf,"]")
  ATROPOS_SEGMENTATION_INITIALIZATION <- "PriorProbabilityImages"
  ATROPOS_SEGMENTATION_PRIOR_WEIGHT <- 0
  ATROPOS_SEGMENTATION_LIKELIHOOD <- "Gaussian"
  ATROPOS_SEGMENTATION_CONVERGENCE <- "[12,0.0001]"
  ATROPOS_SEGMENTATION_POSTERIOR_FORMULATION <- "Socrates"
  ATROPOS_SEGMENTATION_MRF <- paste("[0.11,",locmrf,"]")
  # atropos params end

  imgsmall <- resampleImage(img , rep(4, img@dimension) )
  temsmall <- resampleImage(tem , rep(4, img@dimension) )
  # careful initialization of affine mapping , result stored in initafffn
  if (!file.exists(initafffn)) {
    if (is.na(temregmask))
      temp<-affineInitializer(
              fixedImage=temsmall, movingImage=imgsmall,
              searchFactor=15, radianFraction=0.1, usePrincipalAxis=0,
              localSearchIterations=10, txfn=initafffn )
    else
      temp<-affineInitializer(
              fixedImage=temsmall, movingImage=imgsmall,
              searchFactor=15, radianFraction=0.1, usePrincipalAxis=0,
              localSearchIterations=10, txfn=initafffn, mask=temregmask )
  }

  # get laplacian images
  lapi = iMath(img, "Laplacian", 1.5, 1)
  lapt = iMath(tem, "Laplacian", 1.5, 1)

  # FIXME should add mask to below via -x option
  dtem <- antsImageClone(tem, "double")
  dimg <- antsImageClone(img, "double")
#   antsregparams <- list(d = img@dimension, u = 1,
#                         o = EXTRACTION_WARP_OUTPUT_PREFIX,
#                         r = initafffn, z = 1, w = "[0.025,0.975]",
#                         m = paste("mattes[", antsrGetPointerName(antsImageClone(lapt,
#                                                                                 "double")), ",", antsrGetPointerName(antsImageClone(lapi, "double")),
#                                   ",", "0.5,32]", sep = ""),
#                         c = "[50x50x50x10,1e-9,15]", t = "SyN[0.1,3,0]",
#                         f = "6x4x2x1", s = "4x2x1x0")
  outprefix <- EXTRACTION_WARP_OUTPUT_PREFIX
  mytx<-antsRegistration( tem, img, typeofTransform = regtype,
                          initialTransform=initafffn, mask=temregmask )
  fwdtransforms <- mytx$fwdtransforms
  invtransforms <- mytx$invtransforms
  temmaskwarped <- antsApplyTransforms( img, temmask,
                                        transformlist = invtransforms,
                                        interpolator = "nearestNeighbor" )
  temmaskwarped<-thresholdImage( temmaskwarped, 0.5, 1 )
  tmp <- antsImageClone(temmaskwarped)

  tmp = iMath(temmaskwarped, "MD", 2)
  tmp = iMath(tmp, "GetLargestComponent", 2)
  tmp = iMath(tmp, "FillHoles")
  gc()
  seg <- antsImageClone(img, "unsigned int")
  tmpi <- antsImageClone(tmp, "unsigned int")
  atroparams <- list(d = img@dimension, a = img,
                     m = ATROPOS_BRAIN_EXTRACTION_MRF,
                     o = seg, x = tmpi,
                     i = ATROPOS_BRAIN_EXTRACTION_INITIALIZATION,
                     c = ATROPOS_BRAIN_EXTRACTION_CONVERGENCE,
                     k = ATROPOS_BRAIN_EXTRACTION_LIKELIHOOD)
  atropos(atroparams)
  fseg <- antsImageClone(  seg, "float")
  segwm<-thresholdImage(  fseg, 3, 3 )
  seggm<-thresholdImage(  fseg, 2, 2)
  segcsf<-thresholdImage( fseg, 1, 1)
  segwm = iMath(segwm, "GetLargestComponent")
  seggm = iMath(seggm, "GetLargestComponent")
  seggm = iMath(seggm, "FillHoles")
  segwm[segwm > 0.5] <- 3
  tmp = iMath(segcsf, "ME", 10)
  seggm[seggm < 0.5 & tmp > 0.5] <- 2
  seggm[seggm > 0.5] <- 2
  finalseg <- antsImageClone(img)
  finalseg[finalseg > 0] <- 0
  finalseg[seggm > 0.5] <- 2
  finalseg[segwm > 0.5 & seggm < 0.5] <- 3
  finalseg[segcsf > 0.5 & seggm < 0.5 & segwm < 0.5] <- 1
  # BA - finalseg looks good! could stop here
  tmp<-thresholdImage( finalseg, 2, 3)

  tmp = iMath(tmp, "ME", 2)
  tmp = iMath(tmp, "GetLargestComponent", 2)
  tmp = iMath(tmp, "MD", 4)
  tmp = iMath(tmp, "FillHoles")
  tmp[tmp > 0 | temmaskwarped > 0.25] <- 1
  tmp = iMath(tmp, "MD", 5)
  tmp = iMath(tmp, "ME", 5)
  finalseg2 = iMath(tmp, "FillHoles")

  # FIXME - steps above should all be checked again ...
  dseg = iMath(finalseg2, "ME", 5)
  dseg = iMath(dseg, "MaurerDistance")
  droundmax <- 20
  dsearchvals <- c(1:100)/100 * droundmax - 0.5 * droundmax
  mindval <- min(dseg)
  loval <- mindval
  distmeans <- rep(0, length(dsearchvals))
  ct <- 1
  for (dval in (dsearchvals)) {
    loval <- (dval - 1.5)
    dsegt <- antsImageClone(dseg)
    dsegt[dsegt >= loval & dsegt < dval] <- 1
    distmeans[ct] <- mean(img[dsegt == 1])
    ct <- ct + 1
  }
  localmin <- which.min(distmeans)
  dthresh <- dsearchvals[localmin]
  bmask <- antsImageClone(finalseg2)
  bmask<-thresholdImage( dseg, mindval, dthresh )
  brain <- antsImageClone(img)
  brain[finalseg2 < 0.5] <- 0
  return(list(brain = brain, bmask = finalseg2,
              kmeansseg = seg, fwdtransforms = fwdtransforms,
              invtransforms = invtransforms,
              temmaskwarped = temmaskwarped, distmeans = distmeans,
              dsearchvals = dsearchvals))
}
