#' a basic framework for network analysis that produces graph metrics
#'
#' An implementation of a network analysis framework for BOLD data. We expect
#' that you mapped a label image ( e.g. aal ) to the 3D BOLD space. We build a
#' network and graph metrics from this image and these labels based on the
#' user-defined graph density level.
#'
#' @param bold input 4D image
#' @param mask antsImage defines areas of interest
#' @param labels antsImage defines regions of interest ie a parcellation
#' @param motion motion parameters - if missing, will estimate from data
#' @param gdens graph density applied to network covariance matrix
#' @param threshLo lower threshold for the label image
#' @param threshHi upper threshold for the label image
#' @param freqLo lower frequency cutoff
#' @param freqHi upper frequency cutoff
#' @param winsortrim winsorize the bold signal by these values eg 0.02
#' @param throwaway this number of initial bold volumes
#' @return list of outputs
#' @author BB Avants
#' @examples
#' # none yet - this is not very well tested with recent ANTsR
#' \dontrun{
#' myimg <- antsImageRead(getANTsRData("ch2"), 3)
#' mylab <- antsImageRead(getANTsRData("ch2a"), 3)
#' boldfn <- getANTsRData("pcasl")
#' bold <- antsImageRead(boldfn, 4)
#' avgbold <- getAverageOfTimeSeries(bold)
#' breg <- antsRegistration(avgbold, myimg, typeofTransform = c("AffineFast"))
#' warpedParcellation <- antsApplyTransforms(avgbold, mylab,
#'   transformlist = breg$fwdtransforms, interpolator = "NearestNeighbor"
#' )
#' mask <- getMask(avgbold)
#' warpedParcellation <- maskImage(warpedParcellation, img.mask = mask)
#' old <- NA
#' labels <- warpedParcellation
#' gdens <- 0.2
#' threshLo <- 1
#' threshHi <- 90
#' freqLo <- 0.01
#' freqHi <- 0.1
#' winsortrim <- 0.02
#' result <- antsBOLDNetworkAnalysis(bold = bold, mask = mask, warpedParcellation)
#' }
#' @export antsBOLDNetworkAnalysis
antsBOLDNetworkAnalysis <- function(
    bold = NULL, mask = NULL,
    labels = NULL, motion,
    gdens = 0.2, threshLo = 1, threshHi = 90,
    freqLo = 0.01, freqHi = 0.1, winsortrim = 0.02,
    throwaway) {
  myscale <- function(x, doscale = FALSE) {
    if (doscale) {
      return(scale(x))
    }
    return(x)
  }
  # if ( !usePkg("glasso") ) { print("Need glasso package"); return(NULL) }
  # if ( !usePkg("igraph") ) { print("Need igraph package"); return(NULL) }
  stopifnot(!is.null(bold))
  bold <- check_ants(bold)
  mytimes <- dim(bold)[4]
  aalm <- check_ants(labels)
  mask <- check_ants(mask)
  aalmask <- antsImageClone(aalm)
  mylog <- (aalm >= threshLo & aalm <= threshHi & mask > 0.5)
  # aalmask[mylog] <- 1
  # aalmask[!mylog] <- 0
  aalmask <- mylog
  aalm[!mylog] <- 0
  omat <- myscale(timeseries2matrix(bold, aalmask))
  if (missing(motion)) {
    temp <- antsMotionCalculation(bold)
    motion <- temp$moco_params
  }
  templateFD <- rep(0, nrow(motion))
  DVARS <- rep(0, nrow(motion))
  for (i in 2:nrow(motion)) {
    mparams1 <- c(motion[i, 3:14])
    tmat1 <- matrix(as.numeric(mparams1[1:9]), ncol = 3, nrow = 3)
    mparams2 <- c(motion[i - 1, 3:14])
    tmat2 <- matrix(as.numeric(mparams2[1:9]), ncol = 3, nrow = 3)
    pt <- t(matrix(rep(10, 3), nrow = 1))
    newpt1 <- data.matrix(tmat1) %*% data.matrix(pt) + as.numeric(mparams1[10:12])
    newpt2 <- data.matrix(tmat2) %*% data.matrix(pt) + as.numeric(mparams1[10:12])
    templateFD[i] <- sum(abs(newpt2 - newpt1))
    DVARS[i] <- sqrt(mean((omat[i, ] - omat[i - 1, ])^2))
  }
  # question - should this be a constant of 0.2 as recommended in Yan Craddock He
  # Milham?
  if (missing(throwaway)) {
    throwaway <- round(10.0 / antsGetSpacing(bold)[4])
  }
  keepinds <- which(templateFD < (mean(templateFD) + 2 * sd(templateFD)) & ((1:mytimes) >
    throwaway))
  keepinds <- c(throwaway, keepinds)
  throwinds <- which(templateFD > (mean(templateFD) + 2 * sd(templateFD)) & ((1:mytimes) >
    throwaway))
  doimpute <- TRUE
  if (length(throwinds) > 0 & doimpute) {
    for (i in throwinds) {
      previ <- max(keepinds[keepinds < i])
      nexti <- min(keepinds[keepinds > i])
      wt1 <- 1 - abs(i - previ) / (nexti - previ)
      wt2 <- 1 - abs(i - nexti) / (nexti - previ)
      omat[i, ] <- wt1 * omat[previ, ] + omat[nexti, ] * wt2
      DVARS[i] <- wt1 * DVARS[previ] + DVARS[nexti] * wt2
      templateFD[i] <- wt1 * templateFD[previ] + templateFD[nexti] * wt2
      motion[i, ] <- wt1 * motion[previ, ] + motion[nexti, ] * wt2
    }
  }
  keepinds <- throwaway:mytimes
  usemotiondirectly <- TRUE
  if (usemotiondirectly) {
    motionnuis <- as.matrix(motion[keepinds, 3:ncol(motion)])
  }
  colnames(motionnuis) <- paste("mot", 1:ncol(motionnuis), sep = "")
  bkgd <- TRUE
  if (bkgd) {
    negmask <- antsImageClone(mask)
    negmask <- negmask == 0
    # neginds <- which(as.array(backgroundvoxels))
    # negmask[negmask >= 0] <- 0
    # backgroundvoxels[] <- FALSE
    # backgroundvoxels[neginds] <- TRUE
    # negmask[backgroundvoxels] <- 1
    negmask <- iMath(negmask, "ME", 1)
    tempmat <- myscale(timeseries2matrix(bold, negmask)[keepinds, ])
    bgsvd <- svd(tempmat)
    mysum <- cumsum(bgsvd$d) / sum(bgsvd$d)
    newnuisv <- min(c(10, which(mysum > 0.8)[1]))
    bgdnuis <- bgsvd$u[, 1:newnuisv]
    colnames(bgdnuis) <- paste("bgdNuis", 1:newnuisv, sep = "")
  }
  omat <- omat[keepinds, ]
  ##################################################
  classiccompcor <- compcor(omat, mask = mask, ncompcor = 4)
  omotionnuis <- as.matrix(motion[keepinds, 3:ncol(motion)])
  if (!usePkg("magic")) {
    print("Need magic package")
    return(NULL)
  }
  motnuisshift <- magic::ashift(omotionnuis, c(1, 0))
  motmag <- apply(omotionnuis, FUN = mean, MARGIN = 2)
  matmag <- sqrt(sum(motmag[1:9] * motmag[1:9]))
  tranmag <- sqrt(sum(motmag[10:12] * motmag[10:12]))
  motsd <- apply(omotionnuis - motnuisshift, FUN = mean, MARGIN = 2)
  matsd <- sqrt(sum(motsd[1:9] * motsd[1:9]))
  transd <- sqrt(sum(motsd[10:12] * motsd[10:12]))
  dmatrix <- (omotionnuis - motnuisshift)[, 1:9]
  dtran <- (omotionnuis - motnuisshift)[, 10:12]
  dmatrixm <- apply(dmatrix * dmatrix, FUN = sum, MARGIN = 1)
  dtranm <- apply(dtran * dtran, FUN = sum, MARGIN = 1)
  if (bkgd) {
    mynuis <- cbind(
      scale(dmatrixm)[, 1], scale(dtranm)[, 1], classiccompcor,
      bgdnuis, templateFD[keepinds], DVARS[keepinds]
    )
  } else {
    mynuis <- cbind(
      scale(dmatrixm)[, 1], scale(dtranm)[, 1], classiccompcor,
      templateFD[keepinds], DVARS[keepinds]
    )
  }
  colnames(mynuis)[1:2] <- c("dmatrix", "dtran")
  colnames(mynuis)[(length(colnames(mynuis)) - 1):length(colnames(mynuis))] <- c(
    "FD",
    "DVARS"
  )
  mytimes <- dim(omat)[1]
  mat <- myscale(residuals(lm(omat ~ mynuis)), doscale = TRUE)
  flo <- freqLo
  fhi <- freqHi
  mynetwork <- filterfMRIforNetworkAnalysis(mat,
    tr = antsGetSpacing(bold)[4],
    mask = aalmask, cbfnetwork = "BOLD", labels = aalm, graphdensity = as.numeric(gdens),
    freqLo = flo, freqHi = fhi, usesvd = FALSE
  ) # , nuisancein = locmotnuis )
  ################################################## now finalize it all ############### return network values on full dataset
  names(matmag) <- "MatrixMotion"
  names(tranmag) <- "TransMotion"
  names(matsd) <- "DMatrixMotion"
  names(transd) <- "DTransMotion"
  mydeg <- mynetwork$graph$degree
  names(mydeg) <- paste("Deg", 1:length(mynetwork$graph$degree), sep = "")
  mypr <- mynetwork$graph$pagerank
  names(mypr) <- paste("PR", 1:length(mynetwork$graph$degree), sep = "")
  mycent <- mynetwork$graph$centrality
  names(mycent) <- paste("Cent", 1:length(mynetwork$graph$degree), sep = "")
  mytrans <- mynetwork$graph$localtransitivity
  names(mytrans) <- paste("Trans", 1:length(mynetwork$graph$degree), sep = "")
  myclose <- mynetwork$graph$closeness
  names(myclose) <- paste("Close", 1:length(mynetwork$graph$degree), sep = "")
  mybtwn <- mynetwork$graph$betweeness
  names(mybtwn) <- paste("Btwn", 1:length(mynetwork$graph$degree), sep = "")
  mytimes <- length(keepinds)
  names(mytimes) <- "NTimePoints"
  meanFD <- mean(templateFD)
  names(meanFD) <- "meanFD"
  meanDVARS <- mean(DVARS)
  names(meanDVARS) <- "meanDVARS"
  myc <- c(
    matmag, tranmag, matsd, transd, mydeg, mypr, mycent, mytrans, myclose,
    mybtwn, mytimes, meanFD, meanDVARS
  )
  outmat <- matrix(myc, nrow = 1)
  colnames(outmat) <- names(myc)
  return(list(
    subjectSummary = outmat, mynetwork = mynetwork,
    boldmask = aalmask, nuisanceVariables = mynuis
  ))
  ##################################################
}
