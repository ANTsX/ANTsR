#' Perfusion Regression
#'
#' Estimate CBF using standard regression and optionally robust regression.
#'
#'
#' @param mask_img Mask image selects the voxels where CBF will be estimated.
#' Voxels corresponding to logical FALSE are not computed.
#' @param mat Matrix with a column for every time-series voxel. Number of rows
#' equals the number of time units in the series.
#' @param xideal 1D time-series signal to be used a ideal or model for
#' regression.
#' @param nuis Nuisance parameters obtained from '.get_perfusion_predictors'.
#' @param dorobust Real value in interval from 0 to 1.  If greater than 0, then
#' robust regression will be performed.  A typical value would be 0.95 i.e. use
#' voxels with 95 percent confidence.
#' @param skip skip / stride over this number of voxels to increase speed
#' @param selectionValsForRegweights scalar function to guide parameter est.
#' @param useBayesian if greater than zero, use a bayesian prior w/this weight
#' @return Success -- An object of type 'antsImage' containing the CBF estimate
#' for voxels corresponding to the mask input\cr
#' @author Shrinidhi KL Avants BB
#' @examples
#'
#' \dontrun{
#' # predictors -- result of calling '.get_perfusion_predictors'
#' cbf <- perfusionregression( mask_img, mat , predictors$xideal , predictors$nuis )
#' }
#'
#' @export perfusionregression
perfusionregression <- function(mask_img, mat, xideal, nuis = NA,
  dorobust = 0, skip = 20,
  selectionValsForRegweights = NULL, useBayesian = 0) {
  myusage <- "usage: perfusionregression(mask_img , mat , xideal , nuis ,  dorobust = 0, skip = 20 )"
  if (nargs() == 0) {
    print(myusage)
    return(NULL)
  }
  if ( !usePkg("robust") ) { print("Need robust package"); return(NULL) }
  if (missing(mat) | missing(xideal) | missing(nuis)) {
    print("Missing one or more input parameter(s).")
    print(myusage)
    return(NULL)
  }
  cbfform <- formula(mat ~ xideal)
  rcbfform <- formula(mat[, vox] ~ xideal)
  if (!all(is.na(nuis))) {
    rmat <- residuals(lm(mat ~ nuis))
    cbfform <- formula(mat ~ xideal + nuis)
    rcbfform <- formula(rmat[, vox] ~ xideal)
  }
  mycbfmodel <- lm(cbfform)  # standard regression
  cbfi <- antsImageClone(mask_img)
  betaideal <- ((mycbfmodel$coeff)[2, ])
  if (mean(betaideal) < 0)
    betaideal <- (betaideal) * (-1)
  cbfi[mask_img == 1] <- betaideal  # standard results
  if (dorobust == 0) {
    return(list(cbfi = cbfi, indstozero = NA, regweights = rep(1, length(xideal))))
  }
  indstozero <- NULL
  # robust procedure Yohai, V.J. (1987) High breakdown-point and high efficiency
  # estimates for regression.  _The Annals of Statistics_ *15*, 642-65
  ctl <- lmrob.control("KS2011", max.it = 1000)
  regweights <- rep(0, nrow(mat))
  rbetaideal <- rep(0, ncol(mat))
  robvals <- mat * 0
  vox <- 1
  ct <- 0
  visitvals <- (skip:floor((ncol(mat) - 1)/skip)) * skip
  if (skip == 1) {
    visitvals <- 1:ncol(mat)
  }
  rgw <- regweights
  myct <- 0
  if (!all(is.na(selectionValsForRegweights))) {
    vissel <- selectionValsForRegweights[visitvals]
    visselThresh <- 0.8 * max(vissel)
  } else {
    visselThresh <- 0
    vissel <- rep(100, length(visitvals))
  }
  thisct <- 1
  for (vox in visitvals) {
    try(mycbfmodel <- lmrob(rcbfform, control = ctl), silent = T)
    rbetaideal[vox] <- mycbfmodel$coeff[2]
    if (!is.null(mycbfmodel$rweights) & vissel[thisct] > visselThresh) {
      rgw <- rgw + mycbfmodel$rweights
      myct <- myct + 1
      robvals[, myct] <- mycbfmodel$rweights
    }
    thisct <- thisct + 1
  }
  if (skip == 1)
    for (i in 1:nrow(robvals)) {
      temp <- antsImageClone(mask_img)
      temp[mask_img == 1] <- robvals[i, ]
      temp<-smoothImage(temp, 1.5)
      robvals[i, ] <- temp[mask_img == 1]
    }
  regweights <- (rgw/myct)
  if (is.na(mean(regweights)))
    regweights[] <- 1
  # check if the robustness selects the blank part of the time series now use the
  # weights in a weighted regression
  indstozero <- which(regweights < (dorobust * max(regweights)))
  keepinds <- which(regweights > (dorobust * max(regweights)))
  if (length(keepinds) < 20) {
    indstozero <- which(regweights < (0.95 * dorobust * max(regweights)))
    keepinds <- which(regweights > (0.95 * dorobust * max(regweights)))
  }
  if (length(keepinds) < 20) {
    indstozero <- which(regweights < (0.5 * dorobust * max(regweights)))
    keepinds <- which(regweights > (0.5 * dorobust * max(regweights)))
  }
  regweights[indstozero] <- 0  # hard thresholding
  # robvals[indstozero,]<-0 # check if this is a good idea ....
  print(regweights)
  if (skip == 1) {
    for (v in 1:ncol(mat)) {
      rwloc <- robvals[, v]
      indstozero <- which(rwloc < (dorobust * max(rwloc)))
      keepinds <- which(rwloc > (dorobust * max(rwloc)))
      if (length(keepinds) < 20) {
        indstozero <- which(rwloc < (0.95 * dorobust * max(rwloc)))
        keepinds <- which(rwloc > (0.95 * dorobust * max(rwloc)))
      }
      if (length(keepinds) < 20) {
        indstozero <- which(rwloc < (0.5 * dorobust * max(rwloc)))
        keepinds <- which(rwloc > (0.5 * dorobust * max(rwloc)))
      }
      robvals[indstozero, v] <- 0
    }
  }
  # standard weighted regression
  if (dorobust >= 1 | dorobust <= 0) {
    mycbfmodel <- lm(cbfform)
  }
  if (dorobust < 1 & dorobust > 0) {
    mycbfmodel <- lm(cbfform, weights = regweights)
  }
  betaideal <- ((mycbfmodel$coeff)[2, ])
  if (useBayesian > 0) {
    smoothcoeffmat <- mycbfmodel$coefficients
    nmatimgs <- list()
    for (i in 1:nrow(smoothcoeffmat)) {
      temp <- antsImageClone(mask_img)
      temp[mask_img == 1] <- smoothcoeffmat[i, ]
      temp<-smoothImage( temp, 1.5 )
      nmatimgs[[i]] <- antsGetNeighborhoodMatrix(temp, mask_img, rep(1, 3),
        boundary.condition = "mean")
      smoothcoeffmat[i, ] <- temp[mask_img == 1]
    }
    invcov <- solve(cov(t(smoothcoeffmat)))
    betaideal <- rep(0, ncol(mat))
    blmX <- model.matrix(mycbfmodel)
    for (v in 1:ncol(mat)) {
      parammat <- nmatimgs[[1]][, v]
      for (k in 2:length(nmatimgs)) parammat <- cbind(parammat, nmatimgs[[k]][,
        v])
      pcov <- cov(parammat)
      locinvcov <- tryCatch(solve(pcov), error = function(e) return(invcov))
      if (typeof(locinvcov) == "character")
        locinvcov <- invcov
      prior <- (smoothcoeffmat[, v])
      if (skip == 1)
        regweights <- robvals[, v]
      blm <- bayesianlm(blmX, mat[, v], prior, locinvcov * useBayesian, regweights = regweights)
      betaideal[v] <- blm$beta[1]
    }
  }
  if (mean(betaideal) < 0)
    betaideal <- (betaideal) * (-1)
  cbfi[mask_img == 1] <- betaideal  # robust results
  print(paste("Rejected", length(indstozero)/nrow(mat) * 100, " % "))
  return(list(cbfi = cbfi, indstozero = indstozero, regweights = regweights))
}
# y = x beta + c => y - c = x beta
