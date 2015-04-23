#' Censor bad volumes from ASL data.
#' 
#' @param asl input asl image
#' @param mask mask for calculating perfusion 
#' @param method one of 'outlier', 'robust', or 'scor'.  See \code{Details}.
#' @return vector of the same length as number of timepoints in \code{asl}, with 
#'  1 indicating the corresponding timepoint is included and 0 indicating exclusion.
#' @author Kandel BM 
#' @examples 
#' nvox <- 5 * 5 * 5 * 10
#' dims <- c(5, 5, 5, 10)
#' asl <- makeImage(dims, rnorm(nvox) + 500) %>% iMath("PadImage", 2)
#' censored <- aslCensoring(asl)
#' @export aslCensoring 

aslCensoring <- function(asl, mask=NA, nuis=NA, method='outlier') {
  # Supporting functions for censoring data: robSelection and scor.
  robSelection <- function(mat, xideal, nuis=NA,  robthresh=0.95, skip=20) {
    cbfform <- formula(mat ~ xideal)
    rcbfform <- formula(mat[, vox] ~ xideal)
    if (!all(is.na(nuis))) {
      rmat <- residuals(lm(mat ~ nuis))
      cbfform <- formula(mat ~ xideal + nuis)
      rcbfform <- formula(rmat[, vox] ~ xideal)
    }  
    if (!all(is.na(nuis))) {
      rmat <- residuals(lm(mat ~ nuis))
      cbfform <- formula(mat ~ xideal + nuis)
      rcbfform <- formula(rmat[, vox] ~ xideal)
    }
    mycbfmodel <- lm(cbfform)  # standard regression
    betaideal <- ((mycbfmodel$coeff)[2, ])
    if (mean(betaideal) < 0) {
      betaideal <- (betaideal) * (-1)
    }
    cbfi[mask_img == 1] <- betaideal  # standard results
    indstozero <- NULL
    ctl <- robustbase::lmrob.control("KS2011", max.it = 1000)
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
    thisct <- 1
    for (vox in visitvals) {
      try(mycbfmodel <- robustbase::lmrob(rcbfform, control = ctl), silent = T)
      rbetaideal[vox] <- mycbfmodel$coeff[2]
      if (!is.null(mycbfmodel$rweights)) {
        rgw <- rgw + mycbfmodel$rweights
        myct <- myct + 1
        robvals[, myct] <- mycbfmodel$rweights
      }
      thisct <- thisct + 1
    }
    regweights <- (rgw/myct)
    if (is.na(mean(regweights))) {
      regweights[] <- 1
    }
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
    if (dorobust < 1 & dorobust > 0) {
      mycbfmodel <- lm(cbfform, weights = regweights)
    }
    betaideal <- ((mycbfmodel$coeff)[2, ])
    if (mean(betaideal) < 0) {
      betaideal <- (betaideal) * (-1)
    }
    indstozero
  }

  scor <- function(asl){
    npairs <- dim(asl)[1]
    indices <- 1:npairs
    meancbf <- apply(asl, 1, mean)
    var.tot <- var(meancbf) 
    var.prev <- var.tot + 1
    while(var.tot < var.prev){
      print(paste(var.prev, var.tot))
      var.prev <- var.tot
      meancbf.prev <- meancbf
      cc <- rep(NA, npairs)
      for(timepoint in 1:npairs){
        if (is.na(indices[timepoint]))
          next
        tmp <- asl[, timepoint]
        cc[timepoint] <- cor(meancbf, tmp)
      }
      indices[which.max(cc)] <- NA
      meancbf <- apply(asl[, indices[!is.na(indices)]], 1, mean)
      var.tot <- var(meancbf)
    }
    indices.out <- rep(1, length(indices)) 
    indices.out[which(is.na(indices))] <- 0 
  }
  
  if (is.na(mask)){
    myar <- apply(as.array(asl), c(1, 2, 3), mean) 
    img <- makeImage(dim(myar), myar) 
    antsSetSpacing(img, antsGetSpacing(asl)[1:3])
    antsSetOrigin(img, antsGetOrigin(asl)[1:3])
    antsSetDirection(img, antsGetDirection(asl)[1:3, 1:3])
    mask <- getMask(img)
  }
  ts <- timeseries2matrix(asl, mask)
  
  if (method == 'robust') {
    xideal <- (rep(c(1, 0), 
      dim(mat)[1])[1:dim(mat)[1]] - 0.5)  # control minus tag
    inds <- robSelection(ts, xideal, nuis)
  } else if (method == 'outlier') {
    inds <- aslOutlierRejection(asl, mask)
  } else if (method == 'scor') {
    inds <- scor(ts) 
  }

} 
