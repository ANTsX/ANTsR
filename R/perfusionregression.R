perfusionregression <- function(mask_img, mat, xideal, nuis = NA, dorobust = 0, skip = 20, 
  regweights = NULL) {
  getPckg <- function(pckg) install.packages(pckg, repos = "http://cran.r-project.org")
  myusage <- "usage: perfusionregression(mask_img , mat , xideal , nuis ,  dorobust = 0, skip = 20 )"
  if (nargs() == 0) {
    print(myusage)
    return(NULL)
  }
  if (missing(mat) | missing(xideal) | missing(nuis)) {
    print("Missing one or more input parameter(s).")
    print(myusage)
    return(NULL)
  }
  usemat <- mat
  print("standard regression")
  cbfform <- formula(usemat ~ xideal)
  rcbfform <- formula(usemat[, vox] ~ xideal)
  if (!is.na(nuis)) {
    cbfform <- formula(usemat ~ xideal + nuis)
    rcbfform <- formula(usemat[, vox] ~ xideal + nuis)
  }
  mycbfmodel <- lm(cbfform)  # standard regression
  cbfi <- antsImageClone(mask_img)
  betaideal <- ((mycbfmodel$coeff)[2, ])
  if (mean(betaideal) < 0) 
    betaideal <- (betaideal) * (-1)
  cbfi[mask_img == 1] <- betaideal  # standard results
  
  indstozero <- NULL
  if (dorobust > 0 & is.null(regweights)) {
    pckg <- try(require(robust))
    if (!pckg) {
      cat("Installing 'robust' from CRAN\n")
      getPckg("robust")
      require("robust")
    }
    # robust procedure Yohai, V.J. (1987) High breakdown-point and high efficiency
    # estimates for regression.  _The Annals of Statistics_ *15*, 642-65
    if (dorobust > 1) {
      print("dorobust too large, setting to 0.95")
      dorobust <- 0.95
    }
    print(paste("begin robust regression:", dorobust * 100, "%"))
    ctl <- lmrob.control("KS2011", max.it = 1000)
    regweights <- rep(0, nrow(usemat))
    rbetaideal <- rep(0, ncol(usemat))
    robvals <- usemat * 0
    vox <- 1
    ct <- 0
    visitvals <- (skip:floor((ncol(usemat) - 1)/skip)) * skip
    # mynodes <- round(detectCores()/2) # round( getOption('mc.cores', 2L) / 2 )
    # cl<-makeForkCluster( nnodes = mynodes ) registerDoParallel( cl , cores =
    # mynodes )
    rgw <- regweights
    myct <- 0
    ptime <- system.time({
      # rgw<-foreach(vox=visitvals,.combine='+',.init=regweights,.verbose=F) %dopar% {
      for (vox in visitvals) {
        try(mycbfmodel <- lmrob(rcbfform, control = ctl), silent = T)
        rbetaideal[vox] <- mycbfmodel$coeff[2]
        if (!is.null(mycbfmodel$rweights)) {
          rgw <- rgw + mycbfmodel$rweights
          myct <- myct + 1
          robvals[, myct] <- mycbfmodel$rweights
        }
      }
    })
    print(rgw)
    regweights <- (rgw/myct)
    if (is.na(mean(regweights))) 
      regweights[] <- 1
    print(paste("donewithrobreg", myct))
    print(regweights)
    print(paste(ptime))
    # check if the robustness selects the blank part of the time series now use the
    # weights in a weighted regression
    indstozero <- which(regweights < (dorobust * max(regweights)))
    keepinds <- which(regweights > (dorobust * max(regweights)))
    print(paste("dorobust ", dorobust, " i2z ", length(indstozero)))
    if (length(keepinds) < 20) {
      indstozero <- which(regweights < (0.95 * dorobust * max(regweights)))
      keepinds <- which(regweights > (0.95 * dorobust * max(regweights)))
    }
    if (length(keepinds) < 20) {
      indstozero <- which(regweights < (0.5 * dorobust * max(regweights)))
      keepinds <- which(regweights > (0.5 * dorobust * max(regweights)))
    }
    regweights[indstozero] <- 0  # hard thresholding 
    print(regweights)
    mycbfmodel <- lm(cbfform, weights = regweights)  # standard weighted regression
    betaideal <- ((mycbfmodel$coeff)[2, ])
    if (mean(betaideal) < 0) 
      betaideal <- (betaideal) * (-1)
    cbfi[mask_img == 1] <- betaideal  # robust results
    print(paste("Rejected", length(indstozero)/nrow(usemat) * 100, " % "))
  } else if (dorobust > 0 & !is.null(regweights)) {
    mycbfmodel <- lm(cbfform, weights = regweights)  # standard weighted regression
    betaideal <- ((mycbfmodel$coeff)[2, ])
    if (mean(betaideal) < 0) 
      betaideal <- (betaideal) * (-1)
    cbfi[mask_img == 1] <- betaideal  # robust results
  }
  return(list(cbfi = cbfi, indstozero = indstozero, regweights = regweights))
}
# y = x beta + c => y - c = x beta 
