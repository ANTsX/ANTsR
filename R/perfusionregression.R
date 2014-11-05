perfusionregression <- function(mask_img, mat, xideal,
  nuis = NA, dorobust = 0, skip = 20,
  selectionValsForRegweights = NULL,
  useBayesian=0 )
  {
  getPckg <- function(pckg) install.packages(pckg, repos = "http://cran.r-project.org")
  myusage <- "usage: perfusionregression(mask_img , mat , xideal , nuis ,  dorobust = 0, skip = 20 )"
  if (nargs() == 0) {
    print(myusage)
    return(NULL)
  }
  pckg <- try(require(robust))
  if (!pckg) {
    cat("Installing 'robust' from CRAN\n")
    getPckg("robust")
    require("robust")
  }
  if (missing(mat) | missing(xideal) | missing(nuis)) {
    print("Missing one or more input parameter(s).")
    print(myusage)
    return(NULL)
  }
  cbfform <- formula(mat ~ xideal)
  rcbfform <- formula(mat[, vox] ~ xideal)
  if (!is.na(nuis)) {
    cbfform <- formula(mat ~ xideal + nuis)
    rcbfform <- formula(mat[, vox] ~ xideal + nuis)
  }
  mycbfmodel <- lm(cbfform)  # standard regression
  cbfi <- antsImageClone(mask_img)
  betaideal <- ((mycbfmodel$coeff)[2, ])
  if (mean(betaideal) < 0)
    betaideal <- (betaideal) * (-1)
  cbfi[mask_img == 1] <- betaideal  # standard results
  indstozero <- NULL
  # robust procedure Yohai, V.J. (1987)
  # High breakdown-point and high efficiency
  # estimates for regression.  _The Annals of Statistics_ *15*, 642-65
  ctl <- lmrob.control("KS2011", max.it = 1000)
  regweights <- rep(0, nrow(mat))
  rbetaideal <- rep(0, ncol(mat))
  robvals <- mat * 0
  vox <- 1
  ct <- 0
  visitvals <- (skip:floor((ncol(mat) - 1)/skip)) * skip
  if ( skip == 1 ) {
    visitvals<-1:ncol(mat)
  }
  rgw <- regweights
  myct <- 0
    if ( !all(is.na(selectionValsForRegweights))) {
      vissel<-selectionValsForRegweights[visitvals]
      visselThresh<-0.8 * max(vissel)
    } else {
      visselThresh<-0
      vissel<-rep(100,length(visitvals))
    }
    thisct<-1
    for (vox in visitvals)
      {
      try(mycbfmodel <- lmrob(rcbfform, control = ctl), silent = T)
      rbetaideal[vox] <- mycbfmodel$coeff[2]
      if ( !is.null(mycbfmodel$rweights) &
            vissel[thisct] > visselThresh )
        {
        rgw <- rgw + mycbfmodel$rweights
        myct <- myct + 1
        robvals[, myct] <- mycbfmodel$rweights
        }
      thisct<-thisct+1
      }
    if ( skip == 1 )
    for ( i in 1:nrow(robvals) )
      {
      temp<-antsImageClone( mask_img )
      temp[ mask_img == 1 ] <- robvals[i,]
      SmoothImage(3,temp,5.0,temp)
      robvals[i,]<-temp[ mask_img==1 ]
      }
    regweights <- (rgw/myct)
    if ( is.na(mean(regweights)) ) regweights[] <- 1
    # check if the robustness selects the blank part
    # of the time series
    # now use the weights in a weighted regression
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
    print(regweights)
    # standard weighted regression
    if ( dorobust >= 1 | dorobust <= 0 )
      mycbfmodel <- lm( cbfform )
    if ( dorobust < 1 & dorobust > 0 )
      mycbfmodel <- lm(cbfform, weights = regweights)
    betaideal <- ((mycbfmodel$coeff)[2, ])
    if ( useBayesian > 0 )
    {
    smoothcoeffmat<-mycbfmodel$coefficients
    nmatimgs<-list()
    for ( i in 1:nrow(smoothcoeffmat) )
      {
      temp<-antsImageClone( mask_img )
      temp[ mask_img == 1 ] <- smoothcoeffmat[i,]
      SmoothImage(3,temp,2.0,temp)
      nmatimgs[[i]]<-antsGetNeighborhoodMatrix(temp,mask_img,rep(1,3))
      smoothcoeffmat[i,]<-temp[ mask_img==1 ]
      }
    invcov <- solve( cov( t( smoothcoeffmat ) ) )
    betaideal<-rep(0,ncol(mat))
    blmX<-model.matrix( mycbfmodel )
    for ( v in 1:ncol(mat) )
      {
      parammat<-nmatimgs[[1]][,v]
      for ( k in 2:length(nmatimgs))
        parammat<-cbind( parammat, nmatimgs[[k]][,v] )
      locinvcov<-solve( cov( parammat ) )
      prior<-(smoothcoeffmat[,v])
      if ( skip == 1 ) regweights<-robvals[,v]
      blm<-bayesianlm(  blmX, mat[,v], prior, invcov*useBayesian,
        regweights=regweights )
      betaideal[v]<-blm$beta[1]
      }
    }
    if ( mean(betaideal) < 0) betaideal <- (betaideal) * (-1)
    cbfi[mask_img == 1] <- betaideal  # robust results
    print(paste("Rejected", length(indstozero)/nrow(mat) * 100, " % "))
  return(list(cbfi = cbfi, indstozero = indstozero, regweights = regweights))
}
# y = x beta + c => y - c = x beta
