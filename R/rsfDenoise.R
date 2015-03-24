#' WIP: data-driven denoising for resting state fMRI
#'
#' Uses a target function to denoise resting bold data
#'
#' @param boldmatrix input bold matrix
#' @param targety target to predict
#' @param motionparams motion parameters / nuisance variables
#' @param selectionthresh e.g. 0.1 take 10 percent worst variables for noise
#' estimation
#' @param maxnoisepreds integer search range e.g 1:10
#' @param debug boolean
#' @param polydegree eg 4 for polynomial nuisance variables
#' @param crossvalidationgroups prior defined or integer valued
#' @param tr bold tr
#' @param scalemat boolean
#' @param noisepoolfun function to help select noise pool e.g. max
#' @return matrix is output
#' @author Avants BB
#' @examples
#'
#' \dontrun{
#' # if (!exists("fn") ) fn<-getANTsRData("pcasl")
#' # bold <- antsImageRead( fn )
#' # avgbold<-getAverageOfTimeSeries(bold)
#' # boldmask<-getMask( avgbold )
#' # roimask<-antsImageRead("roi.nii.gz")
#' # timeselect<-10:(dim(bold)[4]-10)
#' # # can do this if you like its approach
#' # cleanfMRI <- preprocessfMRI( bold, maskImage=boldmask,
#' #   frequencyLowThreshold = 0.01, frequencyHighThreshold = 0.1,
#' #   spatialSmoothingType = "gaussian", spatialSmoothingParameters = 2,
#' #   residualizeMatrix=TRUE, numberOfCompCorComponents=2 )
#' # boldmat<-timeseries2matrix( cleanfMRI$cleanBoldImage, cleanfMRI$maskImage )
#' # roimat<-timeseries2matrix( cleanfMRI$cleanBoldImage, roimask )
#' # roimean<-rowMeans( roimat ) # svd instead?
#' # roimat<-matrix( roimean, ncol=1)
#' # dnz<-rsfDenoise( boldmat[timeselect,] ,
#' #   roimat[timeselect,1], motionparams=NA,
#' #   polydegree=1, crossvalidationgroups = 8, maxnoisepreds=c(2:4), debug=F )
#' # # might iterate over above to further refine noise variables
#' # mdl<-bigLMStats( lm( boldmat[timeselect,] ~ roimean[timeselect] +
#' #    dnz$polys + dnz$noiseu  ), 0.001 )
#' # betas<-mdl$beta.t[1,]
#' # sum(betas[betas > 3])
#' # betaimg<-antsImageClone( boldmask )
#' # betaimg[ boldmask == 1 ]<-betas
#' # antsImageWrite( betaimg, "betas2.nii.gz" )
#' #
#' # # more complex
#' # bold<-antsImageRead("bold.nii.gz")
#' # boldmask<-antsImageRead("meanboldmask.nii.gz")
#' # aalimg<-antsImageRead("meanboldAALmask.nii.gz")
#' # data("aal",package="ANTsR")
#' # dmnlabels<-aal$label_num[aal$isdmn>0]
#' # aalvec<-aalimg > 0
#' # whichregion<-3
#' # for ( i in 1:max(aalimg) )
#' #   {
#' #   if ( ! ( i %in% dmnlabels[whichregion] ) )
#' #     {
#' #     aalimg[ aalimg == as.numeric(i) ]<-0
#' #     }
#' #   }
#' # maskvec<-boldmask > 0 & aalimg == whichregion
#' # boldmask[ maskvec ]<-0
#' # timeselect<-10:dim(bold)[4]
#' # if ( ! exists("moco") ) {
#' #   moco<-.motion_correction(bold,moreaccurate=1)
#' #   moco<-as.matrix( moco$moco_params )[timeselect,3:ncol(moco$moco_params)]
#' # }
#' # boldmat<-timeseries2matrix(bold,boldmask)
#' # boldmat<-boldmat[timeselect,]
#' # aalimg[aalimg > 0 ]<-1
#' # dmnvec<-rowMeans(timeseries2matrix(bold,aalimg))[timeselect]
#' # dmnvec<-(stl(ts(dmnvec, frequency = 4),"per")$time.series)[,2]
#' # dmnvec2<-(stl(ts(dmnvec, frequency = 100),"per")$time.series)[,2]
#' # dmnvec<-ts(as.numeric(dmnvec)-as.numeric(dmnvec2))
#' # dmnmat<-matrix( dmnvec, ncol=1)
#' # dnz<-rsfDenoise( boldmat , dmnmat[,1], motionparams=moco, polydegree=4,
#' #                 crossvalidationgroups = 8, maxnoisepreds=1:4, debug=F )
#' # print(paste("Best number of noise regressors",dnz$n))
#' # # now recompute the matrix using the full mask
#' # boldmask<-antsImageRead("meanboldmask.nii.gz")
#' # boldmat<-timeseries2matrix(bold,boldmask)
#' # boldmat<-boldmat[timeselect,]
#' # mdl<-bigLMStats( lm( boldmat ~ dmnmat[,1] + dnz$polys + dnz$noiseu ), 0.001 )
#' # betas<-mdl$beta.t[1,]
#' }
#'
#' @export rsfDenoise
rsfDenoise <- function(boldmatrix,
  targety, motionparams = NA, selectionthresh = 0.1,
  maxnoisepreds = 1:12, debug = FALSE,
  polydegree = 4, crossvalidationgroups = 4,
  tr = 1, scalemat = F, noisepoolfun = max) {
  nvox <- ncol(boldmatrix)
  groups <- crossvalidationgroups
  if (length(groups) == 1) {
    kfolds <- groups
    groups <- c()
    grouplength <- round(nrow(boldmatrix)/kfolds) - 1
    for (k in 1:kfolds) groups <- c(groups, rep(k, grouplength))
    groups <- c(rep(1, nrow(boldmatrix) - length(groups)), groups)
  }


  getnoisepool <- function(x, frac = selectionthresh) {
    xord <- sort(x)
    l <- round(length(x) * frac)
    val <- xord[l]
    return(x < val & x < 0)
  }

  crossvalidatedR2 <- function(residmatIn, targety, groups, howmuchnoise = 0, noiseu = NA,
    p = NA) {
    residmat <- residmatIn
    nvox <- ncol(residmat)
    kfo <- unique(groups)
    R2 <- matrix(rep(0, nvox * length(kfo)), nrow = length(kfo))
    for (k in kfo) {
      selector <- groups != k
      mydf <- data.frame(targety[selector])
      if (!all(is.na(p)))
        mydf <- data.frame(mydf, p[selector, ])
      mylm1 <- lm(residmat[selector, ] ~ ., data = mydf)
      selector <- groups == k
      mydf <- data.frame(targety[selector])
      if (!all(is.na(p)))
        mydf <- data.frame(mydf, p[selector, ])
      predmat <- predict(mylm1, newdata = mydf)
      realmat <- residmat[selector, ]
      for (v in 1:nvox) R2[k, v] <- 100 * (1 - sum((predmat[, v] - realmat[,
        v])^2)/sum((mean(realmat[, v]) - realmat[, v])^2))
    }
    return(R2)
  }

  ################################################# overall description of the method 1. regressors include: design + trends +
  ################################################# noise-pool 2. find noise-pool by initial cross-validation without noise
  ################################################# regressors 3. cross-validate predictions using different numbers of noise
  ################################################# regressors 4. select best n for predictors from noise pool 5. return the noise
  ################################################# mask and the value for n make regressors
  timevals <- NA
  if (all(is.na(timevals)))
    timevals <- 1:nrow(boldmatrix)
  p <- stats::poly(timevals, degree = polydegree)
  if (!all(is.na(motionparams)))
    p <- cbind(data.matrix(motionparams), p)
  rawboldmat <- data.matrix(boldmatrix)
  svdboldmat <- residuals(lm(rawboldmat ~ 0 + p))
  if (debug)
    print("lm")
  ################### now redo some work w/new hrf
  R2base <- crossvalidatedR2(svdboldmat, targety, groups, p = NA)
  R2base <- apply(R2base, FUN = noisepoolfun, MARGIN = 2)
  noisepool <- getnoisepool(R2base)
  if (all(noisepool == TRUE)) {
    print("all voxels meet your pvalthresh - try increasing the value")
    return(NA)
  }
  if (all(noisepool == FALSE)) {
    print("zero voxels meet your pvalthresh - try decreasing the value")
    return(NA)
  } else print(paste("Noise pool has nvoxels=", sum(noisepool)))
  if (scalemat)
    svdboldmat <- scale(svdboldmat)
  ##### should the denoising be done per group / run ?
  noiseu <- svd(svdboldmat[, noisepool], nv = 0, nu = max(maxnoisepreds))$u
  R2summary <- rep(0, length(maxnoisepreds))
  ct <- 1
  for (i in maxnoisepreds) {
    svdboldmat <- residuals(lm(rawboldmat ~ 0 + p + noiseu[, 1:i]))
    R2 <- crossvalidatedR2(svdboldmat, targety, groups, noiseu = NA, howmuchnoise = i,
      p = NA)
    R2max <- apply(R2, FUN = max, MARGIN = 2)
    if (ct == 1)
      R2perNoiseLevel <- R2max else R2perNoiseLevel <- cbind(R2perNoiseLevel, R2max)
    R2pos <- R2max[R2max > 0]
    R2summary[ct] <- median(R2pos)
    print(paste("NoiseU:", i, "MeanRSqrd", R2summary[ct]))
    ct <- ct + 1
  }
  scl <- 0.95
  if (max(R2summary) < 0)
    scl <- 1.05
  bestn <- maxnoisepreds[which(R2summary > scl * max(R2summary))[1]]
  return(list(n = bestn, R2atBestN = R2summary[bestn], noisepool = noisepool, R2base = R2base,
    R2final = R2perNoiseLevel, noiseu = noiseu, polys = p))
}
