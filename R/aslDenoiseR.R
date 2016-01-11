#' WIP: data-driven denoising for ASL MRI
#'
#' Denoises regression based reconstruction of CBF from arterial spin labeling
#'
#' @param boldmatrix input bold matrix
#' @param targety target to predict
#' @param covariates motion or other parameters / nuisance variables
#' @param selectionthresh e.g. 0.1 take 10 percent worst variables for noise
#' estimation
#' @param maxnoisepreds integer search range e.g 1:10
#' @param debug boolean
#' @param polydegree eg 4 for polynomial nuisance variables or 'loess'
#' @param crossvalidationgroups prior defined or integer valued
#' @param scalemat boolean
#' @param noisepoolfun function to help select noise pool e.g. max
#' @param usecompcor boolean
#' @return matrix is output
#' @author Avants BB
#' @examples
#'
#' # asl<-antsImageRead( getANTsRData("pcasl") )
#' set.seed(1)
#' nvox <- 10*10*10*20
#' dims <- c(10,10,10,20)
#' asl <- makeImage( dims , rnorm( nvox )+500 )
#' aslmean <- getAverageOfTimeSeries( asl )
#' aslmask <- getMask( aslmean  )
#' aslmat<-timeseries2matrix( asl, aslmask )
#' for ( i in 1:10 ) aslmat[,i*2]<-aslmat[,i*2]*2
#' asl<-matrix2timeseries( asl, aslmask, aslmat )
#' tc<-as.factor(rep(c("C","T"),nrow(aslmat)/2))
#' dv<-computeDVARS(aslmat)
#' dnz<-aslDenoiseR( aslmat, tc, covariates=dv, selectionthresh=0.1,
#'   maxnoisepreds=c(1:2), debug=TRUE, polydegree=2, crossvalidationgroups=2 )
#' \dontrun{
#' # a classic regression approach to estimating perfusion
#' # not recommended, but shows the basic idea.
#' # see ?quantifyCBF for a better approach
#' perfmodel<-lm( aslmat ~ tc + dnz$noiseu  )
#' perfimg<-antsImageClone(aslmask)
#' perfimg[ aslmask == 1 ]<-bigLMStats( perfmodel )$beta[1,]
#' m0<-getAverageOfTimeSeries(asl)
#' ctl<-c(1:(nrow(aslmat)/2))*2
#' m0[ aslmask==1 ]<-colMeans(aslmat[ctl,])
#' pcasl.parameters<-list( sequence="pcasl", m0=m0 )
#' cbf <- quantifyCBF( perfimg, aslmask, pcasl.parameters )
#' }
#'
#' @export aslDenoiseR
aslDenoiseR <- function(
  boldmatrix,
  targety,
  covariates = NA,
  selectionthresh = 0.1,
  maxnoisepreds = 1:12,
  debug = FALSE,
  polydegree = 4,
  crossvalidationgroups = 4,
  scalemat = F,
  noisepoolfun = max,
  usecompcor = F) {
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

  ################################################# overall description of the method 1. regressors include: design + trends +
  ################################################# noise-pool 2. find noise-pool by initial cross-validation without noise
  ################################################# regressors 3. cross-validate predictions using different numbers of noise
  ################################################# regressors 4. select best n for predictors from noise pool 5. return the noise
  ################################################# mask and the value for n make regressors
  timevals <- NA
  if (all(is.na(timevals)))
    timevals <- 1:nrow(boldmatrix)
  #
  if (is.numeric(polydegree)) {
    if (polydegree > 0) {
      p <- stats::poly(timevals, degree = polydegree)
      aslmat <- residuals(lm(boldmatrix ~ 0 + p))
      if (!all(is.na(covariates))) {
        covariates <- cbind(data.matrix(covariates), p)
      } else covariates <- p
    }
  } else if (polydegree == 'loess') {
    timevals <- 1:nrow(boldmatrix)
    mean.ts <- apply(boldmatrix, 1, mean)
    myloess <- loess(mean.ts ~ timevals)
    p <- myloess$fitted
    if (!all(is.na(covariates))) {
      covariates <- cbind(data.matrix(covariates), p)
    } else covariates <- p
  }
  rawboldmat <- data.matrix(boldmatrix)
  svdboldmat <- residuals(lm(rawboldmat ~ 0 + covariates))
  if (debug)
    print("lm")
  ################### now redo some work w/new hrf
  R2base <- crossvalidatedR2(svdboldmat, targety, groups,
    covariates = covariates)
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
  if (usecompcor)
    noiseu <- compcor(svdboldmat, max(maxnoisepreds))
  if (!usecompcor)
    noiseu <- svd(svdboldmat[, noisepool], nv = 0, nu = max(maxnoisepreds))$u
  R2summary <- rep(0, length(maxnoisepreds))
  ct <- 1
  for (i in maxnoisepreds) {
    svdboldmat <- residuals(lm(rawboldmat ~ 0 + covariates + noiseu[, 1:i]))
    R2 <- crossvalidatedR2(svdboldmat, targety, groups,
      covariates = covariates )
    R2max <- apply(R2, FUN = max, MARGIN = 2)
    if (ct == 1)
      R2perNoiseLevel <- R2max else R2perNoiseLevel <- cbind(R2perNoiseLevel, R2max)
    R2pos <- R2max[R2max > 0]
    R2summary[ct] <- median(R2pos)
    print(paste("NoiseU:", i, "MeanRSqrd", R2summary[ct]))
    ct <- ct + 1
  }
  scl <- 0.95
  if (max(R2summary, na.rm = T) < 0)
    scl <- 1.05
  mxt <- scl * max(R2summary, na.rm = T)
  bestn <- maxnoisepreds[which(R2summary > mxt)[1]]
  if (ct == 2)
    R2final <- R2perNoiseLevel
  if (ct > 2)
    R2final <- R2perNoiseLevel[, bestn - min(maxnoisepreds) + 1]
  return(list(n = bestn, R2atBestN = R2summary[bestn], noisepool = noisepool, R2base = R2base,
    R2final = R2final, noiseu = noiseu[, 1:bestn],
    covariates = covariates))
}
