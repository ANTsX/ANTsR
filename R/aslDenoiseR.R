#' WIP: data-driven denoising for ASL MRI
#' 
#' Denoises regression based reconstruction of CBF from arterial spin labeling
#' 
#' 
#' @param mat input ASL matrix
#' @return matrix is output
#' @author Avants BB
#' @examples
#' 
#' \dontrun{
#' fn<-"PEDS012_20131101_pcasl_1.nii.gz"
#' asl<-antsImageRead(fn,4)
#' # image available at http://files.figshare.com/1701182/PEDS012_20131101.zip
#' tr<-antsGetSpacing(asl)[4]
#' aslmean<-getAverageOfTimeSeries( asl )
#' aslmask<-getMask(aslmean,lowThresh=mean(aslmean)*0.5,cleanup=TRUE)
#' aslmat<-timeseries2matrix(asl,aslmask)
#' tc<-as.factor(rep(c("C","T"),nrow(aslmat)/2))
#' dv<-computeDVARS(aslmat)
#' clustasl<-clusterTimeSeries( aslmat, 4 )
#' dnz<-aslDenoiseR( aslmat, tc, motionparams=dv, selectionthresh=0.1,
#'   maxnoisepreds=c(1:16), debug=FALSE, polydegree=4,
#'   crossvalidationgroups=sample(clustasl$clusters) )
#' nzimg<-antsImageClone(aslmask)
#' nzimg[ aslmask == 1 ]<-dnz$R2final
#' antsImageWrite(nzimg,'nzimg.nii.gz')
#' # a classic regression approach to estimating perfusion
#' # not recommended, but shows the basic idea.
#' # see ?quantifyCBF for a better approach
#' perfmodel<-lm( aslmat ~ tc + dnz$noiseu  )
#' perfimg<-antsImageClone(aslmask)
#' perfimg[ aslmask == 1 ]<-bigLMStats( perfmodel )$beta[1,]
#' antsImageWrite(perfimg,'perf.nii.gz')
#' m0<-getAverageOfTimeSeries(asl)
#' ctl<-c(1:(nrow(aslmat)/2))*2
#' m0[ aslmask==1 ]<-colMeans(aslmat[ctl,])
#' pcasl.parameters<-list( sequence="pcasl", m0=m0 )
#' cbf <- quantifyCBF( perfimg, aslmask, pcasl.parameters )
#' antsImageWrite(perfimg,'cbf.nii.gz')
#' }
#' 
#' @export aslDenoiseR
aslDenoiseR <- function(boldmatrix, targety, motionparams = NA, selectionthresh = 0.1,
  maxnoisepreds = 1:12, debug = FALSE, polydegree = 4, crossvalidationgroups = 4,
  scalemat = F, noisepoolfun = max, usecompcor=F ) {
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
      for (v in 1:nvox)
        {
        sum1<-sum((predmat[, v] - realmat[,v])^2,na.rm=T)
        sum2<-sum((mean(realmat[, v],na.rm=T)- realmat[, v])^2, na.rm=T)
        R2[k, v] <- 100 * (1 - sum1/sum2 )
        }
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
  if (usecompcor) noiseu<-compcor(  svdboldmat , max(maxnoisepreds) )
  if ( ! usecompcor )
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
  if (max(R2summary, na.rm=T) < 0)
    scl <- 1.05
  mxt<-scl * max(R2summary , na.rm=T )
  bestn <- maxnoisepreds[which(R2summary > mxt )[1]]
  if ( ct == 2 ) R2final<-R2perNoiseLevel
  if ( ct > 2 ) R2final<-R2perNoiseLevel[,bestn-min(maxnoisepreds)+1]
  return(list(n = bestn, R2atBestN = R2summary[bestn],
    noisepool = noisepool, R2base = R2base,
    R2final = R2final, noiseu = noiseu[, 1:bestn], polys = p)
    )
}
