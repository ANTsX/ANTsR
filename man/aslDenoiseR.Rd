% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aslDenoiseR.R
\name{aslDenoiseR}
\alias{aslDenoiseR}
\title{WIP: data-driven denoising for ASL MRI}
\usage{
aslDenoiseR(
  boldmatrix,
  targety,
  covariates = NA,
  selectionthresh = 0.1,
  maxnoisepreds = 2:12,
  polydegree = "loess",
  crossvalidationgroups = 4,
  scalemat = F,
  noisepoolfun = max,
  usecompcor = F,
  verbose = F
)
}
\arguments{
\item{boldmatrix}{input bold matrix}

\item{targety}{target to predict}

\item{covariates}{motion or other parameters / nuisance variables}

\item{selectionthresh}{e.g. 0.1 take 10 percent worst variables for noise
estimation}

\item{maxnoisepreds}{integer search range e.g 1:10}

\item{polydegree}{eg 4 for polynomial nuisance variables or 'loess'}

\item{crossvalidationgroups}{prior defined or integer valued}

\item{scalemat}{boolean}

\item{noisepoolfun}{function to help select noise pool e.g. max}

\item{usecompcor}{boolean}

\item{verbose}{boolean}
}
\value{
matrix is output
}
\description{
Denoises regression based reconstruction of CBF from arterial spin labeling
}
\examples{

# asl<-antsImageRead( getANTsRData("pcasl") )
set.seed(1)
nvox <- 10 * 10 * 10 * 20
dims <- c(10, 10, 10, 20)
asl <- makeImage(dims, rnorm(nvox) + 500)
aslmean <- getAverageOfTimeSeries(asl)
aslmask <- getMask(aslmean)
aslmat <- timeseries2matrix(asl, aslmask)
for (i in 1:10) aslmat[, i * 2] <- aslmat[, i * 2] * 2
asl <- matrix2timeseries(asl, aslmask, aslmat)
tc <- as.factor(rep(c("C", "T"), nrow(aslmat) / 2))
dv <- computeDVARS(aslmat)
dnz <- aslDenoiseR(aslmat, tc,
  covariates = dv, selectionthresh = 0.1,
  maxnoisepreds = c(1:2), polydegree = 2, crossvalidationgroups = 2
)
testthat::expect_equal(dnz$R2atBestN, 7, tolerance = 0.5)

\dontrun{
# a classic regression approach to estimating perfusion
# not recommended, but shows the basic idea.
# see ?quantifyCBF for a better approach
perfmodel <- lm(aslmat ~ tc + dnz$noiseu)
perfimg <- antsImageClone(aslmask)
perfimg[aslmask == 1] <- bigLMStats(perfmodel)$beta[1, ]
m0 <- getAverageOfTimeSeries(asl)
ctl <- c(1:(nrow(aslmat) / 2)) * 2
m0[aslmask == 1] <- colMeans(aslmat[ctl, ])
pcasl.parameters <- list(sequence = "pcasl", m0 = m0)
cbf <- quantifyCBF(perfimg, aslmask, pcasl.parameters)

# default mode network example

if (!exists("bold")) {
  bold <- antsImageRead(getANTsRData("rsbold"))
  meanbold <- getAverageOfTimeSeries(bold)
  boldmask <- getMask(meanbold)
  # map to mni
  mni <- antsImageRead(getANTsRData("mni"))
  mniaal <- antsImageRead(getANTsRData("mnia"))
  mymap <- antsRegistration(meanbold * boldmask, mni,
    typeofTransform = "SyNBold",
    verbose = 1
  )
  aalimg <- antsApplyTransforms(meanbold, mniaal, mymap$fwdtransforms,
    interpolator = "NearestNeighbor"
  )
  data("aal", package = "ANTsR")
  timeselect <- 10:dim(bold)[4]
  if (!exists("moco")) {
    moco <- antsMotionCalculation(bold, boldmask)
  }
  sbold <- smoothImage(moco$moco_img, 3.0)
  antsImageWrite(boldmask, "boldmask.nii.gz")
  antsImageWrite(meanbold, "boldmean.nii.gz")
  antsImageWrite(aalimg, "boldaal.nii.gz")
  boldmask <- boldmask * thresholdImage(aalimg, 1, Inf)
}
postcing <- aal$label_num[grep("Cingulum_Post", aal$label_name)]
postCingMask <- maskImage(boldmask, aalimg,
  level = as.numeric(postcing), binarize = T
)
mpostCingMask <- antsImageClone(postCingMask) * 0
mpostCingMask[postCingMask == 0] <- 1
boldmat <- timeseries2matrix(sbold, boldmask * mpostCingMask)
boldmat <- boldmat[timeselect, ]
boldmat <- frequencyFilterfMRI(boldmat, tr = antsGetSpacing(bold)[4], opt = "trig")
dmnvec <- (timeseries2matrix(sbold, postCingMask)[timeselect, ])
dmnvec <- rowMeans(
  frequencyFilterfMRI(dmnvec, tr = antsGetSpacing(bold)[4], opt = "trig")
)
dmnmat <- matrix(dmnvec, ncol = 1)
mocpar <- moco$moco_params[timeselect, 3:14]
dnz <- aslDenoiseR(boldmat, dmnvec,
  covariates = mocpar, selectionthresh = 0.2,
  maxnoisepreds = c(2:10), polydegree = "loess",
  crossvalidationgroups = 8
)
boldmat <- timeseries2matrix(sbold, boldmask)
boldmat <- boldmat[timeselect, ]
boldmat <- frequencyFilterfMRI(boldmat, tr = antsGetSpacing(bold)[4], opt = "trig")
mdl <- bigLMStats(lm(boldmat ~ dmnvec + dnz$covariates + dnz$noiseu), 0.001)
betas <- mdl$beta.t[1, ]
betaImg <- makeImage(boldmask, betas)
antsImageWrite(betaImg, "dmnBetas.nii.gz")
# this should give default mode network around beta = 12
}

}
\author{
Avants BB
}
