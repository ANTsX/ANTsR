% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiResRandomForestRegression.R
\name{multiResRandomForestRegression}
\alias{multiResRandomForestRegression}
\title{multiple resolution neighborhood random forest regression}
\usage{
multiResRandomForestRegression(
  y,
  x,
  labelmasks,
  rad = NA,
  nsamples = 10,
  multiResSchedule = c(0),
  ntrees = 500
)
}
\arguments{
\item{y}{vector of scalar values or labels.  if a factor, do classification,
otherwise regression.}

\item{x}{a list of lists where each list contains feature images}

\item{labelmasks}{a list of masks where each mask defines the image space
for the given list and the number of parallel predictors.  more labels means
more predictors.  alternatively, separate masks may be used for each feature
in which case this should be a list of lists.  see examples.}

\item{rad}{vector of dimensionality d define nhood radius}

\item{nsamples}{(per subject to enter training)}

\item{multiResSchedule}{an integer vector defining multi-res levels}

\item{ntrees}{(for the random forest model)}
}
\value{
list with a random forest model, a vector identifying which rows
correspond to which subjects and a prediction vector.
}
\description{
Represents feature images as a neighborhood across scales
to build a random forest prediction from an image population. A use case
for this function is to predict cognition from multiple image features, e.g.
from the voxelwise FA of the corpus callosum and, in parallel, voxelwise
measurements of the volume of the inferior frontal gyrus.
}
\examples{

mask <- makeImage(c(100, 100), 0)
mask[30:60, 30:60] <- 1
mask[35:45, 50:60] <- 2
ilist <- list()
masklist <- list()
inds <- 1:8
yvec <- rep(0, length(inds))
scl <- 0.33 # a noise parameter
for (i in inds) {
  img <- antsImageClone(mask)
  imgb <- antsImageClone(mask)
  limg <- antsImageClone(mask)
  img[3:6, 3:6] <- rnorm(16, 1) * scl * (i) + scl * mean(rnorm(1))
  imgb[3:6, 3:6] <- rnorm(16, 1) * scl * (i) + scl * mean(rnorm(1))
  ilist[[i]] <- list(img, imgb) # two features
  yvec[i] <- i^2.0 # a real outcome
  masklist[[i]] <- antsImageClone(mask)
}
r <- c(1, 1)
mr <- c(2, 0)
featMat <- getMultiResFeatureMatrix(ilist[[1]], masklist[[1]],
  rad = r, , multiResSchedule = mr
)
rfm <- multiResRandomForestRegression(
  yvec, ilist, masklist,
  rad = r, multiResSchedule = mr
)
preds <- predict(rfm, newdata = featMat)
\dontrun{
# data: https://github.com/stnava/ANTsTutorial/tree/master/phantomData
fns <- Sys.glob("phantom*wmgm.jpg")
ilist <- imageFileNames2ImageList(fns)
masklist <- list()
flist <- list()
for (i in 1:length(fns))
{
  # 2 labels means 2 sets of side by side predictors and features at each scale
  locseg <- kmeansSegmentation(ilist[[i]], 2)$segmentation
  masklist[[i]] <- list(locseg, locseg \%>\% thresholdImage(2, 2), locseg)
  flist[[i]] <- list(
    ilist[[i]], ilist[[i]] \%>\% iMath("Laplacian", 1),
    ilist[[i]] \%>\% iMath("Grad", 1)
  )
}
yvec <- factor(rep(c(1, 2), each = 4)) # classification
r <- c(1, 1)
mr <- c(2, 1, 0)
ns <- 50
trn <- c(1:3, 6:8)
ytrain <- yvec[trn]
ftrain <- flist[trn]
mtrain <- masklist[trn]
mrrfr <- multiResRandomForestRegression(ytrain, ftrain, mtrain,
  rad = c(1, 1),
  nsamples = ns, multiResSchedule = mr
)
mypreds <- rep(NA, length(fns))
mymode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
for (i in 4:5) # test set
{
  fmat <- getMultiResFeatureMatrix(flist[[i]], masklist[[i]],
    rad = r, multiResSchedule = mr, nsamples = ns
  )
  myp <- predict(mrrfr, newdata = fmat)
  mypreds[i] <- mymode(myp) # get the most frequent observation
  # use median or mean for continuous predictions
}
print("predicted")
print(mypreds[-trn])
print("ground truth")
print(yvec[-trn])
}
}
\references{
Pustina, D, et al.  Automated segmentation of chronic stroke
lesions using LINDA: Lesion Identification with Neighborhood Data Analysis,
Human Brain Mapping, 2016. (related work, not identical)
}
\seealso{
\code{\link{getMultiResFeatureMatrix}} \code{\link{mrvnrfs}}
}
\author{
Avants BB, Tustison NJ
}
