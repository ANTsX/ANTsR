% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rfSegmentation.R
\name{rfSegmentationPredict}
\alias{rfSegmentationPredict}
\title{A rfSegmentationPredict function.}
\usage{
rfSegmentationPredict(
  rfSegmentationModel,
  featureMatrix,
  mask,
  verbose = FALSE
)
}
\arguments{
\item{rfSegmentationModel}{input rf model}

\item{featureMatrix}{input feature matrix}

\item{mask}{antsImage mask}

\item{verbose}{bool}
}
\value{
segmentation is output
}
\description{
Predict image segmentation via random forests.
}
\examples{

if (usePkg("randomForest")) {
  img <- antsImageRead(getANTsRData("r16"))
  mask <- getMask(img)
  mask2 <- getMask(img)
  mask[129:255, 1:255] <- 0
  mask2[2:128, 1:255] <- 0
  segs <- kmeansSegmentation(img, k = 3, kmask = mask)
  fmat <- t(antsrimpute(getNeighborhoodInMask(img, mask, c(2, 2))))
  rfsegs <- rfSegmentation(fmat, mask, segs$segmentation, ntrees = 100)
  fmat2 <- t(antsrimpute(getNeighborhoodInMask(img, mask2, c(2, 2))))
  rfseg2 <- rfSegmentationPredict(rfsegs$rfModel, fmat2, mask2)
}

}
\author{
Tustison NJ, Avants BB
}
