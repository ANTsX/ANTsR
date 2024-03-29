% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jointLabelFusion.R
\name{jointLabelFusion}
\alias{jointLabelFusion}
\title{joint label and intensity fusion}
\usage{
jointLabelFusion(
  targetI,
  targetIMask,
  atlasList,
  beta = 4,
  rad = 2,
  labelList = NULL,
  rho = 0.01,
  usecor = FALSE,
  rSearch = 3,
  nonnegative = FALSE,
  maxLabelPlusOne = FALSE,
  noZeroes = FALSE,
  verbose = FALSE
)
}
\arguments{
\item{targetI}{antsImage to be approximated}

\item{targetIMask}{mask with value 1}

\item{atlasList}{list containing antsImages with intensity images}

\item{beta}{weight sharpness, default to 2}

\item{rad}{neighborhood radius, default to 2}

\item{labelList}{optional list containing antsImages with segmentation labels}

\item{rho}{ridge penalty increases robustness to outliers but also
makes image converge to average}

\item{usecor}{employ correlation as local similarity}

\item{rSearch}{radius of search, default is 3}

\item{nonnegative}{constrain weights to be non-negative}

\item{maxLabelPlusOne}{boolean
this will add max label plus one to the non-zero parts of each label where the target mask
is greater than one.  NOTE: this will have a side effect of adding to the original label
images that are passed to the program.  It also guarantees that every position in the
labels have some label, rather than none.  Ie it guarantees to explicitly parcellate the
input data.}

\item{noZeroes}{boolean will zero out target mask regions that have any zero label.
this prevents JLF from computing a solution in regions not covered by the initial library.}

\item{verbose}{boolean}
}
\value{
approximated image, segmentation and probabilities
}
\description{
A multiple atlas voting scheme to customize labels for a new subject. This
function will also perform intensity fusion. It almost directly calls the
\code{C++} in the ANTs executable so is much faster than other variants in ANTsR.
One may want to normalize image intensities for each input image before
passing to this function.  If no labels are passed, we do intensity fusion.
Note on computation time: the underlying \code{C++}
is multithreaded.  You can control the number of threads by setting the
environment variable \code{ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS} e.g. to use all or
some of your CPUs.  This will improve performance substantially. For instance,
on a macbook pro from 2015, 8 cores improves speed by about 4x.
}
\examples{

set.seed(123)
ref <- ri(1)
ref <- resampleImage(ref, c(50, 50), 1, 0)
ref <- iMath(ref, "Normalize")
mi <- ri(2)
mi2 <- ri(3)
mi3 <- ri(4)
mi4 <- ri(5)
mi5 <- ri(6)
refmask <- getMask(ref)
refmask <- iMath(refmask, "ME", 2) # just to speed things up
ilist <- list(mi, mi2, mi3, mi4, mi5)
seglist <- list()
for (i in 1:length(ilist))
{
  ilist[[i]] <- iMath(ilist[[i]], "Normalize")
  mytx <- antsRegistration(
    fixed = ref, moving = ilist[[i]],
    typeofTransform = c("Affine"), verbose = TRUE
  )
  mywarpedimage <- antsApplyTransforms(
    fixed = ref,
    moving = ilist[[i]],
    transformlist = mytx$fwdtransforms
  )
  ilist[[i]] <- mywarpedimage
  seg <- thresholdImage(ilist[[i]], "Otsu", 3)
  seglist[[i]] <- seg
}
r <- 2
pp <- jointLabelFusion(ref, refmask, ilist,
  rSearch = 2,
  labelList = seglist, rad = rep(r, length(dim(ref)))
)
pp2 <- jointLabelFusion(ref, refmask, ilist,
  rSearch = 2,
  labelList = seglist, rad = rep(r, length(dim(ref)))
)
testthat::expect_equal(pp2$segmentation, pp$segmentation)
pp <- jointLabelFusion(ref, refmask, ilist,
  rSearch = 2,
  rad = rep(r, length(dim(ref)))
)

\dontrun{
ref <- antsImageRead(getANTsRData("ch2"))
n <- 50
ref <- resampleImage(ref, c(n, n, n), 1, 0)
ref <- iMath(ref, "Normalize")
refmask <- getMask(ref)
ilist <- list()
seglist <- list()
for (k in 1:5) {
  mi <- antsImageClone(ref) + rnorm(n^3, 0, 0.1)
  mykseg <- kmeansSegmentation(mi, 3, refmask)$segmentation
  ilist[[k]] <- mi
  seglist[[k]] <- mykseg
}
pp <- jointLabelFusion(ref, refmask, ilist,
  rSearch = 2,
  labelList = seglist, rad = rep(2, length(dim(ref))), verbose = TRUE
)
plot(ref, pp$segmentation)
plot(pp$intensity)
}

}
\author{
Brian B. Avants, Hongzhi Wang, Paul Yushkevich, Nicholas J. Tustison
}
\keyword{fusion,}
\keyword{template}
