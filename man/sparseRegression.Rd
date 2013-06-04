\name{sparseRegression}
\alias{sparseRegression}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sparse regression on input images.
}
\description{
Compute a sparse, spatially coherent regression from a set of input images (with mask) to an outcome variable.
}
\usage{
sparseRegression(inmatrix, demog, outcome, mask=NA, sparseness=0.05, nvecs=50, 
    its=5, cthresh=250, statdir=NA, z=0, smooth=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inmatrix}{Input data matrix, with dimension number of subjects by number of voxels.}
  \item{demog}{Input demographics data frame.  Contains outcome variable to regress against.}
  \item{outcome}{Name of column in \code{demog} to regress against.}
  \item{mask}{Mask for reconstructing \code{inmatrix} in physical space.}
  \item{sparseness}{Level of sparsity desired.  \code{0.05}, for example, makes 5\% of the matrix be non-zero.}
  \item{nvecs}{Number of eigenvectors to return.}
  \item{its}{Number of cross-validation folds to run.}
  \item{cthresh}{Cluster threshold.}
  \item{statdir}{Where to put results.  If not provided, a temp directory is created.}
  \item{z}{Row (subject-wise) sparseness.}
  \item{smooth}{Amount of smoothing.}
}
\value{
  A list of values: 
  \item{eigenanatomyimages}{Coefficient vector images.}
  \item{umatrix}{Projections of input images on the sparse regression vectors.  Can be used for, e.g., subsequent classification/predictions.}
  \item{projections}{Predicted values of outcome variable.}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Kandel B.M., D. Wolk, J. Gee, and B. Avants. Predicting Cognitive Data from Medical Images Using Sparse Linear Regression. Information Processing in
Medical Imaging, 2013. 
%% ~put references to the literature/web site here ~
}
\author{
Kandel BM, Avants BB. 
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
require(ANTsR)
nsubj <- 100
input <- t(replicate(nsubj, rnorm(125)))
outcome <- 1:nsubj
demog <- data.frame(outcome=outcome)
input[, 40:60] <- 30 + outcome + rnorm(length(input[, 40:60]))
mymask <- as.antsImage(array(rep(1, 125), dim=c(5,5,5)))
myregression <- sparseRegression(input, demog, "outcome", mymask, 
    sparseness=0.05, nvecs=5, its=3)

# visualization of results
sample <- rep(0, 125)
sample[40:60] <-1 
signal.img <- as.antsImage(array(rep(0,125), dim=c(5, 5, 5)))
signal.img[signal.img >= 0 ] <- sample
plotANTsImage( signal.img, axis=2, slices="1x5x1")
readline("Visual representation of signal source; press any key to continue.")
for ( i in 1:5) {
  plotANTsImage(myregression$eigenanatomyimages[[i]], axis=2, slices="1x5x1")
  readline(paste("Eigenvector ", i, ". Press any key to continue.", sep=''))
}

}
