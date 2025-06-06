% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aslCensoring.R
\name{aslCensoring}
\alias{aslCensoring}
\title{Censor bad volumes from ASL data.}
\usage{
aslCensoring(
  asl,
  mask = NULL,
  nuis = NA,
  method = "outlier",
  reject.pairs = FALSE,
  ...
)
}
\arguments{
\item{asl}{input asl image}

\item{mask}{mask for calculating perfusion}

\item{nuis}{fixed nuisance parameters}

\item{method}{one of 'outlier', 'robust', or 'scor'.  See \code{Details}.}

\item{reject.pairs}{whether to reject only tag-control pairs of images,
as opposed to single images.  Rejecting pairs of images is necessary for
non-regression-based ASL averaging methods.}

\item{...}{Additional arguments to pass to censoring method.  See \code{Details.}}
}
\value{
vector of the same length as number of timepoints in \code{asl}, with
 1 indicating the corresponding timepoint is included and 0 indicating exclusion.
}
\description{
Censor bad volumes from ASL data.
}
\details{
\code{aslCensoring} is an interface to ASL timepoint censoring algorithms.
Three options are currently provided, with different additional arguments:
\describe{
 \item{\code{outlier}}{ Outlier rejection from Tan et al.  This method rejects
   volumes that are either far from the mean of the time-series or whose
   standard deviation is far from the standard deviations of the individual volumes.
   Accepts two additional arguments:
     \describe{
        \item{\code{sigma.mean}: }{how many standard
        deviations the mean of the volume can be from the
        mean of all the volumes before
        being thrown out.}
        \item{\code{sigma.sd}: }{how many standard deviations from the
        mean of standard deviations can the standard deviation of the volume be
        before being thrown out.}
     }
}
 \item{\code{robust}}{ Uses a robust regression approach to estimate volumes
   with high leverage.  Accepts three arguments:
   \describe{
     \item{\code{nuis}:}{ Nuisance regressors to use as covariates.}
     \item{\code{robthresh}:}{ Threshold for weights on leverage estimates.  Points
        with weights under this value will be thrown out; defaults to 0.95.}
     \item{\code{skip}:}{ Proportion of points to skip when estimating leverage.
       Defaults to 20 (1/20 of the image is used).}
    }
  }
  \item{\code{scor}}{ SCOR method of Dolui et al.  No parameters.}
}
}
\examples{
set.seed(1)
nvox <- 5 * 5 * 5 * 30
dims <- c(5, 5, 5, 30)
voxvals <- array(rnorm(nvox) + 500, dim = dims)
voxvals[, , , 5] <- voxvals[, , , 5] + 600
asl <- makeImage(dims, voxvals)
censored <- aslCensoring(asl)

}
\references{
Tan H. et al., ``A Fast, Effective Filtering Method
for Improving Clinical Pulsed Arterial Spin Labeling MRI,'' JMRI 2009.
}
\author{
Kandel BM
}
