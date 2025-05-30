% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statFieldThresh.R
\name{statFieldThresh}
\alias{statFieldThresh}
\title{Produces a threshold value based on cluster or voxel level statistics}
\usage{
statFieldThresh(
  x,
  pval,
  nvox,
  n,
  fwhm,
  resels,
  df,
  fieldType,
  threshType,
  pp = 0.001,
  verbose = FALSE
)
}
\arguments{
\item{x}{statistical map of class antsImage}

\item{pval}{p-value for determining threshold}

\item{nvox}{minimum desired cluster size (in voxels)}

\item{n}{number of images in conjunction}

\item{fwhm}{full width at half maxima}

\item{resels}{resolution elements in search region}

\item{df}{degrees of freedom expressed as df = c(degrees of interest, degrees of error)}

\item{fieldType}{\describe{
\item{T: }{T-field}
\item{F: }{F-field}
\item{X: }{Chi-square field'}
\item{Z: }{Gaussian field}
}}

\item{threshType}{\describe{
    \item{cRFT: }{computes a threshold per expected cluster level probability}
    \item{pRFT: }{uses the mask and pval calculates the minimum statistical threshold}
    \item{cFDR: }{uses an uncorrected threshold at the alpha level and then computes and FDR threshold based on cluster maxima}
    \item{pFDR: }{computes the fdr threshold for the entire field of voxels}
}}

\item{pp}{primary (initial) p-value threshold used in FDR methods}

\item{verbose}{enables verbose output}
}
\value{
Outputs a statistical value to be used for threshold a SPM
}
\description{
Produces a threshold value based on cluster or voxel level statistics
}
\details{
A statistical threshold level is determined using the estimated p-value \code{pval}
given the provided parameters. "cRFT" and "pRFT" specify the method of estimation
should use RFT cluster and peak statistic estimates respectively.

In addition to RFT based thresholds the user may choose to utilize false-discovery rate
(FDR) based thresholds. These use a primary p-value threshold (default \code{pp = .001})
to create suprathreshold clusters which in turn are used to determine a final threshold
(utilizing \code{pval} at this point). If the estimated peak-FDR statistic is used ("pFDR")
then all suprathreshold voxels are FDR corrected to determine the threshold. If the
estimated cluster-FDR statistic is used then the cluster maxima are FDR corrected in order
to determine the threshold.

It is important that the user recognize that when statistical analyses are computed
using RFT that the threshold level plays a role in predicting the p-value. This is
because the probability of obtaining results is weighted against the probability of
any occurence given the threshold. For example, if two clusters where exactly the
same but had one was obtained using a lower threshold it would have a lower p-value.
This has been shown with a power analysis using similar parameters to those presented
herein with the additional variable of signal characteristics to demonstrate the effect
of image modality on analysis (Friston et al., 1996). Therefore, parameters should be
chosen according to the type of analysis being performed (fMRI, PET, or VBM) and the
hypothesis being tested. Due to widely varying oppinions on appropriate thresholding
procedures no specific recommendations are made here. This function simply facilitates
the use of several approaches that users may utilize after consulting available literature.
}
\examples{
\dontrun{
# threshold according to estimated peak height
thresh_image <- statFieldThresh(timg, .05, 10, 1, mysmooth$fwhm,
  c(1, fit$df.residual),
  fieldType = "T",
  threshType = pRFT
)
}
}
\references{
Chumbley J., (2010) Topological FDR for neuroimaging

Friston K.J., (1994) Assessing the Significance of Focal Activations Using Their Spatial Extent

Friston K.J., (1996) Detecting Activations in PET and fMRI: Levels of Inference and Power
}
\author{
Zachary P. Christensen
}
