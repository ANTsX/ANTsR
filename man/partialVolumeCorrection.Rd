\name{partialVolumeCorrection}
\alias{partialVolumeCorrection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Perform partial volume correction for ASL images. 
}
\description{
This function performs a partial volume correction for ASL images by dividing the observed CBF by the gray matter and white matter probabilities, as described in Johnson et al., Radiology 2005: 
CBF_corrected = CBF_observed / (GM_prob + 0.4 * WM_prob)
}
\usage{
partialVolumeCorrection(image, image.gm, image.wm, mask=NULL, proportion=0.4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{image}{Low-resolution image to be corrected.}
  \item{image.gm}{Gray matter probability image for partial volume correction.}
  \item{image.wm}{White matter probability image for partial volume correction.}
  \item{mask}{Brain mask for image.}
  \item{proportion}{Ratio of activity for white matter to gray matter.  Assumed to be 0.4.}
%%     ~~Describe \code{x} here~~

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Returns partial volume corrected antsImage.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Method: Pattern of cerebral hypoperfusion in Alzheimer disease and mild cognitive impairment measured with arterial spin-labeling MR imaging: initial experience.  Johnson NA, Jahng GH, Weiner MW, Miller BL, Chui HC, Jagust WJ, Gorno-Tempini ML, Schuff N. Radiology 2005. 

Ratio of GM to WM activity: Quantitative magnetic resonance imaging of human brain perfusion at 1.5 T using steady-state inversion of arterial water. Roberts DA, Detre JA, Bolinger L, Insko EK, Leigh JS Jr. PNAS 1994. 

%% ~put references to the literature/web site here ~
}
\author{
Kandel BM
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
activity.gm <- 10
activity.wm <- activity.gm * 0.4
percent.gm <- matrix(seq(0.1, 1, by=0.1), nrow=2)
percent.wm <- -percent.gm + 1
activity.obs <- percent.gm * rnorm(n=length(percent.gm), mean=activity.gm, sd=5) + 
                  rnorm(n=length(percent.wm), mean=activity.wm, sd=5)
img <- as.antsImage(as.array(activity.obs))
img.gm <- as.antsImage(as.array(percent.gm))
img.wm <- as.antsImage(as.array(percent.wm))
activity.corrected <- partialVolumeCorrection(img, img.gm, img.wm)
print(as.array(activity.corrected))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
