\name{abpN4}
\alias{abpN4}

\title{
N4 with previously set parameters based on ants brain processing strategies. 
}

\description{
Truncate outlier intensities and bias correct with N4
}

\usage{
abpN4( img = inimg , intensityTruncation=c( 0.025, 0.975, 256 )  , mask = NA , usen3 = FALSE )
}

\arguments{
\item{image}{
 image to be bias corrected
}

\item{intensityTruncation}{
Params to TruncateImageIntensity in ImageMath 
}

\item{mask}{
optional antsImage mask
}

\item{usen3}{
Use N3 instead of N4
}

}

\value{
outputs a bias corrected image.
1 -- Failure
}

\author{
Tustison N, Avants BB
}

\examples{
\dontrun{
fn<-getANTsRData('r16') 
img<-antsImageRead(fn,2)
img2<-abpN4( img )
}
}
