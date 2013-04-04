\name{mni2tal}
\alias{mni2tal}

\title{Brett's mni2tal}

\description{
  mni2tal for converting from ch2/mni space to tal - very approximate.  

  see http://bioimagesuite.yale.edu/mni2tal/501_95733_More%20Accurate%20Talairach%20Coordinates%20SLIDES.pdf

  http://imaging.mrc-cbu.cam.ac.uk/imaging/MniTalairach
}

\usage{
 talCoord<-mni2tal( x = c( 10,12,14) ) 
}

\arguments{
  \item{x}{
   Point in mni152 space.
  }
}
\details{
  This is a standard approach but it's not very accurate.
}


\value{
  The output point is in approximate Talairach space.
}

\author{
  Matthew Brett, adapted for ANTsR by B Avants
}

\examples{
\dontrun{
  mni2tal( c(10,12,14) )
}
}
\keyword{Talairach}
