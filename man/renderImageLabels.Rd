\name{renderImageLabels}
\alias{renderImageLabels}

\title{
3D surface-based rendering of image segmentation labels
}

\description{
Will use rgl to render surfaces
}

\usage{
renderImageLabels<-function( labelsimg, surfval=0.5, smoothsval = 0, alphasurf=1, physical=TRUE, color=c() )
}

\arguments{
\item{labelsimg}{
3D images of integer labels
}

\item{surfval}{
intensity level that defines isosurface 
}

\time{smoothsval}{
sigma for smoothing of each extracted label image
}

\item{alphasurf}{
opacity of each rendered surface
}

\item{physical}{
flag to use true spatial coordinates
}

\item{color}{
colors to use for each label
}

}

\value{
0 -- Success\cr
1 -- Failure
}

\author{
Duda, J
}

\examples{
\dontrun{
renderImageLabels(labels)
renderImageLabels(labels, smoothsval=0.5, alphasurf=0.3, color=snapColors(100) )
}
}
