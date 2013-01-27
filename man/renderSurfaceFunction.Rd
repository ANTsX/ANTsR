\name{renderSurfaceFunction}
\alias{renderSurfaceFunction}

\title{
3D surface-based rendering of volume images.
}

\description{
Will use rgl to render a substrate (e.g. anatomical) and overlay image (e.g. functional).  
}

\usage{
renderSurfaceFunction<-function( surfimg, funcimg, surfval=0.5, basefval , offsetfval , smoothsval = 0, smoothfval = 0, blobrender = TRUE )
}

\arguments{
\item{surfimg}{
Input image to use as rendering substrate.
}

#  surfimg, funcimg, surfval=0.5, basefval , offsetfval , smoothsval = 0, smoothfval = 0, blobrender = TRUE
\item{funcimg}{
Input image to use as functional overlay.
}

\item{surfval}{
intensity level that defines isosurface 
}

\item{basefval}{
intensity level that defines lower threshold for functional image 
}


\item{offsetfval}{
intensity level that defines upper threshold for functional image 
}

\item{blobrender}{
  render a blob as opposed to a surface patch
}

}

\value{
0 -- Success\cr
1 -- Failure
}

\author{
Avants B, Kandel B
}

\examples{
\dontrun{
renderSurfaceFunction(simg)
renderSurfaceFunction(simg,fimg,basefval=0.2,blobrender=FALSE)
}
}
