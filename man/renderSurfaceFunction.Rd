\name{renderSurfaceFunction}
\alias{renderSurfaceFunction}

\title{
3D surface-based rendering of volume images.
}

\description{
Will use rgl to render a substrate (e.g. anatomical) and overlay image (e.g. functional).  
}

\usage{
renderSurfaceFunction<-function( surfimg, funcimg=list(overlayimage1,overlayimage2,etc), surfval=0.5, basefval , offsetfval , smoothsval = 0, smoothfval = 0, blobrender = TRUE )
}

\arguments{
\item{surfimg}{
Input image to use as rendering substrate.
}

#  surfimg, funcimg, surfval=0.5, basefval , offsetfval , smoothsval = 0, smoothfval = 0, blobrender = TRUE
\item{funcimg}{
Input list of images to use as functional overlays.
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

\seealso{
  \code{\link{plotBasicNetwork}}
} 

\examples{
       mnit<-getANTsRData("mni")
       mnit<-antsImageRead(mnit,3)
       mnia<-getANTsRData("mnia")
       mnia<-antsImageRead(mnia,3)
       ThresholdImage(3,mnit,mnit,1,max(mnit))
       ImageMath(3,mnit,"FillHoles",mnit)
       cnt<-getCentroids( mnia, clustparam = 50 )
       aalcnt<-cnt$centroids[1:90,]
       brain<-renderSurfaceFunction( surfimg =list( mnit ) , alphasurf=0.1 ,smoothsval = 1.5 )
       brain<-renderSurfaceFunction( surfimg =list( mnit ) , list(mnia), alphasurf=0.1 ,smoothsval = 1.5 )
}
