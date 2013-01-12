\name{plotANTsImage}
\alias{plotANTsImage}

\title{
Plotting an image slice or multi-slice with optional color overlay.
}

\description{
This is a plotting utility for antsImage types with a background and color overlay option.  Useful for displaying statistical results overlaid on a background image.
}

\usage{
plotANTsImage( myantsimage , functional=NA, color="red", axis=1, slices="1x1x1" , threshold="1x0", quality=NA )
}

\arguments{
\item{myantsimage}{the reference image on which to overlay }
\item{color = <string>}{the color for the overlay , e.g "blue"}
\item{functional}{ the image to use as overlay }
\item{axis  = <value>}{character ... the axis to slice (1 , 2 or  3)  }
\item{slices = <string>}{ character, the slices to overlay written as 10x20x3 where 10x20 is the range and 3 is the increment, for multislice display}
\item{threshold = <string>}{ character, we overlay values above/below this threshold : of form LOxHI }
\item{quality = <value>}{integer quality magnification factor 1 => large (e.g. 10) }
}

\value{
output is plot to standard R window

or

1 -- Failure
}

\author{
Avants BB
}

\examples{
\dontrun{
# in 2D
  plotANTsImage(myantsimage=img,functional=funcimg,threshold="50x150",color="red",axis=1)
# in 3D
  plotANTsImage(myantsimage=img,functional=img,threshold="50x150",slices="10x20x3",color="red",axis=0)
}
}
