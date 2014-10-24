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
\item{color = <string>}{the color for the overlay , e.g c("blue","red") length of this list should match the image list}
\item{functional}{ list of the images to use as overlays }
\item{axis  = <value>}{character ... the axis to slice (1 , 2 or  3)  }
\item{slices = <string>}{ character, the slices to overlay written as 10x20x3 where 10x20 is the range and 3 is the increment, for multislice display}
\item{threshold = <string>}{ character, we overlay values above/below this threshold : of form LOxHI
}
\item{quality = <value>}{integer quality magnification factor 1 => large (e.g. 10) }
\item{outname="figx.jpg"}{output name if you want to write the result to a file}
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
  mnit<-getANTsRData("mni")
  mnit<-antsImageRead(mnit,3)
  mniafn<-getANTsRData("mnia")
  mnia<-antsImageRead(mniafn,3)
  ThresholdImage(3,mnia,mnia,22,25)
  SmoothImage(3,mnia,1.5,mnia)
  mnia2<-antsImageRead(mniafn,3)
  ThresholdImage(3,mnia2,mnia2,1,4)
  SmoothImage(3,mnia2,1.5,mnia2)
  plotANTsImage( mnit, list(mnia,mnia2), slices='50x140x5',
   threshold = "0.25x1", axis=0,color=c('red','blue') )
  ofn<-paste(tempfile(),'.png',sep='')
  # write directly to a file
  plotANTsImage( mnit, list(mnia,mnia2), slices='50x140x5',
   threshold = "0.25x1", axis=0,color=c('red','blue'), outname = ofn )
}
