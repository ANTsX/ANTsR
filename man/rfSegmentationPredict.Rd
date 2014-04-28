\name{rfSegmentationPredict}
\alias{rfSegmentationPredict}

\title{
A rfSegmentationPredict function.
}

\description{
Predict image segmentation via random forests.
}

\usage{
seg<-rfSegmentationPredict( model , featureimages , mask )
}

\arguments{
\item{model}{
  input rf model
}
\item{featureimages}{
  input list of antsImage feature images - should match model
}
}

\value{
  segmentation is output
}

\author{
Tustison NJ, Avants BB
}

\examples{
library(randomForest)
img<-antsImageRead( getANTsRData('r16') ,2)
mask<-getMask( img )
mask2<-getMask( img )
ImageMath(2,mask,"ME",mask,25)
mask2[ mask == 1 ]<-0
segs<-Atropos( d = img@dimension, a = img, m = "[0.2,1x1]",c = "[5,0]",  i = "kmeans[3]", x = mask)
fimgs<-lappend( img )
rfsegs<-rfSegmentation( segs$segmentation, fimgs , ntrees=100, verbose=TRUE ) 
rfseg2<-rfSegmentationPredict(  rfsegs$rfModel , fimgs , mask2 ) 
plotANTsImage( rfseg2 )
}
