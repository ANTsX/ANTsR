\name{rfSegmentation}
\alias{rfSegmentation}

\title{
A rfSegmentation function.
}

\description{
Image segmentation via random forests.
}

\usage{
segprobs<-rfSegmentation( labelimage , featureimages )
}

\arguments{
\item{labelimage}{
  input antsImage labelimage --- assume non-zero entries create a mask
}
\item{featureimages}{
  input list of antsImage feature images - length n means n predictors in rf 
}
}

\value{
  list of n-probability images is output where n is number of classes
}

\author{
Tustison NJ, Avants BB
}

\examples{
img<-antsImageRead( getANTsRData('r16') ,2)
mask<-getMask( img )
segs<-Atropos( d = img@dimension, a = img, m = "[0.2,1x1]",c = "[5,0]",  i = "kmeans[3]", x = mask)
imgg<-antsImageClone(img)
ImageMath( img@dimension, imgg, "Laplacian", img )
fimgs<-list(img,imgg)
rfsegs<-rfSegmentation( segs$segmentation,  fimgs ) 
segs2<-Atropos( d = img@dimension, a = fimgs, m = "[0.2,1x1]",c = "[5,0]",  i = rfsegs$probabilityimages, x = mask) 
}
