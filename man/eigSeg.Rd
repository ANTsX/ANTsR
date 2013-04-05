\name{eigSeg}
\alias{eigSeg}

\title{
Segmentation for eigenanatomy.
}

\description{
Segment a mask into regions based on the max value in an image list.   At a given voxel the segmentation label will contain the index to the image that has the largest value.  If the 3rd image has the greatest value, the segmentation label will be 3 at that voxel.
}

\usage{
eigsegimage<-eigSeg( mask, imageList  )
}

\arguments{
\item{mask}{
  D-dimensional mask  > 0 defining segmentation region.
}

\item{imageList}{
 list containing antsImages.
}


}

\value{
segmentation image.
}

\author{
Avants BB
}

\examples{
\dontrun{
mylist<-list(image1,image2)
# 
myseg<-eigSeg( mask, mylist ) 
}
}
