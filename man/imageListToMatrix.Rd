\name{imageListToMatrix}
\alias{imageListToMatrix}

\title{Read Images into a Matrix}

\description{
  Read images into rows of a matrix.
}

\usage{
imageListToMatrix(imageList, mask)
}

\arguments{
  \item{imageList}{
    A character vector containing a list of image files to read, in
    order - these are image objects, not file names.
  }
  \item{mask}{
    An \code{antsImage} containing a binary mask, voxels in the mask are
    placed in the matrix.
  }
}


\value{
  A matrix containing the masked data, the result of calling
  \code{as.numeric(image, mask)} on each input image.
}
\seealso{
  \code{\link{matrixToImages}, \link{getMask}}
} 

\author{
  Cook PA, Avants B
}

\examples{
\dontrun{

  imageFNList <- list.files("./", pattern = glob2rx("testView*.nii.gz"), full.names = TRUE)
  
  imageList<-list()
  for ( fn in imageFNList ) 
    imageList<-lappend( imageList, antsImageRead( fn, 3 ) )

  mask <- antsImageRead("/mnt/data/masks/brainmask.nii.gz",3)

  mat <- imageListToMatrix(imageList, mask)

}

}
