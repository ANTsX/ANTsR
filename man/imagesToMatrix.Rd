\name{imagesToMatrix}
\alias{imagesToMatrix}

\title{
Read Images into a Matrix
}

\description{
  Read images into rows of a matrix.
}

\usage{
imagesToMatrix(imageList, mask)
}

\arguments{
  \item{imageList}{
    A character vector containing a list of image files to read, in
    order.
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
  Cook PA
}

\examples{

# make some simulated images and convert them to a matrix

  n <- 8
  tdir<-tempdir()
  for ( i in 1:n ) {
    simimg<-as.antsImage( replicate(64, rnorm(64) ) )
    antsImageWrite( simimg, paste(tdir,"/image",i,".mha",sep=''))
  }
  imageList = list.files(tdir, pattern = "\\.mha", full.names = TRUE)
  mask = getMask( antsImageRead( imageList[1] , 2 ) )
  mat = imagesToMatrix(imageList, mask)
  print(dim(mat))

}
