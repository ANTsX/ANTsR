\name{Extract}
\alias{Extract}

\title{
Extract/Replace regions an antsImage
}

\description{
Set of methods to extract a \code{numeric vector} from an \code{antsImage}. Set of methods to replace the pixels in an \code{antsImage} using a \code{numeric vector}.
}

\usage{
img[ mask ]
img[ lst ]
img[ mask , region ]
img[ row , col , slice , time ]

img[ mask ] <- vect
img[ lst ] <- vect
img[ mask , region ] <- vect
img[ row , col , slice , time ] <- vect
}

\arguments{
\item{img}{
Image object of S4 class 'antsImage' to be indexed.
}

\item{mask}{
\code{logical vector/matrix/array} to be used as mask while extracting pixel values from the image. \code{1/TRUE}s return corresponding pixel values ; \code{0/FALSE}s return \code{NA}. The vector provided will be 'matched' against the image-region column-wise meaning that first column of image-region is extracted using first column-length values from the vector, second column is extracted using the next column-length values from the vector and so on. Default: entire region is extracted.
}

\item{region}{
\code{antsRegion} with in the \code{antsImage} to be considered for extraction. An \code{antsRegion} object can be created using \code{new}. Default: largest-possible-region( entire image ) in the image.
}

\item{lst}{
\code{list} of named arguments containing names 'mask' and 'region'. \code{lst$mask} is used as 'mask' and \code{lst$region} is used as 'region'.
}

\item{vect}{
\code{numeric vector} of length equal to number of pixels in the 'region' considered. Depending on the mask, the values in the numeric vector will replace the corresponding pixel values in the image indexed by the region. Only the values corresponding to \code{1/TRUE} in the 'mask' are used. Image pixels corresponding to \code{0/FALSE} in the 'mask' are left unchanged. This vector is matched against the image-region column-wise meaning that the first column of the image-region gets its values from the first column-length values in this vector, second column of the image-region gets its values from the second column-length values in this vector and so on.
}

\item{row}{
\code{numeric vector} used to select the 'row' dimension of the image. Use 'NULL' to access the entire 'row' of the image.
}

\item{col}{
\code{numeric vector} used to select the 'column' dimensin of the image. Use 'NULL' to access the entire 'column' of the image.
}

\item{slice}{
\code{numeric vector} used to select the 'slice' dimension of the image. Use 'NULL' to access the all 'slice's of the image.
}

\item{time}{
\code{numeric vector} used to select the 'time' dimension of the image. Use 'NULL' to access all the 'time's of the image.
}

}

\details{
\describe{
\item{\code{img[mask]}}{
Image-region considered is the largest-possible-region( entire image ) of the image.
}

\item{\code{img[row,col,slice,time]}}{
Number of indices must match the number of dimensions of the image. A 2D image requires and allows indices 'row' and 'col' only. A 3D image requires and allows indices 'row', 'col' and 'slice' only. A 4D image requires and allows indices 'row', 'col', 'slice' and 'time'.
}
}
}

\value{
Extractors : \cr
\code{numeric vector} of dimensions equal to that of 'region' considered or number of 'pixels' considered -- Success ; NA -- Failure \cr
Mutators : \cr
Modified S4 object of class \code{antsImage} -- Success ; Original S4 object of class \code{antsImage} -- Failure
}

\author{
Shrinidhi KL
}

\examples{
\dontrun{
# extract a vector from an image 'img' of class 'antsImage' by considering only the region starting at index (1,1,1) with size (10,10,10) ;
# here the first 10 elements of vect correspond to first column ( row = 1:10 , col = 1 , slice = 1 ) of image-region, next 10 elements come from
# second column ( row = 1:10 , col = 2 , slice = 1 ) of the image-region and so on
reg = new( "antsRegion" , index = c(1,1,1) , size = c(10,10,10) )
vect = img[ NULL , region = reg ] # OR
vect = img[ list( mask = NULL , region = reg ) ]
# set the values of pixels in the square ( row = 1:10 , col = 1:10 , slice = 3 ) using the vector c(1:100) ; 
# here the first column ( row = 1:10 , col = 1 , slice = 3 ) of image gets values c(1:10), second column ( row = 1:10 , col = 2 , slice = 3 ) gets
# values c(11:20) and so on
img[ 1:10 , 1:10 , 3 ] <- c(1:100)
}
}
