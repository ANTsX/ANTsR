\name{SmoothImage}
\alias{SmoothImage}

\title{
Smooth an image
}

\description{
Perform smoothing on the given image with a given sigma
}

\usage{
SmoothImage( "<imageDimension>" , "<inputImage>" , "Sigma" , "<outputimage>")
}

\arguments{
\item{imageDimension 2|3|4}{
Number of dimensions of the input image
}

\item{inputImage}{
Input image to operate on
}


\item{Sigma}{
Smoothing factor
}

\item{outputImage}{
Result image
}

}

\value{
0 -- Success\cr
1 -- Failure
}

\author{
Shrinidhi KL, Avants BB
}

\examples{
\dontrun{
SmoothImage( 3 , "input_img.nii" ,1, "output_img.nii")
}
}
