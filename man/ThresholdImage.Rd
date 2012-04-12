\name{ThresholdImage}
\alias{ThresholdImage}

\title{
Theshold Image
}

\description{
Perform thresholding on the given image
}

\usage{
ThresholdImage( "<imageDimension>" , "<inputImage>" , "<outputImage>" , "<thresh-low>" , "<thresh-high>" , "<inside-value>" , "<outside-value>" )
ThresholdImage( "<imageDimension>" , "<inputImage>" , "<outputImage>" , "Otsu" , "<number-of-thresholds>" )
}

\arguments{
\item{imageDimension 2|3}{
Number of dimensions of the input image
}

\item{inputImage}{
Input image to operate on
}

\item{outputImage}{
Result image
}

\item{thresh-low}{
Lower edge of threshold window
}

\item{thresh-high}{
Higher edge of threshold window
}

\item{inside-value}{
Inside value
}

\item{outside-value}{
Outside value
}

\item{number-of-thresholds}{
Number of thresholds
}
}

\value{
0 -- Success\cr
1 -- Failure
}

\author{
Shrinidhi KL
}

\examples{
\dontrun{
ThresholdImage( 3 , "input_img.nii" , "output_img.nii" , 2 , 3 )
ThresholdImage( 3 , "input_img.nii" , "output_img.nii" , "Otsu" , 3 )
}
}
