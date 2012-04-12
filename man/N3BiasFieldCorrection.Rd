\name{N3BiasFieldCorrection}
\alias{N3BiasFieldCorrection}

\title{
Bias Field Correction
}

\description{
Perform Bias Field Correction on the given image
}

\usage{
N3BiasFieldCorrection( "<imageDimension>" , "<inputImage>" , "<outputImage>" [, "<shrinkFactor>"] [, "<maskImage>"] [, "<numberOfIterations>"] [, "<numberOfFittingLevels>"] [, "<outputBiasField>"])
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

\item{shrikFactor}{
Shrink Factor
}

\item{maskImage}{
Image to be used as mask
}

\item{numberofIterations}{
Number of Iterations
}

\item{numberofFittingLevels}{
Number of fitting levels
}

\item{outputBiasField}{
Output bias-field
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
N3BiasFieldCorrection( 3 , "input_img.nii" , "output_img.nii" , 2 )
}
}
