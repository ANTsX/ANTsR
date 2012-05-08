\name{antsImageWrite}
\alias{antsImageWrite}

\title{
Image Write
}

\description{
Write an image object of S4 class 'antsImage' to a file.
}

\usage{
antsImageWrite( image , filename )
}

\arguments{
\item{image}{
Image object of S4 class 'antsImage' to be written.
}

\item{filename}{
Name of the file to write the image to. 
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
# write an image 'img' of class 'antsImage' to a file named 'imagefile.nii'
antsImageWrite( img , "imagefile.nii" )
}
}
