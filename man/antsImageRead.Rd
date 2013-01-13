\name{antsImageRead}
\alias{antsImageRead}

\title{
Image Read
}

\description{
Read an image file into an S4 object of class 'antsImage'.
}

\usage{
antsImageRead( filename ,  dimension = 3 , pixeltype = "float" )
}

\arguments{
\item{filename}{
Name of the file to read the image from. 
}

\item{pixeltype}{
C++ datatype to be used to represent the pixels read. This datatype need not be the same as the datatype used in the file. Allowed values: "double", "float" , "unsigned int" , "unsigned char".
}

\item{dimension}{
Number of dimensions of the image read. This need not be the same as the dimensions of the image in the file. Allowed values: 2, 3, 4.
}
}

\value{
S4 object of Class 'antsImage' -- Success\cr
                             1 -- Failure
}

\author{
Shrinidhi KL
}

\examples{
\dontrun{
# read an image from a file named 'imagefile.nii' into an image of pixeltype 'double' and dimensions '4'
img <- antsImageRead( "imagefile.nii" , dimension = 3  )
img <- antsImageRead( "imagefile.nii" , dimension = 2  )
img <- antsImageRead( "imagefile.nii" , dimension = 4 , pixeltype =  "double" )
}
}
