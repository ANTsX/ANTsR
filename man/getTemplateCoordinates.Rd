\name{getTemplateCoordinates}
\alias{getTemplateCoordinates}

\title{Define an anatomical coordinate system in a new image based on a template}

\description{
 This function will provide a mapping that labels an input image and its blobs.

 Uses Matthew Brett's mni2tal to get the final Talairach coordinates from MNI space.
}

\usage{
}

\arguments{
  \item{x}{
  x
  }
}
\details{
  This is a standard approach but it's not very accurate.
}


\value{
  The output point is in approximate template space.
}

\author{
  Avants, BB
}

\examples{
\dontrun{
 tem<-antsImageRead('templates/template_brain.nii.gz',3)
 temlab<-antsImageRead('temp.nii.gz',3)
 # try getANTsRData('ch2') & getANTsRData('ch2b') if you have www access
 mych2<-list( antsImageRead(getANTsRData('ch2'),3), 
              antsImageRead(getANTsRData('ch2b'),3) )
 mytem<-list(tem,temlab)
 coords<-getTemplateCoordinates( mytem , mych2 , convertToTal = TRUE )
 plotANTsImage( coords$myImageInTemplateSpace,slices="100x100x1")
}
}
\keyword{Talairach, Template, Coordinates}
