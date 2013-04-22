\name{imageFileNames2ImageList}
\alias{imageFileNames2ImageList}

\title{
Simple imageFileNames2ImageListing function.
}

\description{
ImageFileNames2ImageLists converts the input list of file names to a list containing antsImages.
}

\usage{
  gfnl<-imageFileNames2ImageList( c("a.nii.gz","b.nii.gz") , dim = 3 )
}

\arguments{
\item{x}{
  input file name list
}
}

\value{
a list like this :  mylist<-list( img1, img2 , etcetera ) is the output ... img* are antsImages
}

\author{
Avants BB
}

\examples{
  gglb<-paste("gmView1vec*.nii.gz",sep='')
  gfnl<-imageFileNames2ImageList( list.files(path=statdir, pattern = glob2rx(gglb),full.names = T,recursive = T) , 3 )
}
}
