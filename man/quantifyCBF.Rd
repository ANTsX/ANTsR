\name{quantifyCBF}
\alias{quantifyCBF}

\title{
Simple quantifyCBF function.
}

\description{
Computes CBF from ASL - pasl or pcasl
}

\usage{
cbfrecon<-quantifyCBF( aslmat, aslmask , parameters )
}

\arguments{
\item{aslmat}{
  input asl matrix
}
\item{aslmask}{
  3D image mask (antsImage)
}
\item{parameters}{
  list with entries for sequence and m0 (at minimimum)
}
}

\value{
a list is output with 3 types of cbf images
}

\author{
Avants BB, Kandel B, Duda JT
}

\examples{
  \dontrun{
  asl<-antsImageRead("PEDS012_20131101_pcasl_1.nii.gz",4)
# image available at http://files.figshare.com/1701182/PEDS012_20131101.zip
  pcasl.processing <- aslPerfusion( asl, moreaccurate=F, interpolation="linear",
    dorobust=0.95 )
  pcasl.parameters <- list( sequence="pcasl", m0=pcasl.processing$m0 )
  cbf <- quantifyCBF( pcasl.processing$perfusion, pcasl.processing$mask, pcasl.parameters )
  meancbf <- cbf$kmeancbf
  print(mean(meancbf[ pcasl.processing$mask==1 ]))
  antsImageWrite( meancbf , 'temp.nii.gz')
  plotANTsImage(  meancbf, slices='1x50x1')
  }
}
