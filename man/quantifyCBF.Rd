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
  if (!exists("fn") ) fn<-"PEDS012_20131101_pcasl_1.nii.gz"
  # PEDS029_20101110_pcasl_1.nii.gz # high motion subject
  asl<-antsImageRead(fn,4)
# image available at http://files.figshare.com/1701182/PEDS012_20131101.zip
  pcasl.bayesian <- aslPerfusion( asl, interpolation="linear",
        dorobust=0., useDenoiser=4, skip=11, useBayesian=1000,
        moreaccurate=0, verbose=T, maskThresh=0.5 ) # throw away lots of data
# user might compare to useDenoiser=FALSE
  pcasl.parameters <- list( sequence="pcasl", m0=pcasl.bayesian$m0 )
  cbf <- quantifyCBF( pcasl.bayesian$perfusion, pcasl.bayesian$mask,
     pcasl.parameters )
  meancbf <- cbf$kmeancbf
  print(mean(meancbf[ pcasl.bayesian$mask==1 ]))
  antsImageWrite( meancbf , 'temp.nii.gz')
  pcasl.processing <- aslPerfusion( asl, moreaccurate=0,
    interpolation="linear",
    dorobust=0.95, useDenoiser=NA, skip=5,  useBayesian=0 )
  # user might compare to useDenoiser=FALSE
  pcasl.parameters <- list( sequence="pcasl", m0=pcasl.processing$m0 )
  cbf <- quantifyCBF( pcasl.processing$perfusion, pcasl.processing$mask, pcasl.parameters )
  meancbf <- cbf$kmeancbf
  print(mean(meancbf[ pcasl.processing$mask==1 ]))
  antsImageWrite( meancbf , 'temp2.nii.gz')
  plotANTsImage(  meancbf, slices='1x50x1')
  }
}
