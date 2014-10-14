\name{aslDenoiseR}
\alias{aslDenoiseR}
\title{
WIP: data-driven denoising for ASL MRI
}
\description{
Denoises regression based reconstruction of CBF from arterial spin labeling
}
\usage{
wmat<-aslDenoiseR( mat  )
}
\arguments{
\item{mat}{
  input ASL matrix
}
}
\value{
matrix is output
}
\author{
Avants BB
}

\examples{
\dontrun{
asl<-antsImageRead("PEDS012_20131101_pcasl_1.nii.gz",4)
# image available at http://files.figshare.com/1701182/PEDS012_20131101.zip
tr<-antsGetSpacing(asl)[4]
aslmean<-getAverageOfTimeSeries( asl )
aslmask<-getMask(aslmean,lowThresh=mean(aslmean),cleanup=TRUE)
aslmat<-timeseries2matrix(asl,aslmask)
tc<-as.factor(rep(c("C","T"),nrow(aslmat)/2))
dnz<-aslDenoiseR( aslmat, tc, motionparams=NA, selectionthresh=0.5,
  maxnoisepreds=c(2,4,6,8), debug=FALSE, polydegree=4,
  crossvalidationgroups=6 )
nzimg<-antsImageClone(aslmask)
nzimg[ aslmask == 1 ]<-dnz$R2final[,1]
antsImageWrite(nzimg,'nzimg1.nii.gz')
nzimg[ aslmask == 1 ]<-dnz$R2final[,2]
antsImageWrite(nzimg,'nzimg2.nii.gz')
}
}
