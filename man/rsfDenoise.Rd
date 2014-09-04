\name{rsfDenoise}
\alias{rsfDenoise}
\title{
WIP: data-driven denoising for resting state fMRI
}
\description{
Uses a target function to denoise resting bold data
}
\usage{
wmat<-rsfDenoise( mat  )
}
\arguments{
\item{mat}{
  input bold matrix
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
bold<-antsImageRead("bold.nii.gz",4)
boldmask<-antsImageRead("meanboldmask.nii.gz",3)
aalimg<-antsImageRead("meanboldAALmask.nii.gz",3)
data("aal",package='ANTsR')
dmnlabels<-aal$label_num[aal$isdmn>0]
aalvec<-aalimg > 0 
whichregion<-3
for ( i in 1:max(aalimg) )
  {
  if ( ! ( i %in% dmnlabels[whichregion] ) ) 
    {
    aalimg[ aalimg == as.numeric(i) ]<-0
    }
  }
maskvec<-boldmask > 0 & aalimg == as.numeric(dmnlabels[whichregion])
boldmask[ maskvec ]<-0
timeselect<-10:dim(bold)[4]
if ( ! exists("moco") ) moco<-motion_correction(bold,moreaccurate=0)$moco_params[timeselect,]
boldmat<-timeseries2matrix(bold,boldmask)
boldmat<-boldmat[timeselect,]
aalimg[aalimg > 0 ]<-1
dmnvec<-rowMeans(timeseries2matrix(bold,aalimg))[timeselect]
dmnvec<-(stl(ts(dmnvec, frequency = 4),"per")$time.series)[,2]
dmnmat<-matrix( dmnvec, ncol=1)
dnz<-rsfDenoise( boldmat , dmnmat[,1], motionparams=moco, polydegree=4,
                crossvalidationgroups = 8, maxnoisepreds=1:4, debug=F )
print(paste("Best number of noise regressors",dnz$n))
# now recompute the matrix using the full mask
boldmask<-antsImageRead("meanboldmask.nii.gz",3)
boldmat<-timeseries2matrix(bold,boldmask)
boldmat<-boldmat[timeselect,]
mdl<-bigLMStats( lm( boldmat ~ dmnmat[,1] + dnz$polys + dnz$noiseu ), 0.001 )
betas<-mdl$beta.t[1,]
}
}