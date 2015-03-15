#' Uses probabilistic segmentation to constrain pcasl-based cbf computation.
#'
#' Employs a robust regression approach to learn the relationship between a
#' sample image and a list of images that are mapped to the same space as the
#' sample image.
#'
#'
#' @param pcasl img antsImage for cbf
#' @param segmentation image, should cover the brain.
#' @param tissuelist a list containing antsImages eg list(prob1,...,probN)
#' @param myPriorStrength - e.g 30
#' @param useDataDrivenMask - morphology parameters e.g. 3
#' @param denoisingComponents - data-driven denoising parameters
#' @param robustnessvalue - value (e.g. 0.95) that throws away time points
#' @param localweights Use estimate of voxel-wise reliability to inform prior
#' weight?
#' @param priorBetas prior betas for each tissue and predictor
#' @return estimated cbf image
#' @author Brian Beaumont Avants and Benjamin M. Kandel
#' @keywords asl, bayesian blood cerebral flow,
#' @examples
#'
#' \dontrun{
#' set.seed(123)
#' # see fMRIANTs github repository for data and I/O suggestions
#' }
#'
#' @export bayesianCBF
bayesianCBF<-function( pcasl, segmentation, tissuelist,
  myPriorStrength=30.0,
  useDataDrivenMask=3,
  denoisingComponents=1:8,
  robustnessvalue=0.95, # higher rejects more data. 0.9 or less - keep all
  localweights=F, priorBetas=NA
)
{
compcorComponents<-0
motionAcc<-2 # motion accuracy - 0 is for testing, 1 or 2 real studies
if ( all(dim(tissuelist[[1]])==1) | all(dim(seg)==1) |  all(dim(pcasl)==1) )
  stop(paste("Check your input data"))
avg<-getAverageOfTimeSeries(pcasl)
avg<-n3BiasFieldCorrection(avg,2)
avg<-n3BiasFieldCorrection(avg,2)
mask<-antsImageClone(seg)
mask[ mask > 0 ]<-1
if ( useDataDrivenMask > 0 )
  {
  mask2<-getMask(avg,mean(avg),Inf,useDataDrivenMask)
  # cleans up mask to agree with data-driven mask2
  mask[mask2==0]<-0
  seg[mask2==0]<-0
  }
aslmat<-timeseries2matrix(pcasl, mask)
perfpro <- aslPerfusion( pcasl, skip=10,
        dorobust=robustnessvalue, useDenoiser=denoisingComponents,
        moreaccurate=motionAcc, verbose=1, mask=mask, useBayesian=0,
        ncompcor=compcorComponents )
perfpro$m0<-n3BiasFieldCorrection(perfpro$m0,2)
pcasl.parameters <- list( sequence="pcasl", m0=perfpro$m0 )
perfdf<-data.frame( xideal=perfpro$xideal,
            nuis=perfpro$nuisancevariables)
perfdf<-perfdf[,!is.na(colMeans(perfdf))]
perfmodel<-lm( aslmat ~.,data=perfdf, weights=perfpro$regweights )
getpriors<-function( img, seg )
  {
  n<-max(seg)
  p<-rep(0,n)
#  Use the median to be conservative.
  segvec<-( seg[ seg > 0 ] )
  for ( i in 1:n ) p[i]<-median( img[ segvec == as.numeric(i) ] )
  return(p)
  }
if ( all( is.na( priorBetas ) ) )
  {
  blm<-bigLMStats( perfmodel, includeIntercept=T )
  bayespriormatfull<-blm$beta
  }
if ( ! all( is.na( priorBetas ) ) )
  {
  bayespriormatfull<-priorBetas
  }
n<-max(seg)*nrow(bayespriormatfull)
bayespriormat<-matrix( rep(0,n), nrow=max(seg) )
for( i in 1:ncol(bayespriormat) )
  bayespriormat[,i]<-getpriors( bayespriormatfull[i,] , seg )
# set 4 to equal 2 - dgm = gm
bayespriormat[4,]<-bayespriormat[2,]
# set csf to zero perfusion
bayespriormat[1,2]<-0
X<-model.matrix( perfmodel )
localtissuemat<-imageListToMatrix(tissuelist,mask)
priorwt<-diag(ncol(bayespriormat))*myPriorStrength
priorwt[3:ncol(priorwt),3:ncol(priorwt)]<-0
## alternative priors below
# instead use bayespriormatfull - to est cov?
priorwt2<-solve(cov(bayespriormat)+
   diag(ncol(bayespriormat))*.1)*myPriorStrength
bayesianperfusionloc<-localtissuemat*0
bayesianperfusionlocp<-localtissuemat*0
if (localweights) {
  motion_params <- .motion_correction(pcasl, moreaccurate=1)$moco_params[, 1:4]
  reliability <- aslDenoiseR(aslmat, perfpro$xideal, motion_params,
    usecompcor=T)$R2final
  reliability[reliability<0.05] <- 0.05
  unreliability = log(min(reliability) / reliability)
  if(max(unreliability)<=0){
    unreliability <- unreliability - min(unreliability)
  }
  unreliability <- unreliability / max(unreliability)
}
for ( i in 1:ncol(aslmat) )
  {
  # here is where we get really bayesian
  # average over all tissue models ...
  localtissuemat[,i]<-abs(localtissuemat[,i])/
    sum(abs(localtissuemat[,i]))
  for ( segval in 1:max(seg) )
    {
    tissueprior<-localtissuemat[segval,i]
    localprior<-bayespriormat[segval,]
    if(!localweights) {
      blm<-bayesianlm(  X, aslmat[,i], localprior, priorwt,
                      regweights=perfpro$regweights,
                      includeIntercept=T )
    } else {
      blm<-bayesianlm(  X, aslmat[,i], localprior, priorwt*unreliability[i],
                      regweights=perfpro$regweights,
                      includeIntercept=T )
    }
    locbeta<-blm$beta[2]
    bayesianperfusionloc[segval,i]<-locbeta
    bayesianperfusionlocp[segval,i]<-locbeta*tissueprior
    }
  }
bperfimg<-makeImage(mask,colSums(bayesianperfusionlocp))
bcbf<- quantifyCBF( bperfimg, mask,pcasl.parameters )
bcbf<-bcbf$meancbf
bayespriormatfull[2,]<-colSums(bayesianperfusionlocp)
return( list(bcbf=bcbf, nz=sum(perfpro$regweights<0.001),
  posteriorBetaSummary=bayespriormatfull ) )
}
