## @knitr Q1
library(knitr)
library(ANTsR)
mydim<-2
#' define prefix for output files
outpre<-"TEST"
#' get the images
glb<-glob2rx(paste("phantom*bian.nii.gz",sep=''))
fnl<-list.files(path='../../',pattern=glb,full.names=T,recursive=T)
maskfn<-"phantomtemplate.jpg" 
#' get the mask , should be in same space as image 
glb<-glob2rx(paste(maskfn,sep=''))
maskfn<-list.files(path='../../',pattern=glb,full.names=T,recursive=T)
mask<-antsImageRead(maskfn,'float',mydim)
#' get regions of mask according to logical comparisons
logmask<-( mask > 120 & mask < 130 )
notlogmask<-( ! logmask )
#' make sure it holds zeroes
mask[ notlogmask ]<-0

## @knitr Q2
#' count voxels and create matrix to hold image data
nvox<-sum( c( logmask ) ) 
mat<-matrix( length(fnl) * nvox , nrow = length(fnl), ncol = nvox  )
for ( i in 1:length(fnl) )
  {
  i1<-antsImageRead(fnl[i],'float',mydim)
  vec<-i1[ logmask ]
  mat[ i, ]<-vec
  }
#' identify your predictors and use in regression 
predictor<-c(rep(2,4),rep(1,4))
#' the regression for your study
testformula<-( vox ~ 1 + predictor ) 
betavals<-rep(NA, nvox )
pvals<-rep(NA, nvox )
ntst<-1
#' there are better/faster ways but this is simple 
while ( ntst < (nvox+1) ) 
  {
  vox<-mat[,ntst]
  summarymodel<-summary(lm( testformula ))
#' get the t-vals for this predictor and write to an image
  betavals[ntst]<-summarymodel$coef[2,3]
#' get the beta for this predictor and write to an image
  pvals[ntst]<-summarymodel$coef[2,4]  
  ntst<-ntst+1
  }
betaimg<-antsImageClone(mask)
betaimg[ logmask ]<-betavals
antsImageWrite(betaimg,paste(outpre,'_beta.nii.gz',sep=''))
pimg<-antsImageClone(mask)
#' write out 1 minus p to allow display of bright values
pimg[ logmask ]<-1-pvals  
antsImageWrite(pimg,paste(outpre,'_pvals.nii.gz',sep=''))
#' corrected pvalues 
qimg<-antsImageClone(mask)
qvals<-p.adjust(pvals,method="BH")
sigct<-sum( c( qvals < 0.05 ) )
qimg[ logmask ]<-1-qvals
antsImageWrite(qimg,paste(outpre,'_qvals.nii.gz',sep=''))
teststat<-round( sigct/sum(logmask)*100 )
print(paste('done',teststat,"% significant"))
isucceeded<-FALSE
if ( teststat == 60 )
  isucceeded<-TRUE
