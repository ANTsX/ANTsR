library(ANTsR)
mydim<-2
pre<-""
glb<-glob2rx(paste(pre,"phantom*bian.nii.gz",sep='')) # get the images
fnl<-list.files(path='./',pattern=glb,full.names=T,recursive=T)
print(fnl)
pre<-"TEST"
maskfn<-"./example_images/phantomtemplate.jpg" # get the mask , should be in same space as image
mask<-antsImageRead(maskfn,mydim)
logmask<-( mask > 120 & mask < 130 )  # get the parts of the mask > 0
notlogmask<-( ! logmask )  # get the parts of the mask > 0
mask[ notlogmask ]<-0 # make sure it holds zeroes
nvox<-sum( c( logmask ) ) # count voxels
mat<-matrix( length(fnl) * nvox , nrow = length(fnl), ncol = nvox  )
for ( i in 1:length(fnl) )  # create the matrix
  {
  i1<-antsImageRead(fnl[i],mydim)
  vec<-i1[ logmask ]
  mat[ i, ]<-vec
  }
predictor<-c(rep(2,4),rep(1,4)) # identify your predictors and use in regression
testformula<-( vox ~ 1 + predictor ) # the regression
betavals<-rep(NA, nvox )
pvals<-rep(NA, nvox )
ntst<-1
while ( ntst < (nvox+1) ) # there are better/faster ways but this is simple
  {
  vox<-mat[,ntst]
  summarymodel<-summary(lm( testformula ))
  betavals[ntst]<-summarymodel$coef[2,3]  # get the t-vals for this predictor and write to an image
  pvals[ntst]<-summarymodel$coef[2,4]  # get the beta for this predictor and write to an image
  ntst<-ntst+1
  }
betaimg<-antsImageClone(mask)
betaimg[ logmask ]<-betavals
antsImageWrite(betaimg,paste(pre,'_beta.nii.gz',sep=''))
pimg<-antsImageClone(mask)
pimg[ logmask ]<-1-pvals # write out 1 minus p to allow display of bright values
antsImageWrite(pimg,paste(pre,'_pvals.nii.gz',sep=''))
qimg<-antsImageClone(mask)
qimg[ logmask ]<-1-p.adjust(pvals,method="BH") # BH corrected p-values
antsImageWrite(qimg,paste(pre,'_qvals.nii.gz',sep=''))
print('done')
