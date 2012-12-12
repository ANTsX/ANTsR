library(ANTsR)
# We assume a bunch of data has been normalized to a template and now
# you want to do a statistical study.
# Note that the procedure for a first level fMRI study , a 2nd level
# fMRI study or a morphometry / volumetry / correlation study is very similar.
# Also, 2D and 3D are equally easy though not equally fast.
#
mydim<-2
outpre<-"TEST"
pre<-""
glb<-glob2rx(paste(pre,"phantom*bian.nii.gz",sep='')) # get the images
fnl<-list.files(path='./',pattern=glb,full.names=T,recursive=T)
print(fnl)
maskfn<-"phantomtemplate.jpg" # get the mask , should be in same space as image 
glb<-glob2rx(paste(pre,maskfn,sep='')) # get the images
maskfn<-list.files(path='./',pattern=glb,full.names=T,recursive=T)
mask<-antsImageRead(maskfn,'float',mydim)
logmask<-( mask > 120 & mask < 130 )  # get the parts of the mask > 0
notlogmask<-( ! logmask )  # get the parts of the mask > 0
mask[ notlogmask ]<-0 # make sure it holds zeroes
nvox<-sum( c( logmask ) ) # count voxels 
mat<-matrix( length(fnl) * nvox , nrow = length(fnl), ncol = nvox  )
for ( i in 1:length(fnl) )  # create the matrix 
  {
  i1<-antsImageRead(fnl[i],'float',mydim)
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
antsImageWrite(betaimg,paste(outpre,'_beta.nii.gz',sep=''))
pimg<-antsImageClone(mask)
pimg[ logmask ]<-1-pvals # write out 1 minus p to allow display of bright values 
antsImageWrite(pimg,paste(outpre,'_pvals.nii.gz',sep=''))
# corrected pvalues 
qimg<-antsImageClone(mask)
qimg[ logmask ]<-1-p.adjust(pvals,method="BH")
antsImageWrite(qimg,paste(outpre,'_qvals.nii.gz',sep=''))
print('done')
