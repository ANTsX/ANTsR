## ----global options, include=FALSE---------------------------------------
library(knitr)
library(ANTsR)
runLongExamples<-FALSE

## ----basic read,message=FALSE,warnings=FALSE-----------------------------
img<-antsImageRead( getANTsRData("r16"), 2 ) # built in image
img

## ----basic_plot,message=FALSE,warnings=FALSE,echo=FALSE------------------
img2<-antsImageRead( getANTsRData("r64"), 2 ) # built in image
invisible(plot(img2))

## ----physical space------------------------------------------------------
mnifilename<-getANTsRData("r27")
img<-antsImageRead(mnifilename, pixeltype="unsigned char")
img
retval<-antsImageWrite(img,mnifilename)
antsGetSpacing(img)
antsGetDirection(img)
antsGetOrigin(img)
print(img[120,122]) # same type of thing in 3 or 4D
print(max(img))

## ----indexing------------------------------------------------------------
gaussimg<-array( data=rnorm(125), dim=c(5,5,5) )
arrayimg<-array( data=(1:125), dim=c(5,5,5) )
img<-as.antsImage( arrayimg )
print( max(img) )
print( mean(img[ img > 50  ]))
print( max(img[ img >= 50 & img <= 99  ]))
# if using SUBSET using an antsImage, you must be explicit
sub = as.array(img >= 50 & img <= 99) > 0
print( mean( gaussimg[ sub  ]) )

## ----fourd---------------------------------------------------------------
gaussimg<-makeImage(c(5,5,5,10), voxval = rnorm(125*10)  )
print(dim(gaussimg))
avg3d<-getAverageOfTimeSeries( gaussimg )
voxelselect <- avg3d < 0.25
mask<-antsImageClone( avg3d )
mask[ voxelselect  ]<-0
mask[ !voxelselect  ]<-1
gmat<-timeseries2matrix( gaussimg, mask )
print(dim(gmat))

## ----makeimage-----------------------------------------------------------
newimg<-makeImage( mask, mean(avg3d) )    # from scalar
newimg<-makeImage( mask, colMeans(gmat) ) # from vector

## ----imagelist-----------------------------------------------------------
nimages<-100
ilist<-list()
for ( i in 1:nimages )
  {
  simimg<-makeImage( c(50,50) , rnorm(2500) )
  simimg<-smoothImage(simimg,1.5)
  ilist[[ i  ]] = simimg
  }
# get a mask from the first image
mask<-getMask( ilist[[1]],
  lowThresh=mean(ilist[[1]]), cleanup=TRUE )
mat<-imageListToMatrix( ilist, mask )
print(dim(mat))

## ----image matrix--------------------------------------------------------
mat<-imageListToMatrix( ilist, mask )
age<-rnorm( nrow(mat) ) # simulated age
gender<-rep( c("F","M"), nrow(mat)/2 ) # simulated gender
# this creates "real" but noisy effects to detect
mat<-mat*(age^2+rnorm(nrow(mat)))
mdl<-lm( mat ~ age + gender )
mdli<-bigLMStats( mdl, 1.e-4 )
print(names(mdli))
print(rownames(mdli$beta.t))
print(paste("age",min(p.adjust(mdli$beta.pval[1,]))))
print(paste("gen",min(p.adjust(mdli$beta.pval[2,]))))

## ----write betas---------------------------------------------------------
agebetas<-makeImage( mask , mdli$beta.t[1,] )
returnval<-antsImageWrite( agebetas, tempfile(fileext ='.nii.gz') )

## ----segmentation--------------------------------------------------------
fi<-antsImageRead( getANTsRData("r16") ,2)
fi<-n3BiasFieldCorrection(fi,2)
seg<-kmeansSegmentation( fi, 3 )
invisible(plot(seg$segmentation))

## ----syn,echo=TRUE,message=FALSE,warnings=FALSE--------------------------
mi<-antsImageRead( getANTsRData("r64") ,2)
mytx<-antsRegistration(fixed=fi , moving=mi ,
    typeofTransform = c('SyN'))
regresult<-iMath(mytx$warpedmovout,"Normalize")
fiedge<-iMath(fi,"Canny",1,5,12)
invisible(plot(regresult, list(fiedge), window.overlay=c(0.5,1)) )

## ----jacobian,echo=TRUE,message=FALSE,warnings=FALSE---------------------
jac<-createJacobianDeterminantImage(fi,mytx$fwdtransforms[[1]],1)
invisible(plot(jac))

## ----nhood---------------------------------------------------------------
mnit<-getANTsRData("r16")
mnit<-antsImageRead( mnit )
mnit <- resampleImage( mnit , rep(4, mnit@dimension) ) # downsample
mask2<-getMask(mnit,lowThresh=mean(mnit),cleanup=TRUE)
radius <- rep(2,mnit@dimension)
mat2<-getNeighborhoodInMask(mnit, mask2, radius,
  physical.coordinates = FALSE,
  boundary.condition = "mean" )
print(dim(mat2))

## ----aal-----------------------------------------------------------------
data(aal,package='ANTsR')
labs<-1:90

## ----motioncorr,results='hide',eval=FALSE--------------------------------
#  # get an average image
#  averageImage <- getAverageOfTimeSeries( boldImage )
#  motionCorrectionResults <- antsMotionCalculation( boldImage,
#     fixed = averageImage )

## ----eanat,echo=TRUE,message=FALSE,warning=FALSE,eval=FALSE--------------
#  # assume you ran the population example above
#  eanat<-sparseDecom( mat, mask, 0.2, 5, cthresh=2, its=2 )
#  eanatimages = matrixToImages( eanat$eig, mask )
#  eseg<-eigSeg(mask, eanatimages ,F)
#  jeanat<-joinEigenanatomy(mat, mask, eanatimages, c(0.1))
#  eseg2<-eigSeg(mask,jeanat$fusedlist,F)

## ----eanatreal,eval=FALSE------------------------------------------------
#  eanat<-sparseDecom( inmatrix=mat, inmask=famask, nvecs=50,
#    sparseness=0.005, cthresh=500, its=5, mycoption=0 )
#  jeanat<-joinEigenanatomy( mat , famask, eanat$eig,
#    c(1:20)/100.0 , joinMethod='multilevel' )
#  useeig<-eanat$eig
#  useeig<-jeanat$fusedlist
#  avgmat<-abs(imageListToMatrix( useeig , famask ))
#  avgmat<-avgmat/rowSums(abs(avgmat))
#  imgmat<-(  mat %*% t(avgmat)  )

