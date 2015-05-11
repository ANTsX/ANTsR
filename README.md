# ANTsR

[![Build Status](https://travis-ci.org/stnava/ANTsR.png?branch=master)](https://travis-ci.org/stnava/ANTsR) [![Coverage Status](https://coveralls.io/repos/stnava/ANTsR/badge.svg)](https://coveralls.io/r/stnava/ANTsR)

An R package providing [ANTs](http://stnava.github.io/ANTs/) features in R.

## Description

Version: 0.3.1

License: GPL (>=2)

Depends:	R (≥ 3.0), methods

Imports:	Rcpp, tools, magrittr

LinkingTo:	Rcpp, ITKR

Author:	Brian B. Avants, Benjamin M. Kandel, Jeff T. Duda, Philip A. Cook, Nicholas J. Tustison

Maintainer:	[Brian B. Avants](http://stnava.github.io/)

URL:	[homepage](http://stnava.github.io/ANTsR/)

BugReports: [github issues](http://github.com/stnava/ANTsR/issues)

NeedsCompilation:	yes

Travis checks:	[ANTsR results](https://travis-ci.org/stnava/ANTsR)

## Downloads

Reference manual:	[ANTsR.pdf](https://github.com/stnava/ANTsR/releases/download/v0.3.1/ANTsR-manual.pdf)

Vignettes:

*	[ANTsR](http://htmlpreview.github.io/?https://github.com/stnava/ANTsDoc/blob/master/html/ANTsR.html)

* [iMath](http://htmlpreview.github.io/?https://github.com/stnava/ANTsDoc/blob/master/html/iMath.html)

Wiki: [Notes and work in progress examples](https://github.com/stnava/ANTsR/wiki)

Package source:	[from github](https://github.com/stnava/ANTsR/zipball/master)

OS X Mavericks, Yosemite binaries:	[OSX](https://github.com/stnava/ANTsR/releases/download/v0.3.1/ANTsR_OSX_0.3.1.tgz)

Linux binaries: [Ubuntu](https://github.com/stnava/ANTsR/releases/download/v0.3.1/ANTsR_1.0_R_x86_64-pc-linux-gnu.tar.gz)

We are working toward Windows binaries.

Install the binary, after downloading, via command line:

```
R CMD INSTALL ANTsR_*.tgz
```

## Research using ANTsR

* [Inter-modality inference](http://www.ncbi.nlm.nih.gov/pubmed/25449745) yet to be added RIPMMARC

* [Eigenanatomy for multiple modality population studies](http://www.ncbi.nlm.nih.gov/pubmed/25448483) function `sparseDecom`

* [Tumor segmentation](http://www.ncbi.nlm.nih.gov/pubmed/25433513) function `mrvnrfs` (not exactly the same but close)

* [Multiple modality pediatric template and population study](http://www.nature.com/articles/sdata20153) employs several aspects of ANTsR

* [Structural networks from subject-level data](http://www.ncbi.nlm.nih.gov/pubmed/25320792) function `makeGraph` plus yet to be added RIPMMARC

* [SCCAN relating neuroimaging and cognitive batteries](http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3911786/) function `sparseDecom2`

* [Sparse regression with manifold smoothness constraints](http://www.ncbi.nlm.nih.gov/pubmed/24683960) function `sparseRegression`

* [Prior-based eigenanatomy](http://www.ncbi.nlm.nih.gov/pubmed/24852460) function `sparseDecom`


## Installation from source

**[drat](https://github.com/cran/drat)**
```
drat::addRepo("ANTs-R")
install.packages("ANTsR")
```
Thanks to [zarquon42b](https://github.com/zarquon42b).


**with devtools in R**
```
library( devtools )
install_github("stnava/cmaker")
install_github("stnava/ITKR")
install_github("stnava/ANTsR")
```

this assumes you have [git](http://git-scm.com/) installed / accessible in your environment, as well as a compiler, preferably `clang`.

windows users should see [Rtools](http://cran.r-project.org/bin/windows/Rtools/) and maybe, also, [installr](https://github.com/talgalili/installr) for assistance in setting up their environment for building (must have a compiler too)


**from command line**

First, clone the repository:
```sh
$ git clone https://github.com/stnava/ITKR.git
$ git clone https://github.com/stnava/ANTsR.git
```

Install the package as follows:
```sh
$ R CMD INSTALL ITKR
$ R CMD INSTALL ANTsR
```

### R dependencies
You may need to install R packages that ANTsR requires. For example:
```
mydeps <- c( "Rcpp", "tools", "methods", "magrittr" )
install.packages( pkgs = mydeps, dependencies = TRUE )
```
These dependencies are subject to change until development is stable.
You can gain additional functionality by installing packages that
are listed in the [`DESCRIPTION` file](https://github.com/stnava/ANTsR/blob/master/DESCRIPTION) under `Suggests`.

The [`travis.yml` file](https://github.com/stnava/ANTsR/blob/master/.travis.yml) also shows a way to install from Linux command line.

## Usage
Load the package:
```
library(ANTsR)
```
List the available functions in the namespace ANTsR:
```
ANTsR::<double-tab>
```

Call help on a function via ?functionName or see function arguments
via  `args(functionName)`

## Overview of ANTsR functionality and useful tools

If nothing else, ANTsR makes it easy to read and write medical images
and to map them into a format compatible with R.

**Read, write, access an image**
```
mnifilename<-getANTsRData("mni")
img<-antsImageRead(mnifilename)
antsImageWrite(img,mnifilename)
antsGetSpacing(img)
antsGetDirection(img)
antsGetOrigin(img)
print(antsGetPixels(img,50,60,44))
print(max(img))
```

**Index an image with a label**
```
gaussimg<-array( data=rnorm(125), dim=c(5,5,5))
arrayimg<-array( data=(1:125), dim=c(5,5,5))
img<-as.antsImage( arrayimg )
print( max(img) )
print( mean(img[ img > 50  ]))
print( max(img[ img >= 50 & img <= 99  ]))
print( mean( gaussimg[ img >= 50 & img <= 99  ]) )
```

**Convert a 4D image to a matrix**
```
gaussimg<-array( data=rnorm(125*10), dim=c(5,5,5,10))
gaussimg<-as.antsImage(gaussimg)
print(dim(gaussimg))
mask<-getAverageOfTimeSeries( gaussimg )
voxelselect <- mask < 0
mask[ voxelselect  ]<-0
mask[ !voxelselect  ]<-1
gmat<-timeseries2matrix( gaussimg, mask )
print(dim(gmat))
```

**Convert a list of images to a matrix**

```
nimages<-100
ilist<-list()
for ( i in 1:nimages )
{
  simimg<-makeImage( c(50,50) , rnorm(2500) )
  simimg<-smoothImage(simimg,1.5)
  ilist[i]<-simimg
}
# get a mask from the first image
mask<-getMask( ilist[[1]],
  lowThresh=mean(ilist[[1]]), cleanup=TRUE )
mat<-imageListToMatrix( ilist, mask )
print(dim(mat))
```

**Do fast statistics on a big matrix**

Once we have a matrix representation of our population, we
might run a quick voxel-wise regression within the mask.  
Then we look at some summary statistics.
```
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
```


**Write out a statistical map**

We might also write out the images so that we can save them for later
or look at them with other software.
```
agebetas<-makeImage( mask , mdli$beta.t[1,] )
antsImageWrite( agebetas, tempfile(fileext ='.nii.gz') )
```

**Neighborhood operations**

Images neighborhoods contain rich shape and texture information.  
We can extract neighborhoods for further analysis at a given scale.
```
mnit<-getANTsRData("mni")
mnit<-antsImageRead(mnit)
mnit <- resampleImage( mnit , rep(4, mnit@dimension) )
mask2<-getMask(mnit,lowThresh=mean(mnit),cleanup=TRUE)
radius <- rep(2,mnit@dimension)
mat2<-getNeighborhoodMatrix(mnit, mask2, radius,
  physical.coordinates = FALSE,
  boundary.condition = "mean" )
```
The `boundary.condition` says how to treat data that is outside of the mask
or the image boundaries.  Here, we replace this data with the mean
in-mask value of the local neighborhood.

**Eigenanatomy & SCCAN**

Images often have many voxels ($p$-voxels) and,
in medical applications, this means that $p>n$ or even $p>>n$, where $n$ is
the number of subjects.
Therefore, we often want to "intelligently" reduce the dimensionality of the
data.  However, we want to retain spatial locality. This is the point of
"eigenanatomy" which is a variation of sparse PCA that uses (optionally)
biologically-motivated smoothness, locality or sparsity constraints.
```
# assume you ran the population example above
eanat<-sparseDecom( mat, mask, 0.2, 5, cthresh=2, its=2 )
eseg<-eigSeg(mask,eanat$eig,F)
jeanat<-joinEigenanatomy(mat,mask,eanat$eig, c(0.1))
eseg2<-eigSeg(mask,jeanat$fusedlist,F)
```
The parameters for the example above are set for fast processing.
You can see our paper for some theory on these methods[@Kandel2014a].

More information is available within the examples that can be seen within
the help for `sparseDecom`, `sparseDecom2` and the helper function
`initializeEigenanatomy`. You might also
see the [sccan tutorial](http://stnava.github.io/sccanTutorial/).

**Other useful tools**
```
?iMath
?ThresholdImage
?quantifyCBF
?antsPreprocessfMRI
?aslPerfusion
?computeDVARS
?getROIValues
?hemodynamicRF
?inspectImageData3D
?makeGraph
?matrixToImages
?antsRegistration
?plotPrettyGraph
?plotBasicNetwork
?getTemplateCoordinates
?antsSet*
```

Parts of `ImageMath` from ANTs are accessible via
```
?iMath
```

for more fMRI focused tools, see [RKRNS](http://stnava.github.io/RKRNS/) and its
github site [github RKRNS](https://github.com/stnava/RKRNS).

A good visualization alternative is [antsSurf](https://github.com/stnava/antsSurf).

## Direct access to ANTs tools

Alternatively, one can use any function in the namespace by providing arguments exactly same as one provides to the corresponding command-line version.

For example, to call the antsRegistration routine:
```
ANTsR::antsRegistration( "-d", "2", "-m", "mi[r16slice.nii.gz,r64slice.nii.gz,1,20,Regular,0.05]", "-t", "affine[1.0]", "-c", "2100x1200x1200x0", "-s", "3x2x1x0", "-f", "4x3x2x1","-u", "1", "-o", "[xtest,xtest.nii.gz,xtest_inv.nii.gz]" )

ANTsR::antsRegistration( "-d", "2", "-m", "mi[r16slice.nii.gz,r64slice.nii.gz,1,20,Regular,0.05]", "-t", "affine[1.0]", "-c", "2100x1200x1200x0", "-s", "3x2x1x0", "-f", "4x3x2x1", "-m", "cc[r16slice.nii.gz,r64slice.nii.gz,1,4]", "-t", "syn[5.0,3,0.0]", "-i", "100x100x0", "-s", "2x1x0", "-f", "3x2x1", "-u", "1", "-o", "[xtest,xtest.nii.gz,xtest_inv.nii.gz]" )
```


# Release notes

## 0.3.1

* WIP: iMath improvements

* WIP: ASL pipeline fuctionality

* BUG: Fixed image indexing bug

* BUG: plot.antsImage improvements

* ENH: more antsRegistration options

* ENH: geoSeg

* ENH: JointLabelFusion and JointIntensityFusion

* ENH: Enable negating images

* ENH: weingarten curvature

* ENH: antsApplyTransformsToPoints with example

* ENH: renormalizeProbabilityImages

* ENH: Suppress output from imageWrite.

## 0.3.0

First official release.
