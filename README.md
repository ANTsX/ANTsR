# ANTsR

[![Build Status](https://travis-ci.org/stnava/ANTsR.png?branch=master)](https://travis-ci.org/stnava/ANTsR)

An R package providing [ANTs](http://stnava.github.io/ANTs/) and [ITK](https://github.com/InsightSoftwareConsortium/ITK) features in R.

Current Authors and Contributors:  Brian B. Avants, Benjamin M. Kandel, Jeff T. Duda, Philip A. Cook, Nicholas J. Tustison

Original Authors: Shrinidhi KL,  Brian B. Avants

## Easiest installation approach (from within R)
```
library( devtools )
install_github("stnava/ANTsR")
```

this assumes you have [cmake](http://www.cmake.org/download/) installed / accessible in your environment

windows users should see [Rtools](http://cran.r-project.org/bin/windows/Rtools/) and maybe, also, [installr](https://github.com/talgalili/installr) for assistance in setting up their environment for building (must have a compiler too)

## ANTsR manual and releases

[R-style manual and releases](https://github.com/stnava/ANTsR/releases) are new
so let us know of any [issues](https://github.com/stnava/ANTsR/issueshttps://github.com/stnava/ANTsR/issues) ...

## Installation from source
see [install from source](http://stnava.github.io/software/2014/01/08/antsr/)

First, clone the repository:
```sh
$ git clone https://github.com/stnava/ANTsR.git
```

Install the package as follows:
```sh
$ R CMD INSTALL ANTsR
```

## Installation using Homebrew

* Install homebrew
```
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```

* Install cmake
```
brew install cmake
```

* Install zlib and wget, if necessary
```
brew install wget
```
```
brew tap homebrew/dupes
```
```
brew install zlib
```

* Edit zlib path in ANTsR/src/ANTS/ANTS-build/CMakeCache.txt file:  
```
ZLIB_INCLUDE_DIR:PATH=/usr/local/Cellar/zlib/1.2.8/include
```

* Possible errors

```
Error: make: llvm-g++-4.2: No such file or directory, Error: /bin/sh: llvm-g++-4.2: command not found
```
Solution:  In file: /Library/Frameworks/R.framework/Versions/3.0/Resources/etc/Makeconf

Find: llvm-g++-4.2

Replace with: g++

```
Warning: cmake-3.1.0 already installed, it's just not linked
```
Solution:
```
brew link --overwrite cmake
```



### Binaries
These are still under development; use at your own discretion.

 * [Mac OS X](https://dl.dropboxusercontent.com/u/9717050/ANTsR_osx_1.0.tgz)
 * [RHEL](https://dl.dropboxusercontent.com/u/9717050/ANTsR_1.0_R_x86_64-redhat-linux-gnu.tar.gz)
 * [Other Linux](https://dl.dropboxusercontent.com/u/9717050/ANTsR_1.0_R_x86_64-pc-linux-gnu.tar.gz)


### R dependencies
You may need to install R packages that ANTsR requires. For example:
```
install.packages(pkgs = c("Rcpp", "tools", "methods"), dependencies = TRUE);
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
dimension<-3
img<-antsImageRead(mnifilename,dimension)
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
  simimg<-array( data=rnorm(2500), dim=c(50,50))
  simimg<-as.antsImage( simimg )
  SmoothImage(simimg,1.5,simimg)
  ilist[i]<-simimg
}
# get a mask from the first image
mask<-getMask( ilist[[1]],
  lowThresh=mean(ilist[[1]]), cleanup=TRUE )
mat<-imageListToMatrix( ilist, mask )
print(dim(mat))
```

**Do fast statistics on a big matrix**
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
```
agebetas<-antsImageClone( mask )
agebetas[ mask == 1 ]<-mdli$beta.t[1,]
antsImageWrite( agebetas, 'agebetas.nii.gz' )
```

**Neighborhood operations**

```
mnit<-getANTsRData("mni")
mnit<-antsImageRead(mnit,3)
mnit <- resampleImage( mnit , rep(4, mnit@dimension) )
mask<-getMask(mnit,lowThresh=mean(mnit),cleanup=TRUE)
radius <- rep(2,mnit@dimension)
mat<-antsGetNeighborhoodMatrix(mnit,mask,radius,
  physical.coordinates = FALSE,
  boundary.condition = "mean" )
```


**Eigenanatomy & SCCAN**
```
# assume you ran the neighborhood example above
eanat<-sparseDecom( mat, mask, 0.05, 20, cthresh=250 )
eseg<-eigSeg(mask,eanat$eig,F)
jeanat<-joinEigenanatomy(mat,mask,eanat$eig, c(0.1))
# see our paper in the journal "methods"
eseg2<-eigSeg(mask,jeanat$fusedlist,F)
```

```
?sparseDecom
?sparseDecom2
?initializeEigenanatomy
```
See [sccan tutorial](http://stnava.github.io/sccanTutorial/)

**Other useful tools**
```
?ImageMath
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
?plotANTsImage
?antsRegistration
?plotPrettyGraph
?plotBasicNetwork
?getTemplateCoordinates
?antsSet*
```

All of `ImageMath` from ANTs is accessible too
```
ImageMath(3,threshimg,'ClusterThresholdVariate',threshimg,mask,5)
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
