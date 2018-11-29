[ANTsR Instagram](https://www.instagram.com/antsrx/)

<!--
![ants brain](http://i.imgur.com/I2VNWpA.png)
![ants edie](http://i.imgur.com/DcV1NVT.png)
![ants babe](http://i.imgur.com/gwoxI5M.png)
-->
# ANTsR

[![Travis Build Status](https://travis-ci.org/ANTsX/ANTsR.png?branch=master)](https://travis-ci.org/ANTsX/ANTsR)

[DOI 10.17605/osf.io/bsq5v](https://osf.io/bsq5v/)

muschellij2 badges:
[![Travis Build Status](https://travis-ci.org/muschellij2/ANTsR.png?branch=master)](https://travis-ci.org/muschellij2/ANTsR) [![Coverage Status](https://coveralls.io/repos/muschellij2/ANTsR/badge.svg)](https://coveralls.io/r/muschellij2/ANTsR)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/muschellij2/ANTsR?branch=master&svg=true)](https://ci.appveyor.com/project/muschellij2/ANTsR)


<!-- commenting out because we moved to antsx
[![Coverage Status](https://coveralls.io/repos/stnava/ANTsR/badge.svg)](https://coveralls.io/r/stnava/ANTsR)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/stnava/ANTsR?branch=master&svg=true)](https://ci.appveyor.com/project/stnava/ANTsR) -->

A package providing [ANTs](https://github.com/ANTsX/ANTs) features in R as well as imaging-specific data representations, spatially regularized dimensionality reduction and segmentation tools.  See also the **Neuroconductor** [site](https://www.neuroconductor.org/package/details/ANTsR).

## Description

License: Apache License 2.0

Maintainer:	[Brian B. Avants](http://stnava.github.io/)

URL:	[homepage](https://antsx.github.io/ANTsR/)

BugReports: [github issues](http://github.com/ANTsX/ANTsR/issues)

NeedsCompilation:	yes

Travis checks:	[ANTsR results](https://travis-ci.org/ANTsX/ANTsR)

Documentation: [https://antsx.github.io/ANTsR/](https://antsx.github.io/ANTsR/).

## Preview ANTsR on the cloud

Here: [https://rstudio.cloud/project/38492](https://rstudio.cloud/project/38492)

Then try:

```
.libPaths('/cloud/project/RL/')
library(ANTsR)
?plot.antsImage
?antsRegistration
```

To learn more, open and run the Rmd files in the directory "/cloud/project/RL/ANTsR/doc"

Note: as of this writing, memory is very limited on this cloud preview so some examples may not run successfully.

## Downloads

Reference manual:	[ANTsR](https://antsx.github.io/ANTsR/reference/index.html)

[Vignettes](https://antsx.github.io/ANTsR/articles/):

Wiki: [Notes and work in progress examples](https://github.com/ANTsX/ANTsR/wiki)

Package source:	[from github](https://github.com/ANTsX/ANTsR/zipball/master)

Binaries:	[here](http://github.com/ANTsX/ANTsR/releases/)

*Windows* installation option [here](https://github.com/ANTsX/ANTsR/wiki/Installing-ANTsR-in-Windows-10-(along-with-FSL,-Rstudio,-Freesurfer,-etc).)

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

* [Corrective learning for segmentation](http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3049832/) functions `segmentationRefinement.train` and `segmentationRefinement.predict`.

* [LINDA: automated segmentation of stroke lesions](https://www.ncbi.nlm.nih.gov/pubmed/26756101) function `mrvnrfs`.

* [LESYMAP: lesion to symptom mapping](https://www.ncbi.nlm.nih.gov/pubmed/28882479) function `sparseDecom2`.

* [ANTsRNet](https://github.com/ntustison/ANTsRNet)  A growing collection of well-known deep learning
architectures ported to the R language.
    * Image segmentation
        * U-Net (2-D)
        * V-Net (3-D)
    * Image classification  
        * AlexNet (2-D, 3-D)
        * Vgg16/Vgg19 (2-D, 3-D)
        * ResNet/ResNeXt (2-D, 3-D)
        * GoogLeNet (2-D)
        * DenseNet (2-D, 3-D)
    * Object detection
        * Single Shot MultiBox Detector (2-D, 3-D)

## Installation from source

Please read this entire section before choosing which method you prefer.

In general, these **assume** you have [git](http://git-scm.com/) installed / accessible in your environment, as well as a compiler, preferably `clang`.  you may also need [cmake](http://www.cmake.org) if you do/can not install `cmaker`.

Windows users should see [Rtools](http://cran.r-project.org/bin/windows/Rtools/) and maybe, also, [installr](https://github.com/talgalili/installr) for assistance in setting up their environment for building (must have a compiler too).  To my knowledge, there are no recorded instances of ANTsR being installed on Windows.  If someone does so, we would like to know.

You will need to install R packages that ANTsR requires. Minimally:
**Install ITKR and ANTsRCore** [here](https://github.com/stnava/ITKR/releases) and [here](https://github.com/ANTsX/ANTsRCore/releases) then do:
```
mydeps <- c( "Rcpp", "RcppEigen", "magrittr", "rsvd", "magic", "psych" )
install.packages( pkgs = mydeps, dependencies = TRUE )
```
You can gain additional functionality by installing packages that
are listed in the [`DESCRIPTION` file](https://github.com/ANTsX/ANTsR/blob/master/DESCRIPTION) under `Suggests`.
A complete list of recommended ancillary packages [here](https://github.com/ANTsX/ANTsR/wiki/ANTsR-Dependencies-for-(close-to)-full-functionality).

**Method 1: with devtools in R**
```
library( devtools )
# install_github("stnava/cmaker") # if you do not have cmake
install_github("stnava/ANTsR")
```

**Method 2: from command line (most traditional method)**

Assumes git, cmake and compilers are available in your environment (as above).

First, clone the repository:
```sh
$ git clone https://github.com/stnava/ITKR.git
$ git clone https://github.com/ANTsX/ANTsRCore.git
$ git clone https://github.com/ANTsX/ANTsR.git
```

Install the package as follows:
```sh
$ R CMD INSTALL ITKR
$ R CMD INSTALL ANTsRCore
$ R CMD INSTALL ANTsR
```

The [`travis.yml` file](https://github.com/ANTsX/ANTsR/blob/master/.travis.yml) also shows a way to install from Linux command line.


**Method 3: from binaries**

Note that version numbers will change over time.

```
wget https://github.com/stnava/ITKR/releases/download/latest/ITKR_0.4.12_R_x86_64-pc-linux-gnu.tar.gz
R CMD INSTALL ITKR_0.4.12_R_x86_64-pc-linux-gnu.tar.gz
wget https://github.com/ANTsX/ANTsRCore/releases/download/v0.4.2.1/ANTsRCore_0.4.2.1_R_x86_64-pc-linux-gnu.tar.gz
R CMD INSTALL ANTsRCore_0.4.2.1_R_x86_64-pc-linux-gnu.tar.gz
wget https://github.com/ANTsX/ANTsR/releases/download/latest/ANTsR_0.6_R_x86_64-pc-linux-gnu.tar.gz
R CMD INSTALL ANTsR_0.6_R_x86_64-pc-linux-gnu.tar.gz
```


**Method 4: platform independent via docker and kitematic**

This is a beta operation that is in flux but may be convenient for some users.

* based on [this approach](https://github.com/rocker-org/rocker/wiki/Using-the-RStudio-image)

* create a docker username

* download and install kitematic

* login to kitematic with your docker username

* search for `antsr` in the kitematic repository search bar

* create a new container from the `stnava` version of `antsr`

* start the container

* type the "access url" address into a browser to run rstudio with antsr.
it should be something like `http://192.168.99.100:32989/`

* you can also add your home folders to the container instance by adjusting
the "volumes" option under the settings tab.  then you can access local data.

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


# Tagging a beta release

```
git tag -d beta
git push origin :refs/tags/beta
git tag beta
git push  --tags origin
```


# Release notes

More like *development highlights*, as opposed to *release notes*.  See `git log` for the complete history.  We try to follow [these versioning recommendations for R packages](http://yihui.name/en/2013/06/r-package-versioning/).  Under these guidelines, only `major.minor` is an official release.

## 0.4.0

* ENH: better compilation and release style.
* ENH: return boolean same size as image
* ENH: improved decomposition methods
* ENH: easier to use antsrSurf and antsrVol

## 0.3.3

* ENH: spare distance matrix, multi scale svd

## 0.3.2

* ENH: added domainImg option to plot.antsImage which can standardize plot.

* COMP: test for DVCL define requirement to improve clang and gcc compilations

* WIP: transform objects can be created on the fly and manipulated thanks to jeff duda

* ENH: automation for eigenanatomy

* ENH: reworked SCCAN and eanat

* ENH: mrvnrfs

* ENH: resting state Vignette

* DOC: clarify/extend antsApplyTransforms

* ENH: multidimensional images

* STYLE: iMath not ImageMath in ANTsR

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
