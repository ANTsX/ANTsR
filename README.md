

<!--
![ants brain](http://i.imgur.com/I2VNWpA.png)
![ants edie](http://i.imgur.com/DcV1NVT.png)
![ants babe](http://i.imgur.com/gwoxI5M.png)
-->
# Advanced Normalization Tools in R

![Downloads](https://img.shields.io/github/downloads/antsx/antsr/total) 
[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/stnava/ANTsRDocker/master)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-v2.0%20adopted-ff69b4.svg)](code_of_conduct.md)

<br />

ANTsR is an R package which wraps the well-established C++ biomedical image processing library [ANTs](https://github.com/ANTsX/ANTs). It includes efficient reading and writing of medical images, algorithms for registration, segmentation, and statistical learning, as well as functions to create publication-ready visualizations.

If you are looking to train deep learning models on your medical images in R, you might be interested in [ANTsRNet](https://github.com/ANTsX/ANTsPy) which provides pre-trained models and utilities for training deep learning models on medical images.

<br />

## Installation

Currently, the only way to install ANTsR is from the pre-compiled binaries or from source. We are actively working on getting ANTsR onto CRAN / Bioconductor / etc. 

### Binaries

The fastest way to install ANTsR is from pre-compile binaries. Download the relevant version and run this from the command-line:

```
R CMD INSTALL ANTsR_*.tgz
```

### From source using devtools

To install ANTsR from source, you can use devtools to install the latest version of the code directly form GitHub:

```R
devtools::install_github('ANTsX/ANTsR')
```

### From source using R CMD INSTALL

Alternatively, you can clone and install ANTsR and its two main dependencies (ANTsRCore, ITKR) and then install them as you would traditional R source packages.

First, clone the repositories:

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

<br />

## Developer notes

### Maintainer

[Brian B. Avants](http://stnava.github.io/)

### License

Apache License 2.0

<br />

## Relevant links

[ANTsR Instagram](https://www.instagram.com/antsrx/)

[DOI 10.17605/osf.io/bsq5v](https://osf.io/bsq5v/)

Reference manual:	[ANTsR](https://antsx.github.io/ANTsR/reference/index.html)

[Vignettes](https://antsx.github.io/ANTsR/articles/):

Wiki: [Notes and work in progress examples](https://github.com/ANTsX/ANTsR/wiki)

Package source:	[from github](https://github.com/ANTsX/ANTsR/zipball/master)

Binaries:	[here](http://github.com/ANTsX/ANTsR/releases/)

*Windows* installation option [here](https://github.com/ANTsX/ANTsR/wiki/Installing-ANTsR-on-Windows-10-(using-WSL2))

**Suggested packages** [https://github.com/stnava/ANTsRDocker/blob/master/install.R](https://github.com/stnava/ANTsRDocker/blob/master/install.R)

<br />

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

<br />

## Contributing

If you have a question or need help solving a problem, please create an issue on GitHub. If you want to contribute code or have feature request, creating an issue on GitHub is also the best place to start.
