# ANTsR
An R package providing ITK features in R.

Authors: Shrinidhi KL,  Avants BB 

## easiest approach ( from within R )
library( devtools )
install_github("stnava/ANTsR") 

# assumes you have cmake installed / accessible in your environment

## Installation
see [install from source](http://stnava.github.io/software/2014/01/08/antsr/)

### From source
First, clone the repository:
```sh
$ git clone https://github.com/stnava/ANTsR.git
```

Install the package as follows:
```sh
$ R CMD INSTALL ANTsR
```

NOTE: If you see errors like "``Undefined symbols for architecture x86_64``" use:
```sh
$ R --arch=x86_64 CMD INSTALL ANTsR
```

### Binaries
These are still under development; use at your own discretion.

 * [Mac OS X](https://dl.dropboxusercontent.com/u/9717050/ANTsR_osx_1.0.tgz)
 * [RHEL](https://dl.dropboxusercontent.com/u/9717050/ANTsR_1.0_R_x86_64-redhat-linux-gnu.tar.gz)
 * [Other Linux](https://dl.dropboxusercontent.com/u/9717050/ANTsR_1.0_R_x86_64-pc-linux-gnu.tar.gz)


### R dependencies
You will probably have to install the R packages that ANTsR requires. For example:
```
install.packages(pkgs = c("Rcpp", "signal", "timeSeries", "mFilter", "doParallel", "robust", "magic", "knitr", "pixmap", "rgl", "misc3d"), dependencies = TRUE);
```
These dependencies are subject to change until development is stable.

## Usage
Load the package:
```
library(ANTsR)
```
List the available functions in the namespace ANTsR:
```
ANTsR::<double-tab>
```

Call help on a function via ?functionName

Alternatively, one can use any function in the namespace by providing arguments exactly same as one provides to the corresponding command-line version.

For example, to call the antsRegistration routine:
```
ANTsR::antsRegistration( "-d", "2", "-m", "mi[r16slice.nii.gz,r64slice.nii.gz,1,20,Regular,0.05]", "-t", "affine[1.0]", "-c", "2100x1200x1200x0", "-s", "3x2x1x0", "-f", "4x3x2x1","-u", "1", "-o", "[xtest,xtest.nii.gz,xtest_inv.nii.gz]" )

ANTsR::antsRegistration( "-d", "2", "-m", "mi[r16slice.nii.gz,r64slice.nii.gz,1,20,Regular,0.05]", "-t", "affine[1.0]", "-c", "2100x1200x1200x0", "-s", "3x2x1x0", "-f", "4x3x2x1", "-m", "cc[r16slice.nii.gz,r64slice.nii.gz,1,4]", "-t", "syn[5.0,3,0.0]", "-i", "100x100x0", "-s", "2x1x0", "-f", "3x2x1", "-u", "1", "-o", "[xtest,xtest.nii.gz,xtest_inv.nii.gz]" )
```
