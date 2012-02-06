Project: Ritk
Author: Shrinidhi KL
Description: R package providing ITK features in R
Usage:
Clone or Pull Ritk from this repository.

If you have an ITK build ready to use, install the package using the command:
$ R CMD INSTALL --configure-vars="ITK_DIR=<path-to-itk-build>" Ritk/

If you don't have an ITK build, install the package using the command:
$ R CMD INSTALL Ritk/

The installed package can be used in R as follows:
load the package
> library( Ritk )
call the antsRegistration routine
> Ritk::antsRegistration( "-d", "2", "-m", "mi[r16slice.nii.gz,r64slice.nii.gz,1,20,Regular,0.05]", "-t", "affine[1.0]", "-i", "2100x1200x1200x0", "-s", "3x2x1x0", "-f", "4x3x2x1", "-m", "cc[r16slice.nii.gz,r64slice.nii.gz,1,4]", "-t", "syn[5.0,3,0.0]", "-i", "100x100x0", "-s", "2x1x0", "-f", "3x2x1", "-u", "1", "-o", "[xtest,xtest.nii.gz,xtest_inv.nii.gz]" )


