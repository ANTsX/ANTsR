Project: ANTsR
Author: Shrinidhi KL,  Avants BB 
Description: R package providing ITK features in R
Usage:
Clone or Pull ANTsR from this repository as follows:

      $ git clone https://github.com/stnava/ANTsR.git

Install the package as follows:

      $ R CMD INSTALL ANTsR

NOTE: If you see errors like "Undefined symbols for architecture x86_64" use

      $ R --arch=x86_64 CMD INSTALL  ANTsR

The installed package can be used in R as follows:
Load the package
> library( ANTsR )
List the available functions in the namespace ANTsR
> ANTsR::<double-tab>
Use any function in the namespace by providing arguments exactly same as you provide to their command-line version.
For example, call the antsRegistration routine
> ANTsR::antsRegistration( "-d", "2", "-m", "mi[r16slice.nii.gz,r64slice.nii.gz,1,20,Regular,0.05]", "-t", "affine[1.0]", "-i", "2100x1200x1200x0", "-s", "3x2x1x0", "-f", "4x3x2x1", "-m", "cc[r16slice.nii.gz,r64slice.nii.gz,1,4]", "-t", "syn[5.0,3,0.0]", "-i", "100x100x0", "-s", "2x1x0", "-f", "3x2x1", "-u", "1", "-o", "[xtest,xtest.nii.gz,xtest_inv.nii.gz]" )

# still under development, use at your own discretion
