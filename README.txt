Project: ANTsR
Author: Shrinidhi KL
Description: R package providing ITK features in R
Usage:
Clone or Pull ANTsR from this repository as follows:

      $ git clone https://shrinidhikl@github.com/shrinidhikl/ANTsR.git

Install the package as follows:

      $ R CMD INSTALL [--configure-vars="ANTS_DIR=<path-to-ANTS-build>"] [--configure-vars="MAKE_JOBS=<number-of-jobs>"] ANTsR/

In order to provide the optional ANTS_DIR, make sure you have built the ANTS sources from the repository:
git://advants.git.sourceforge.net/gitroot/advants/advants
with the commit tag:
0ca76d70dc8c69ee5d44506eb15169355838ecfd
Other versions of ANTS have not been tested.

A general rule of thumb for MAKE_JOBS is to provide a number equal to the number of hardware execution threads on your computer.

The installed package can be used in R as follows:
Load the package
> library( ANTsR )
List the available functions in the namespace ANTsR
> ANTsR::<double-tab>
Use any function in the namespace by providing arguments exactly same as you provide to their command-line version.
For example, call the antsRegistration routine
> ANTsR::antsRegistration( "-d", "2", "-m", "mi[r16slice.nii.gz,r64slice.nii.gz,1,20,Regular,0.05]", "-t", "affine[1.0]", "-i", "2100x1200x1200x0", "-s", "3x2x1x0", "-f", "4x3x2x1", "-m", "cc[r16slice.nii.gz,r64slice.nii.gz,1,4]", "-t", "syn[5.0,3,0.0]", "-i", "100x100x0", "-s", "2x1x0", "-f", "3x2x1", "-u", "1", "-o", "[xtest,xtest.nii.gz,xtest_inv.nii.gz]" )


