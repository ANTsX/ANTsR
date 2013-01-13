=========================
Getting started with ANTsR
=========================

Clone or Pull ANTsR from this repository as follows:

      $ git clone https://github.com/stnava/ANTsR.git

Install the package as follows:

      $ R CMD INSTALL ANTsR

NOTE: If you see errors like "Undefined symbols for architecture x86_64" use

      $ R --arch=x86_64 CMD INSTALL  ANTsR

The installation may fail if ANTsR dependencies are not installed.
These are listed in:

      $  ANTsR/DESCRIPTION

which will include: 

      Depends: Rcpp (>= 0.9.7.2) , methods, signal , parallel , timeSeries , mFilter , MASS , doParallel, robust, magic, knitr, pixmap, rgl, misc3d

In R, you can do:   
    
     R>  install.packages( 'signal', dependencies=TRUE ) 

Once installation is successful, the ANTsR package can be used in R as follows:

Load the package

> library( ANTsR )

List the available functions in the namespace ANTsR

> ANTsR::<double-tab>

Use any function in the namespace by providing arguments exactly same as you provide to their command-line version.

For example, call the antsImageRead routine

>  antsimage<-antsImageRead( "r16slice.nii.gz", 2 )

.. image:: _static/ANTSWarpImageMultiTransform.png
  :width: 600 px


