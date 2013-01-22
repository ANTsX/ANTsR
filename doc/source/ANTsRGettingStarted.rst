==================================================
 Getting started with ANTsR
==================================================

Brief Introduction to using ANTs in R
---------------------------------------------

Once installation is successful, the ANTsR package can be used in R as follows:

Load the package

> library( ANTsR )

List the available functions in the namespace ANTsR

> ANTsR::<double-tab>

Use any function in the core ANTS namespace by providing arguments
almost exactly as in the command-line version.

For example, call the antsImageRead routine and some standard ANTs functions.

   antsimage<-antsImageRead( "r16slice.nii.gz", 2 )

   imgn3<-antsImageClone(antsimage)

   N3BiasFieldCorrection(list(antsimage@dimension,img,imgn3,'4'))

   Atropos( list( d = 2 , a = imgn3 , m = "[0.2,1x1]" , o =  seg_img_uint , c = "[5,0]" , i = "kmeans[3]" , x = mask) )


User Installation Method: from within R
---------------------------------------------

install.packages(c('devtools','MASS','Rcpp','methods','signal','parallel','timeSeries','mFilter','MASS','robust','magic','knitr','pixmap','rgl','misc3d','lme4'))

library( devtools )

install_github('ANTsR','stnava')


Developer Installation Method: from command line
---------------------------------------------------------------------------------------------------------------------------------------

See the libraries listed above and install them.

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

to get packages.

.. image:: _static/ANTSWarpImageMultiTransform.png
  :width: 600 px


