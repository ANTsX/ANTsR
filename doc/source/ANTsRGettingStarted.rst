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


.. sourcecode:: r
    

    tfn <- getANTsRData("r16")  # download example data
    antsimage <- antsImageRead(tfn, 2)
    imgn3 <- antsImageClone(antsimage)
    N3BiasFieldCorrection(list(antsimage@dimension, antsimage, imgn3, "4"))




User Installation Method: from within R
---------------------------------------------


We depend on CMake_.

.. _CMake: http://www.cmake.org/cmake/resources/software.html

If you have CMake installed, then open R and do:

.. sourcecode:: r
    

    install.packages(c("devtools", "MASS", "Rcpp", "methods", "signal", "parallel", 
        "timeSeries", "mFilter", "MASS", "robust", "magic", "knitr", "pixmap", "rgl", 
        "misc3d", "lme4"))
    library(devtools)
    install_github("ANTsR", "stnava")  # all this will take some time




Developer Installation Method: from command line
---------------------------------------------------------------------------------------------------------------------------------------

See the libraries and CMake_ listed above and install them.

Clone or Pull ANTsR from this repository as follows:

      $ git clone git@github.com:stnava/ANTsR.git

From the parent directory of the new ANTsR directory, install the package as follows:

      $ R CMD INSTALL ANTsR

NOTE: If you see errors like "Undefined symbols for architecture x86_64" use

      $ R --arch=x86_64 CMD INSTALL  ANTsR

The installation may fail if ANTsR dependencies are not installed.
These are listed in:

      $  ANTsR/DESCRIPTION

which will include: 

      Depends: Rcpp (>= 0.9.7.2) , methods, signal , parallel , timeSeries , mFilter , MASS , doParallel, robust, magic, knitr, pixmap, rgl, misc3d

In R, you can do:   
    
     R>  install.packages( "signal", dependencies=TRUE ) 

to get packages.

.. image:: _static/ANTSWarpImageMultiTransform.png
  :width: 600 px


