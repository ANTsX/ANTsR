---
title: "iMath (common mathematical operations inside ANTsR)"
author: "Brian B. Avants, Arman Eshaghi"
date: "`r Sys.Date()`"
output: rmarkdown::html_vignette
vignette: >
    %\VignetteEngine{knitr::rmarkdown}
    %\VignetteIndexEntry{Transformations and statistical representations for images in R}
    \usepackage[utf8]{inputenc}
---

```{r, echo = FALSE, message = FALSE}
knitr::opts <- chunk$set(collapse = T, comment = "#>")
library(ANTsR)
```
## Basic operations
`iMath` is a versatile tool that wraps `ImageMath` from ANTs software. For basic 
arithmetics (e.g., add, subtract), while it's possible to use iMath, usually it 
is easier to use `antsImage` class capabilities.

  * Basic arithmetics with `antsImageClass` is easier, and is similar to 
  array operations in R:

    ```{r}
    fi  <- antsImageRead(getANTsRData("r16"), 2)
    sumfi <- fi + fi
    mulfi <- fi * 2
    #logarithm of image where non-zero
    logfi <- log(fi[fi>0])
    expfi <- exp(fi[fi>0)
    divfi <- sumfi / mulfi
    ```

##Dilation or erosion of masks and images
The typical rule for dilation and erosion of images in ANTsR is minimum 
for erosion and maximum for dilation (ITK rules).
  * Grayscale dilation of an image (compare it with binary dilation below):
  
    ```{r}
    #dilating by a radius of 3 voxels
    GDdilated <- iMath(fi, "GD", 3)
    #to see what dilation has done
    invisible(plot(GDdilated))
    #to see the difference a dilation can make
    invisible(plot(GDdilated - fi))
    ```

  * Grayscale erosion of an image (compare it with binary erosion below).

    ```{r}
    erosion <- iMath(fi, "GE", 3)
    invisible(plot(erosion))

    ```

  * Morphological dilation works on binary images, where it makes white 
  regions bigger:

    ```{r}
    mask <- getMask(fi)
    MD <- iMath(mask, "MD", 3)
    #to see the difference it made
    invisible(plot(MD - mask))
    ```

  * Morphological erosion makes white regions of a binary mask smaller.

    ```{r}
    ME <- iMath(mask, "ME", 3)
    invisible(plot(ME))
    ```

##Padding or cropping an image 

`PadImage` is a function for add/remove voxels to/from images.

* Positive numbers will pad image in each direction. So for example using 2
  will make an image with 256 voxels in X and Y direction, 260.
  
    ```{r}
    padded <- iMath(fi, "PadImage", 2)
    #compare padded image dimensions with the original dimensions
    dim(fi)
    dim(padded)
    ```

  * Negative numbers will crop the image in each direciton. For example using -2
  will convert a 2-dimensional image with 256 voxels in X and Y direction to 
  252 voxels in each dimension.
  
    ```{r}
    cropped <- iMath(fi, "PadImage", -2)
    #compare cropped image with the original one
    dim(fi)
    dim(cropped)
    ```

##Distance map
  
  * `MaurerDistance` calculates the Euclidean distance transform of a binary
  image in time linear with total number of voxels. It assumes that inside 
  the binary image has negative distance, while outside of the binary mask
  has positve distance. Distance maps could be used for example, in shape 
  analysis, skletonizing or path finding.

    ```{r}
    distanceMap <- iMath(mask, 'MaurerDistance')
    invisible(plot(distanceMap))
    ```
 * 


