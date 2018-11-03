## ----global options, include=FALSE---------------------------------------
library( knitr )
knitr::opts_chunk$set(collapse = T, comment = "#>")
library(ANTsR)

## ----basic---------------------------------------------------------------
    fi  <- antsImageRead(getANTsRData("r16"), 2)
    sumfi <- fi + fi
    mulfi <- fi * 2
    #logarithm of image where non-zero
    logfi <- log(fi[fi>0])
    expfi <- exp(fi[fi>0])
    divfi <- sumfi / mulfi

## ----greydilate----------------------------------------------------------
#dilating by a radius of 3 voxels
GDdilated <- iMath(fi,"GD", 3)
#to see what dilation has done
invisible(plot(GDdilated))
#to see the difference a dilation can make
invisible(plot(GDdilated - fi))

## ----greyerode-----------------------------------------------------------
erosion <- iMath(fi,"GE", 3)
invisible(plot(erosion))

## ----dilate--------------------------------------------------------------
mask <- getMask(fi)
MD <- iMath(mask, "MD", 3)
#to see the difference it made
invisible(plot(MD - mask))

## ----erode---------------------------------------------------------------
ME <- iMath(mask, "ME", 3)
invisible(plot(ME))

## ----closer--------------------------------------------------------------
newMask <- iMath(mask, "MC", 4)
invisible(plot(newMask,slices=c(1,1)))

## ----pad_up--------------------------------------------------------------
padded <- iMath(fi, "PadImage", 2)
#compare padded image dimensions with the original dimensions
dim(fi)
dim(padded)

## ----pad_down------------------------------------------------------------
cropped <- iMath(fi, "PadImage", -2)
#compare cropped image with the original one
dim(fi)
dim(cropped)

## ----maurer--------------------------------------------------------------
distanceMap <- iMath(mask, "MaurerDistance")
invisible(plot(distanceMap))

## ----distancemap---------------------------------------------------------
distanceMap <- iMath(mask, "D")
invisible(plot(distanceMap))

## ----pm------------------------------------------------------------------
denoised <- iMath(fi, "PeronaMalik", 10, 0.5)
invisible(plot(denoised))
# to see what the filter has removed
invisible(plot(fi - denoised))

## ----grad----------------------------------------------------------------
  grad <- iMath(fi, "Grad", 1)
  invisible(plot(grad))

## ----lap-----------------------------------------------------------------
  laplacianImage <- iMath(fi, "Laplacian", 1, 1)
  invisible(plot(laplacianImage))

## ------------------------------------------------------------------------
fi<-antsImageRead( getANTsRData("r16") , 2 )
result <- iMath(iMath(fi, "Laplacian", 1), "GD", 3)

## ------------------------------------------------------------------------
require(magrittr)
result <- fi %>% iMath("Laplacian",1)  %>% iMath("GD",3)

## ---- results='asis',echo=FALSE------------------------------------------
data("iMathOps")
kable( iMathOps , caption = "Valid iMath Operations", padding = 0 )

