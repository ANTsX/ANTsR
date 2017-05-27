#' @title Simple Value extractor for antsImage Values
#' @description Takes in a mask and an image and then returns the values
#' 
#' @param x Object of class \code{antsImage}
#' @param mask object to subset the image.  If missing, then all 
#' values of the image are plotted.
#'
#' @return Vector of values
mask_values = function(x, mask) {
  if (!missing(mask)) {
    if (!is.null(mask)) { # need this for Summary
      x = x[ coerce_mask(mask, error = TRUE) ]
    }
  } 
  x = as.numeric(x)
}

#' @title Histogram of Values in an Image
#' @description Computes and displays a histogram of the values
#' of an image with the option for a mask.
#' 
#' @param x Object of class \code{antsImage}
#' @param ... Arguments passed to \code{\link{hist.default}}
#' @param mask object to subset the image.  If missing, then all 
#' values of the image are plotted.
#'
#' @return Output of \code{\link{hist}}
#' @export
#' @examples 
#' img = makeImage(c(10,10),rnorm(100))
#' mask = img > 0
#' hist(img)
#' hist(img, mask = mask)
#' @importFrom graphics hist
hist.antsImage = function(x, ..., mask) {
  x = mask_values(x, mask)
  hist(x, ...)
}


#' @title Sample Quantiles
#' @description Computes sample 
#' quantiles for an image, with the option of a mask.
#' 
#' @param x Object of class \code{antsImage}
#' @param ... Arguments passed to \code{\link{quantile}}
#' @param mask object to subset the image.  If missing, then all 
#' values of the image are used
#'
#' @return Output of \code{\link{quantile}}
#' @export
#' @examples 
#' img = makeImage(c(10,10),rnorm(100))
#' mask = img > 0
#' quantile(img, mask = mask)
#' @importFrom stats quantile
quantile.antsImage = function(x, ..., mask) {
  x = mask_values(x, mask)
  quantile(x, ...)
}

#' @title Density of Values in an Image
#' @description Computes the density of values
#' of an image with the option for a mask.
#'  
#' @param x Object of class \code{antsImage}
#' @param ... Arguments passed to \code{\link{density.default}}
#' @param mask object to subset the image.  If missing, then all 
#' values of the image are plotted.
#'
#' @return Output of \code{\link{density}}
#' @export
#' @examples 
#' img = makeImage(c(10,10),rnorm(100))
#' mask = img > 0
#' density(img, mask = mask)
#' @importFrom stats density
density.antsImage = function(x, ..., mask) {
  x = mask_values(x, mask)
  density(x, ...)
}


#' @title Boxplot of Values in an Image
#' @description Computes the boxplot of values
#' of an image with the option for a mask.
#'  
#' @param x Object of class \code{antsImage}
#' @param ... Arguments passed to \code{\link{boxplot.default}}
#' @param mask object to subset the image.  If missing, then all 
#' values of the image are plotted.
#'
#' @return Output of \code{\link{boxplot}}
#' @export
#' @examples 
#' img = makeImage(c(10,10),rnorm(100))
#' mask = img > 0
#' boxplot(img, mask = mask)
#' @importFrom graphics boxplot
boxplot.antsImage = function(x, ..., mask) {
  x = mask_values(x, mask)
  boxplot(x, ...)
}


#' @title Perform Cut on an image
#' @description Cuts a numeric image into an integer factor,
#'  with the option of a mask.
#' 
#' @param x Object of class \code{antsImage}
#' @param breaks either a numeric vector of two or more unique cut points 
#' or a single number (greater than or equal to 2) 
#' giving the number of intervals into which x is to be cut. 
#' Passed to \code{\link{cut}})
#' @param ... Arguments passed to \code{\link{cut}}
#' @param mask object to subset the image.  If missing, then all 
#' values of the image are used
#'
#' @return Object of class \code{antsImage} with an \code{attribute} of levels
#' @export
#' @examples 
#' img = makeImage(c(10,10),rnorm(100))
#' mask = img > 0
#' cut(img, mask = mask, breaks = 4)
cut.antsImage = function(
  x, 
  breaks,
  ...,
  mask){
  ximg = x
  x = mask_values(x, mask)
  cuts = base::cut(x, breaks = breaks, ...)
  levs = levels(cuts)
  cuts = as.numeric(cuts)
  
  if (!missing(mask)) {
    arr = array(0, dim = dim(ximg))
    m = coerce_mask(mask)
    arr[ m ] = cuts
  } else {
    arr = array(cuts, dim = dim(ximg))
  }
  x = as.antsImage(arr, reference = ximg)
  gc();
  
  attr(x, "levels") = levs
  return(x)
} 
