#' Get a hypercube neighborhood at a voxel
#'
#' Get the values in a local neighborhood of an \code{antsImage}.
#'
#' @param image Image object of S4 class \code{antsImage} to get values from.
#' @param center array of indices for neighborhood center
#' @param kernel either an array of values for neighborhood radius (in voxels) or a binary array of the same dimension as the image, specifying the shape of the neighborhood to extract
#' @param physical.coordinates a logical indicating if voxel indices and
#' offsets should be in voxel or physical coordinates
#' @return a list
#' \itemize{
#'   \item{values}{numeric vector of values}
#'   \item{indices}{matrix providing the coordinates for each value}
#' }
#' @author Duda JT
#' @examples
#' img<-makeImage(c(10,10),rnorm(100))
#' center <- dim(img)/2
#' radius <- rep(3,2)
#' nhlist <- getNeighborhoodAtVoxel(img,center,radius)
#' kernel <- 1*(rnorm(49)>0)
#' dim(kernel) <- c(7,7)
#' randlist <- getNeighborhoodAtVoxel(img,center,kernel)
#' randlist <- getNeighborhoodAtVoxel(img,center,kernel, 
#' physical.coordinates = TRUE)
#' arr = as.array(img)
#' testthat::expect_error(getNeighborhoodAtVoxel(arr,center,kernel), "class")
#' testthat::expect_error(getNeighborhoodAtVoxel(img,as.character(center),kernel), 
#' "center must be")
#' testthat::expect_error(getNeighborhoodAtVoxel(img,center,c(radius, 3)),
#' "kernel must have same") 
#' 
#' @export
getNeighborhoodAtVoxel <- function(image, center, kernel, physical.coordinates = FALSE ) {
  
  image = check_ants(image)
  
  if (!is.antsImage(image)) {
    stop("Input must be of class 'antsImage'")
  }
  
  if ((class(center) != "numeric")) {
    stop("center must be of class 'numeric'")
  }
  
  radius = dim(kernel)
  if ( is.null(radius) ) {
    kernelSize = 2*kernel+1
    kernel = rep(1, prod(kernelSize))
    dim(kernel) = kernelSize
    radius = (kernelSize-1)/2
  }
  else {
    # Check that all sizes are odd
    radius = (dim(kernel)-1)/2
  }
  
  if ( length(dim(kernel)) != image@dimension ) {
    stop("kernel must have same number of dimensions as 'image'")
  }
  
  return(.Call("antsImage_GetNeighborhood", image, center, kernel, radius,
               physical.coordinates, PACKAGE="ANTsRCore"))
}


#' @name getNeighborhoodInMask
#' @title Get neighborhoods for voxels within mask
#'
#' @description this converts a scalar image to a matrix with rows that contain neighbors
#' around a center voxel
#'
#' @param image image object of S4 class \code{antsImage} to get values from.
#' @param mask image object of S4 class \code{antsImage} indicating which voxels to
#' examine. Each voxel > 0 will be used as the center of a neighborhood
#' @param radius array of values for neighborhood radius (in voxels)
#' @param physical.coordinates logical indicating if voxel indices and
#' offsets should be in voxel or physical coordinates
#' @param boundary.condition  string indicating how to handle voxels in a
#' neighborhood, but not in the mask. See \code{Details}.
#' @param spatial.info a boolean indicating of voxel locations and neighborhood
#' offsets should be returned along with pixel values.
#' @param get.gradient a boolean indicating if a matrix of gradients (at the
#' center voxel) should be returned in addition to the value matrix (WIP)
#' @details
#' \code{boundary.condition} should be one of:
#' \itemize{
#'   \item{\code{NA}: }{Fill values with \code{NA}.}
#'   \item{\code{image}: }{Use image value, even if not in mask.}
#'   \item{\code{mean}: }{Use man of all non-\code{NA} values for that neighborhood.}
#' }
#' @return
#'
#' if \code{spatial.info} is false: a matrix of pixel values where the number of rows
#' is the size of the neighborhood and there is a column for each voxel
#'
#' if \code{spatial.info} is true, a list containing three matrices:
#' \itemize{
#'  \item{values: }{matrix of pixel values where the number of rows
#'  is the size of the neighborhood and there is a column for each voxel.}
#'  \item{indices: }{matrix providing the center coordinates for each neighborhood}
#'  \item{offsets: }{matrix providing the offsets from center for each
#'   voxel in a neighborhood}
#' }
#' @author Duda JT
#' @examples
#' r16 <- getANTsRData("r16")
#' r16 <- antsImageRead(r16,2)
#' mask <- getMask(r16,lowThresh=mean(r16),cleanup=1)
#' radius <- rep(2,2)
#' mat <- getNeighborhoodInMask(r16,mask,radius)
#' mat <- getNeighborhoodInMask(r16,mask,radius,
#' boundary.condition ="image")
#' mat <- getNeighborhoodInMask(r16,mask,radius,
#' boundary.condition ="mean") 
#' randlist <- getNeighborhoodInMask(r16,mask,radius,
#' physical.coordinates = TRUE)
#' arr = as.array(r16)
#' testthat::expect_error(getNeighborhoodInMask(arr,mask,radius), "antsImage")
#' testthat::expect_error(getNeighborhoodInMask(r16,as.numeric(mask),radius), 
#' "mask must be")
#' testthat::expect_error(getNeighborhoodInMask(r16,mask,as.character(radius)), 
#' "radius must be")
#' # testthat::expect_error(getNeighborhoodInMask(r16,mask,c(radius, 3)),
#' # "Radius must") 
#'
#' @export
getNeighborhoodInMask <- function(image, mask, radius, physical.coordinates = FALSE,
                                  boundary.condition = "NA", spatial.info = FALSE, get.gradient = FALSE ) {
  
  image = check_ants(image)
  
  if (!is.antsImage(image)) {
    stop("Input must be of class 'antsImage'")
  }
  
  mask = check_ants(mask)
  if (!is.antsImage(mask)) {
    stop("mask must be of class 'antsImage'")
  }
  
  if ((class(radius) != "numeric")) {
    stop("radius must be of class 'numeric'")
  }
  
  if ((prod(radius * 2 + 1) * sum(as.array(mask))) > (2^31 - 1)) {
    stop("Requested matrix size is too large for Rcpp")
  }
  
  boundary = 0
  if (boundary.condition == "image") {
    boundary = 1
  }
  if (boundary.condition == "mean") {
    boundary = 2
  }
  
  return(.Call("antsImage_GetNeighborhoodMatrix", image, mask, radius, physical.coordinates,
               boundary, spatial.info, get.gradient ))
  
}
