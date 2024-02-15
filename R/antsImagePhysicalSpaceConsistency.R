#' Check for physical space consistency
#'
#' Check if two \code{antsImage} objects occupy the same physical space
#'
#'
#' @param img1 Image object of S4 class \code{antsImage}.
#' @param img2 Image object of S4 class \code{antsImage}.
#' @param coordinate.tolerance floating point error tolerance in origin
#' @param direction.tolerance floating point error tolerance in direction matrix
#' @param data.type boolean, if TRUE check pixeltype and components, default is FALSE
#' @return Boolean indicating consistency of physical space
#' @examples
#'
#' img1 <- makeImage(c(10,10), rnorm(100))
#' img2 <- makeImage(c(10,10), rnorm(100))
#' check <- antsImagePhysicalSpaceConsistency(img1, img2)
#' check <- antsImagePhysicalSpaceConsistency(img1, img2, data.type = TRUE)
#' 
#' testthat::expect_error(
#' antsImagePhysicalSpaceConsistency(img1, as.array(img2))
#' )
#' 
#' i2 = img2
#' pixeltype(i2) = "unsigned int"
#' testthat::expect_false(
#' antsImagePhysicalSpaceConsistency(img1, i2, data.type = TRUE)
#' ) 
#' 
#' i2 = mergeChannels(list(img2, img2))
#' testthat::expect_false(
#' antsImagePhysicalSpaceConsistency(img1, i2, data.type = TRUE)
#' )  
#' 
#' i2 = as.antsImage(img2[1:5, 1:5], reference = img2)
#' testthat::expect_false(
#' antsImagePhysicalSpaceConsistency(img1, i2)
#' ) 
#' i2 = img2
#' antsSetSpacing(i2, c(0.5, 0.5))
#' testthat::expect_false(
#' antsImagePhysicalSpaceConsistency(img1, i2)
#' )  
#' i2 = img2
#' antsSetOrigin(i2, c(1, 2))
#' testthat::expect_false(
#' antsImagePhysicalSpaceConsistency(img1, i2)
#' ) 
#' 
#' i2 = img2
#' antsSetDirection(i2, -1*antsGetDirection(i2))
#' testthat::expect_false(
#' antsImagePhysicalSpaceConsistency(img1, i2)
#' )    
#'
#' @export
# This implementation mimics: itkImageToImageFilter.hxx
antsImagePhysicalSpaceConsistency <- function(img1, img2, coordinate.tolerance=1e-2, direction.tolerance=1e-2,
    data.type=FALSE ) {
  img1 = check_ants(img1)
  img2 = check_ants(img2)
  
  if (!(is.antsImage(img1)) || !(is.antsImage(img2))) {
    stop("Both inputs must be of class 'antsImage'")
  }

  # Image dimension check
  if ( !isTRUE(all.equal(dim(img1), dim(img2)) ))
  {
    #print( "Images are of different size")
    return(FALSE)
  }

  # Image spacing check
  cTol = coordinate.tolerance * antsGetSpacing(img1)[1]
  spDiff = abs(antsGetSpacing(img1) - antsGetSpacing(img2))
  if (length( which(spDiff > cTol)) > 0)
  {
    #print( "Images have inconsistent spacing")
    return(FALSE)
  }

  # Image origin check
  ogDiff = abs(antsGetOrigin(img1) - antsGetOrigin(img2))
  if (length( which(ogDiff > cTol)) > 0)
  {
    #print( "Images have inconsistent origin")
    return(FALSE)
  }

  # Image direction check
  dirDiff = abs(antsGetDirection(img1) - antsGetDirection(img2))
  if ( length( which(dirDiff > direction.tolerance) ) > 0)
  {
    #print( "Image have different directions")
    return(FALSE)
  }

  if ( data.type == TRUE )
  {
    if ( img1@pixeltype != img2@pixeltype )
      {
      return(FALSE)
      }
    if ( img1@components != img2@components )
      {
      return(FALSE)
      }
  }

  return(TRUE)
}
