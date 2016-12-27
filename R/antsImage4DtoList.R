#' antsImage4DtoList
#'
#' Resample image by using another image as target reference.
#' This function uses antsApplyTransform with an identity 
#' matrix to achieve proper resampling.
#'
#' @param img input 4D image of class antsImage, 4th dimension has volumes
#' @return output list filled with 3D volumes of class antsImage
#' @author Pustina D
#' @examples
#' ## Do not run
#' ## my4Dimage = antsImageRead('/path/to/image.nii.gz)
#' ## my3Dlist = antsImage4DtoList(my4Dimage)
#' ## End do not run
#'
#' @export antsImage4DtoList
#' 
antsImage4DtoList = function(img) {
  # check input is good
  if ( class(img) != 'antsImage' ) stop('Input is not antsImage.')
  if ( length(dim(img)) != 4 ) stop('Input image is not 4D.')
  
  # put everything to a matrix
  mask <- as.antsImage(array( 1 , dim(img)[1:3] ))
  mat = timeseries2matrix(img, mask)
  
  # put matrix to a list
  newimg = matrixToImages(mat,mask)
  rm(mat)
  invisible(gc)
  
  # copy headers to new images
  direction = antsGetDirection(img)[-4,-4]
  spacing = antsGetSpacing(img)[-4]
  origin = antsGetOrigin(img)[-4]
  for (im in newimg) {
    antsSetDirection(im, direction)
    antsSetSpacing(im, spacing)
    antsSetOrigin(im, origin)
  }
  return(newimg)
}