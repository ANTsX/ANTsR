#' splitNDImageToList
#'
#' This function splits the dimensionality d input image into a list of images
#' with dimensonality d-1 by slicing the input image along its last dimension.
#'
#' @param img input image of class antsImage, last dimension will be split
#' @return output list filled with d-1 volumes of class antsImage
#' @author Pustina D, Avants B
#' @examples
#'
#' my4Dimage = makeImage( c(5,5,5,4) )
#' my3Dlist = splitNDImageToList( my4Dimage )
#'
#' @export splitNDImageToList
#'
splitNDImageToList = function( img ) {
  # check input is good
  if ( class(img) != 'antsImage' ) stop('Input is not antsImage.')
  mydimv = dim( img )
  mydim  = img@dimension
  if ( img@dimension < 3 )
    stop('Input image dimensionality needs to be 3 or greater')

  # put matrix to a list
  newimgs = list()
  for ( z in 1:mydimv[ mydim ] ) {
   newimgs[[z]] = extractSlice(img, z, mydim)
   }

  # copy headers to new images
  direction = antsGetDirection(img)[-mydim,-mydim]
  spacing = antsGetSpacing(img)[-mydim]
  origin = antsGetOrigin(img)[-mydim]
  for (im in newimgs) {
    antsSetDirection(im, direction)
    antsSetSpacing(im, spacing)
    antsSetOrigin(im, origin)
  }
  return(newimgs)
}


#' mergeListToNDImage
#'
#' This function will copy a list of d-1 images into a d-dimension target image.
#' This function reverses the operation performed by \code{splitNDImageToList}.
#'
#' @param img input image of class antsImage, last dimension will be split
#' @param imgList input list to be merged back into dimension d space
#' @return output list filled with d-1 volumes of class antsImage
#' @author Pustina D, Avants B
#' @examples
#'
#' my4Dimage = makeImage( c(5,5,5,4), rnorm( 5*5*5*4 ) )
#' my3Dlist = splitNDImageToList( my4Dimage )
#' my4DimageR = mergeListToNDImage( my4Dimage, my3Dlist )
#'
#' @export mergeListToNDImage
#'
mergeListToNDImage = function( img, imgList ) {
  # check input is good
  if ( class(img) != 'antsImage' ) stop('Input is not antsImage.')
  mydimv = c( dim( imgList[[1]] ), length( imgList ) )
  mydim  = img@dimension
  if ( img@dimension < 3 )
    stop('Input image dimensionality needs to be 3 or greater')

  iarr = array( data = NA, dim = mydimv )
  for ( i in 1:length( imgList ) )
    {
    temp = as.array( imgList[[ i ]] )
    if ( mydim == 3 ) {
      iarr[ , , i ] = temp
      }
    if ( mydim == 4 ) {
      iarr[ ,,, i ] = temp
      }
    }
  iarr = as.antsImage( iarr )
  antsCopyImageInfo( img, iarr )
  antsSetSpacing( iarr,
    c( antsGetSpacing( imgList[[1]] ), tail( antsGetSpacing( img ), 1 ) ) )
  antsSetOrigin( iarr,
    c( antsGetOrigin( imgList[[1]] ), tail( antsGetOrigin( img ), 1 ) ) )
  return( iarr )
}
