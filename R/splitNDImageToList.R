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
    if ( mydim == 3 )
      newimgs[[z]] = makeImage( mydimv[ -mydim ] , img[ 1:mydimv[1], 1:mydimv[2], z ] )
    if ( mydim == 4 )
      newimgs[[z]] = makeImage( mydimv[ -mydim ] ,
        img[ 1:mydimv[1], 1:mydimv[2], 1:mydimv[3], z ] )
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
