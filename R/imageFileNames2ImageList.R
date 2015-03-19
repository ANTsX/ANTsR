#' Simple imageFileNames2ImageListing function.
#'
#' ImageFileNames2ImageLists converts the input list of file names to a list
#' containing antsImages.
#'
#'
#' @param x input file name list
#' @param dim img dimensionality (optional)
#' @return a list like this : mylist<-list( img1, img2 , etcetera ) is the
#' output ... img* are antsImages
#' @author Avants BB
#' @examples
#'
#' \dontrun{
#'   # gglb<-paste('gmView1vec*.nii.gz',sep='')
#'   # gfnl<-imageFileNames2ImageList( list.files(path=statdir,
#'   #   pattern = glob2rx(gglb),full.names = T,recursive = T) )
#' }
#'
#' @export imageFileNames2ImageList
imageFileNames2ImageList <- function( x, dim ) {
  if (nargs() == 0) {
    print("Usage:  ilist<-imageFileNames2ImageList( x , imageDimension ) ")
    return(1)
  }
  if ( missing(dim) ) dim<-antsImageRead( x[1] )@dimension
  localreadfun <- function(x) {
    antsImageRead(x, dim)
  }
  ilist <- lapply(x, localreadfun)
  return(ilist)
}
