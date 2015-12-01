#' Segmentation for eigenanatomy.
#'
#' Segment a mask into regions based on the max value in an image list.  At a
#' given voxel the segmentation label will contain the index to the image that
#' has the largest value.  If the 3rd image has the greatest value, the
#' segmentation label will be 3 at that voxel.
#'
#'
#' @param mask D-dimensional mask > 0 defining segmentation region.
#' @param imgList list containing antsImages or a matrix matching the mask.
#' @param applySegmentationToImages boolean determines if original image list
#' is modified by the segmentation.
#' @param cthresh throw away isolated clusters smaller than this value
#' @return segmentation image.
#' @author Avants BB
#' @examples
#'
#' mylist<-list( antsImageRead( getANTsRData("r16") ),
#'   antsImageRead( getANTsRData("r27") ),
#'   antsImageRead( getANTsRData("r85") ) )
#' myseg<-eigSeg( getMask( mylist[[1]] ) , mylist )
#' mat=imageListToMatrix( mylist, getMask( mylist[[1]] ) )
#' myseg<-eigSeg( getMask( mylist[[1]] ) , mat )
#'
#' @export eigSeg
eigSeg <- function(mask = NA, imgList = NA,
  applySegmentationToImages = FALSE, cthresh=0 ) {
  if (typeof(mask) != "S4") {
    print(args(eigSeg))
    return(1)
  }
  maskvox <- (mask > 0)
  maskseg <- antsImageClone(mask)
  maskseg[maskvox] <- 0
  if ( class(imgList) == "matrix")
    mydata <- imgList
  if ( class(imgList) != "matrix")
    if ( length(imgList) > 0 )
      if ( typeof(imgList) == "list")
        mydata <- imageListToMatrix(imgList, mask)
  if ( ! exists('mydata') )
    stop("wrong input type - see help")
  segids <- apply(abs(mydata), 2, which.max)
  segmax <- apply(abs(mydata), 2, max)
  maskseg[maskvox] <- (segids * (segmax > 1e-09))
  if ( cthresh > 0 )
    {
    for ( kk in 1:max(maskseg) )
      {
      timg = thresholdImage( maskseg, kk, kk ) %>% labelClusters( cthresh )
      timg = thresholdImage( timg, 1, Inf ) * as.numeric( kk )
      maskseg[ maskseg == kk ] = timg[ maskseg == kk ]
      }
    }
  if (applySegmentationToImages & class(imgList) != "matrix" ) {
    for (i in 1:length(imgList)) {
      img <- imgList[[i]]
      img[maskseg != as.numeric(i)] <- 0
      imgList[[i]] <- img
      }
    }
  return(maskseg)
}

.matrixSeg <- function(mydatamatrix) {
  segids <- apply(abs(mydatamatrix), 2, which.max)
  segmax <- apply(abs(mydatamatrix), 2, max)
  binmat <- mydatamatrix * 0
  for (i in 1:ncol(binmat)) binmat[segids[i], i] <- 1
  return(mydatamatrix * binmat)
}
