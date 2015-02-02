#' Segmentation for eigenanatomy.
#' 
#' Segment a mask into regions based on the max value in an image list.  At a
#' given voxel the segmentation label will contain the index to the image that
#' has the largest value.  If the 3rd image has the greatest value, the
#' segmentation label will be 3 at that voxel.
#' 
#' 
#' @param mask D-dimensional mask > 0 defining segmentation region.
#' @param imageList list containing antsImages or filenames pointing to
#' antsImages.
#' @param applySegmentationToImages boolean determines if original image list
#' is modified by the segmentation.
#' @return segmentation image.
#' @author Avants BB
#' @examples
#' 
#' \dontrun{
#' mylist<-list(image1,image2)
#' # both approaches below are ok 
#' myseg<-eigSeg( mask, mylist ) 
#' myseg<-eigSeg( mask, c('a.nii.gz','b.nii.gz')  ) 
#' }
#' 
#' @export eigSeg
eigSeg <- function(mask = NA, imgList = NA, applySegmentationToImages = TRUE) {
  if (typeof(mask) != "S4") {
    print(args(eigSeg))
    return(1)
  }
  maskvox <- (mask > 0)
  maskseg <- antsImageClone(mask)
  maskseg[maskvox] <- 0
  if (length(imgList) > 0) {
    if (typeof(imgList) == "list") 
      mydata <- imageListToMatrix(imgList, mask)
    if (typeof(imgList) == "character") 
      mydata <- imagesToMatrix(imgList, mask)
    segids <- apply(abs(mydata), 2, which.max)
    segmax <- apply(abs(mydata), 2, max)
    maskseg[maskvox] <- (segids * (segmax > 1e-09))
    print(max(segmax))
    print(max(segids))
    if (applySegmentationToImages) {
      for (i in 1:length(imgList)) {
        img <- imgList[[i]]
        img[maskseg != as.numeric(i)] <- 0
        imgList[[i]] <- img
      }
    }
    return(maskseg)
  } else print("No images in list")
}


matrixSeg <- function(mydatamatrix) {
  segids <- apply(abs(mydatamatrix), 2, which.max)
  segmax <- apply(abs(mydatamatrix), 2, max)
  binmat <- mydatamatrix * 0
  for (i in 1:ncol(binmat)) binmat[segids[i], i] <- 1
  return(mydatamatrix * binmat)
} 
