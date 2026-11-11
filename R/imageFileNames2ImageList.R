#' Simple imageFileNames2ImageListing function.
#'
#' ImageFileNames2ImageLists converts the input list of file names to a list
#' containing antsImages.
#'
#'
#' @param x input file name list
#' @return a list containing the images  this : mylist<-list( img1, img2 , etcetera ) is the
#' @author Avants BB, Kandel BM
#' @examples
#' dir <- file.path(tempdir(), "imageFileNames2ImageList")
#' dir.create(dir, showWarnings = FALSE)
#' img <- makeImage(c(4, 4))
#' for (ii in 1:6) {
#'   antsImageWrite(img, file.path(dir, paste0("image", ii, ".nii.gz")))
#' }
#' imagenames <- list.files(dir, glob2rx("*.nii.gz"), full.names = TRUE)
#' images <- imageFileNames2ImageList(imagenames)
#' unlink(dir, recursive = TRUE)
#'
#' @export imageFileNames2ImageList
imageFileNames2ImageList <- function(x) {
  localreadfun <- function(x) {
    antsImageRead(x)
  }
  ilist <- lapply(x, localreadfun)
  return(ilist)
}
