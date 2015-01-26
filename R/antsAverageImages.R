#' antsAverageImages
#'
#' calculate the mean of a list of antsImages imageList: can contain anything that be cast via as.array()
#' @param imageList must be an antsImage list
#' @param reference must be an antsImage of the same size as those in the list
#' @param normalize true or false
#' @author Avants BB
#' @author Avants BB
antsAverageImages <- function(imageList, reference, normalize = FALSE) {

  # FIXME - input checking here
  if (class(reference) != "antsImage") {
    stop("reference must be of class 'antsImage'")
  }

  template <- as.array(reference) * 0

  for (i in imageList) {
    img <- as.array(i)

    if (length(which(dim(img) != dim(reference)) == FALSE) > 0) {
      stop("Inputs and reference must all have same dimensions")
    }

    if (normalize) {
      img <- img/mean(img)
    }
    template <- template + img
  }
  template <- as.antsImage(template/length(imageList))
  antsCopyImageInfo(reference, template)
  return(template)
}
