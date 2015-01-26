#' Measure Min Max Mean
#' 
#' Measure min, max, and mean of image.
#' 
#' 
#' @param image Input image of type \code{antsImage}.
#' @return List with entries \code{img.min}, \code{img.max}, \code{img.mean}.
#' @author Kandel BM.
#' @examples
#' 
#' \dontrun{
#'   myimg <- as.antsImage(array(rnorm(1000), dim=c(10, 10, 10)))
#'   MeasureMinMaxMean(myimg)
#' }
#' 
#' @export MeasureMinMaxMean
MeasureMinMaxMean <- function(image, mask = NA) {
  if (missing(image)) 
    stop("Missing input image.")
  if (!is.na(mask)) {
    imagevec <- image[mask > 0.5]
    img.min <- min(imagevec)
    img.mean <- mean(imagevec)
    img.max <- max(imagevec)
    return(list(img.min = img.min, img.max = img.max, img.mean = img.mean))
  }
  img.min <- min(as.array(image))
  img.mean <- mean(as.array(image))
  img.max <- max(as.array(image))
  list(img.min = img.min, img.max = img.max, img.mean = img.mean)
} 
