#' similarity metrics between two images as a function of geometry
#'
#' compute similarity metric between two images as image is rotated about its
#' center w/or w/o optimization
#'
#' @param in_image1 reference image
#' @param in_image2 moving image
#' @param thetas numeric vector of search angles
#' @param localSearchIterations integer controlling local search in multistart
#' @param metric which metric MI or GC (string)
#' @param scaleImage global scale
#' @param doReflection reflect image about principal axis
#' @param txfn if present, write optimal tx to .mat file
#' @return vector of similarity values
#' @author Brian B. Avants
#' @keywords image similarity
#' @examples
#'
#' fi<-antsImageRead( getANTsRData("r16") ,2)
#' mi<-antsImageRead( getANTsRData("r64") ,2)
#' mival<-invariantImageSimilarity( fi, mi, c(0,10,20) )
#'
#' @export invariantImageSimilarity
invariantImageSimilarity <- function(in_image1, in_image2,
   thetas, localSearchIterations = 0,
  metric = "MI", scaleImage = 1, doReflection = 0, txfn = "") {
  if (length(dim(in_image1)) == 1)
    if (dim(in_image1)[1] == 1)
      return(NULL)
  if (in_image1@pixeltype != "float" | in_image2@pixeltype != "float") {
    print(args(invariantImageSimilarity))
    print("input images must have float pixeltype")
    return(NA)
  }
  # convert to radians if necessary
  thetain <- thetas
  if (max(abs(thetas)) > 2)
    thetain <- (thetas * pi)/180
  ImageMath(in_image1@dimension, in_image1, "Normalize", in_image1)
  ImageMath(in_image2@dimension, in_image2, "Normalize", in_image2)
  if (class(localSearchIterations) != "numeric") {
    print("wrong input: localSearchIterations is not numeric")
    return(NA)
  }
  if (class(metric) != "character") {
    print("wrong input: metric is not numeric")
    return(NA)
  }
  if (doReflection == 0) {
    r1 <- .Call("invariantImageSimilarity", in_image1, in_image2, thetain, localSearchIterations,
      metric, scaleImage, doReflection, txfn, PACKAGE = "ANTsR")
    return(r1)
  }
  txfn1 <- tempfile(fileext = ".mat")
  txfn2 <- tempfile(fileext = ".mat")
  txfn3 <- tempfile(fileext = ".mat")
  txfn4 <- tempfile(fileext = ".mat")
  r1 <- .Call("invariantImageSimilarity", in_image1, in_image2, thetain, localSearchIterations,
    metric, scaleImage, 0, txfn1, PACKAGE = "ANTsR")
  r2 <- .Call("invariantImageSimilarity", in_image1, in_image2, thetain, localSearchIterations,
    metric, scaleImage, 1, txfn2, PACKAGE = "ANTsR")
  r3 <- .Call("invariantImageSimilarity", in_image1, in_image2, thetain, localSearchIterations,
    metric, scaleImage, 2, txfn3, PACKAGE = "ANTsR")
  r4 <- .Call("invariantImageSimilarity", in_image1, in_image2, thetain, localSearchIterations,
    metric, scaleImage, 3, txfn4, PACKAGE = "ANTsR")
  ww <- which.min(c(min(r1), min(r2), min(r3), min(r4)))
  print(ww)
  if (ww == 1) {
    return(list(r1, txfn1))
  }
  if (ww == 2) {
    return(list(r2, txfn2))
  }
  if (ww == 3) {
    return(list(r3, txfn3))
  }
  if (ww == 4) {
    return(list(r4, txfn4))
  }
}
