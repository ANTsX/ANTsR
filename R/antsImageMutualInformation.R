antsImageMutualInformation <- function(in_image1, in_image2) {
  if (length(dim(in_image1)) == 1)
    if (dim(in_image1)[1] == 1)
      return(NULL)
  .Call("antsImageMutualInformation", in_image1, in_image2, PACKAGE = "ANTsR")
}
