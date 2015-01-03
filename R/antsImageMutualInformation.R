antsImageMutualInformation <- function(in_image1, in_image2) {
  if (length(dim(in_image1)) == 1)
    if (dim(in_image1)[1] == 1)
      return(NULL)
  if ( in_image1@pixeltype != "float" |
       in_image2@pixeltype != "float"   )
       {
       print(args(antsImageMutualInformation))
       print("input images must have float pixeltype")
       return(NA)
       }
  .Call("antsImageMutualInformation", in_image1, in_image2, PACKAGE = "ANTsR")
}
