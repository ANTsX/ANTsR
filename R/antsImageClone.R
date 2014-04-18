antsImageClone <- function(in_image, out_pixeltype = in_image@pixeltype) {
   if( length(dim(in_image)) == 1 )
       if( dim(in_image)[1] == 1 )
           return(NULL)
  .Call("antsImageClone", in_image, out_pixeltype, PACKAGE = "ANTsR")
} 
