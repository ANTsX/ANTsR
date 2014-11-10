makeImage <- function(imagesize, voxval = 1) {
  firstparamnumer <-( typeof(imagesize) == "double" |
    typeof(imagesize) == "integer" )
  if ( firstparamnumer )
  {
  imagedimension <- length(imagesize)
  outimg <- new("antsImage", "float", imagedimension)
  if (imagedimension == 2)
    ImageMath(imagedimension, outimg, "MakeImage", imagesize[1], imagesize[2])
  if (imagedimension == 3)
    ImageMath(imagedimension, outimg, "MakeImage", imagesize[1], imagesize[2],
      imagesize[3])
  if (imagedimension == 4)
    ImageMath(imagedimension, outimg, "MakeImage", imagesize[1], imagesize[2],
      imagesize[3], imagesize[4])
  outimg[outimg == 0] <- voxval
  return(outimg)
  }
  if ( class(imagesize)[[1]] == "antsImage")
  {
  img<-antsImageClone( imagesize )
  sel <-( imagesize > 0 )
  if ( length(voxval) == sum(sel) )
    img[ sel ]<-voxval
  return( img )
  }
}
