MeasureMinMaxMean <- function( image, mask=NA ){
  if( missing(image))
    stop("Missing input image.")
  if ( ! is.na( mask ) ) 
  {
  imagevec<-image[ mask > 0.5 ]
  img.min <- min(imagevec)
  img.mean <- mean(imagevec)
  img.max <- max(imagevec)
  return( list(img.min=img.min, img.max=img.max, img.mean=img.mean) )
  }
  img.min <- min(as.array(image)) 
  img.mean <- mean(as.array(image))
  img.max <- max(as.array(image))
  list(img.min=img.min, img.max=img.max, img.mean=img.mean)
}
