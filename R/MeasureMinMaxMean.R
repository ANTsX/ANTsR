MeasureMinMaxMean <- function( image ){
  if( missing(image))
    stop("Missing input image.")
  img.min <- min(as.array(image)) 
  img.mean <- mean(as.array(image))
  img.max <- max(as.array(image))
  list(img.min=img.min, img.max=img.max, img.mean=img.mean)
}
