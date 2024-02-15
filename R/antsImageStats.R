antsImageStats = function( image, mask=NULL, na.rm=FALSE ) {
  
  if (!is.null(mask)) {
    mask = check_ants(mask)
  }
  image = antsImageClone(image, out_pixeltype = "double")
  res =  .Call("antsImageStats", image, mask, na.rm, PACKAGE="ANTsRCore")
  check = !any(sapply(res, is.raw))
  if (!check) {
    stop("raw output form antsImageStats, please report this bug")
  }  
  res = lapply(res, drop)
  return(res )

}
