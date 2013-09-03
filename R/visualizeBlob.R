visualizeBlob <- function(template, blob, outname='wmBlob', dim=3){
  if(missing(template) | missing(blob))
    stop("Must provide template and blob images.")
  if(is.character(template)){
    template <- antsImageRead(template, dim, 'float')
  } else if(class(template) == "antsImage") {
      if(template@pixeltype != "float") {
      template <- antsImageClone(template, 'float')
    }
  } else stop("Template must be file name or antsImage.")

  if(is.character(blob)){ 
    blob <- antsImageRead(blob, dim, 'float') 
  } else if(class(blob) == "antsImage") {
      if(blob@pixeltype != "float") {
      blob <- antsImageClone(blob, 'float')
    }
  } else stop("Blob must be file name or antsImage.")
  
  mymask <- getMask(template)
  myseg <- Atropos(d=dim, a=template, m="[0.25,1x1x1]", c="[2,0]", 
    x=mymask, i="kmeans[3]")
  wm <- antsImageClone(maskImage(myseg$segmentation, myseg$segmentation, 3), 'float')
  glassbrain <- antsImageClone(mymask)
  SmoothImage(dim, glassbrain, "3", glassbrain)
  ThresholdImage(dim, glassbrain, glassbrain, "0.3", "999")
  ImageMath(dim, glassbrain, "FillHoles", glassbrain)
  myrender <- renderSurfaceFunction(list(glassbrain), list(wm, blob), surfval=0.2, smoothsval=1.5, 
    alphasurf=0.3, smoothfval=1.5, alphafunc=1)
  lateralLeft <- rotationMatrix(pi/2, 0, -1, 0) %*% rotationMatrix(pi/2, -1, 0, 0)
  par3d(userMatrix=lateralLeft, windowRect=c(25,25,325,325), zoom=0.7)
  rgl.snapshot(paste(outname, "_lateral.png", sep=''))
  anterior <- rotationMatrix(0, 0, -1, 0) %*% rotationMatrix(pi/2, -1, 0, 0)
  par3d(userMatrix=anterior, windowRect=c(25,25,325,325), zoom=0.7)
  rgl.snapshot(paste(outname, "anterior.png", sep=''))
  list(mask=mymask, seg=myseg, glassbrain=glassbrain)
}
