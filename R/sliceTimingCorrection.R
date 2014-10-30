sliceTimingCorrection <- function(fmri, sliceTime = NA, interpolation = "sinc", sincRadius = 4, 
  bsplineOrder = 3) {
  tr <- antsGetSpacing(fmri)[length(dim(fmri))]
  
  if (is.na(sliceTime)) {
    sliceTime <- tr/(dim(fmri)[length(dim(fmri)) - 1])
    print(paste("Assuming a slice timing of", sliceTime))
  }
  
  corrected <- antsImageClone(fmri)
  
  
  if (interpolation == "sinc") {
    ImageMath(4, corrected, "SliceTimingCorrection", fmri, sliceTime, interpolation, 
      sincRadius)
  } else if (interpolation == "bspline") {
    ImageMath(4, corrected, "SliceTimingCorrection", fmri, sliceTime, interpolation, 
      bsplineOrder)
  } else if (interpolation == "linear") {
    ImageMath(4, corrected, "SliceTimingCorrection", fmri, sliceTime)
    
  } else {
    warning("Invalid interpolation type, options are: linear, sinc, and bspline")
    return(NA)
  }
  
  return(corrected)
} 
