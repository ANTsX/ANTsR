# antsAverageImages calculate the mean of a list of antsImages imageList: can
# contain anything that be cast via as.array() refernce: must be an antsImage as
# the header info is used normalze=FALSE: divide each inputs by its mean before
# averaging returns: antsImage

antsAverageImages <- function(imageList, reference, normalize = FALSE) {
  
  # FIXME - input checking here
  if (class(reference) != "antsImage") {
    stop("reference must be of class 'antsImage'")
  }
  
  template <- as.array(reference) * 0
  
  for (i in imageList) {
    img <- as.array(i)
    
    if (length(which(dim(img) != dim(reference)) == FALSE) > 0) {
      stop("Inputs and reference must all have same dimensions")
    }
    
    if (normalize) {
      img <- img/mean(img)
    }
    template <- template + img
  }
  template <- as.antsImage(template/length(imageList))
  antsCopyImageInfo(reference, template)
  return(template)
} 
