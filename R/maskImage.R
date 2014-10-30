maskImage <- function(img.in, img.mask, level = 1, binarize = FALSE) {
  if (class(level) == "numeric") {
    img.out <- antsImageClone(img.in)
    img.out[img.mask != level] <- 0
    return(img.out)
  }
  if (class(level) == "list") {
    img.out <- antsImageClone(img.in)
    img.out[img.out > 0] <- 0
    for (mylevel in level) {
      myval <- as.numeric(mylevel)
      if (binarize) 
        img.out[img.mask == myval] <- 1
      if (!binarize) 
        img.out[img.mask == myval] <- img.in[img.mask == myval]
    }
    return(img.out)
  }
  
} 
