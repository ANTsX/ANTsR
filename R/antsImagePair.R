antsImagePair <- function(img1, img2) {
  paste(int_antsExtractXptrAsString(img1), int_antsExtractXptrAsString(img2), sep = "_")
} 
