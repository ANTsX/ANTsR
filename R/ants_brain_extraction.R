.ants_brain_extraction <- function(img = "") {
  # check if called with no arguments and print usage
  if (nchar(img) == 0) {
    print("usage: ants_brain_extraction( <time-series-average-image> )")
    return
  }

  # check if there is an extension
  if (length(strsplit(img, ".", fixed = TRUE)[[1]]) < 2) {
    print("There appears to be no extension to the input file. Please provide a [nii|nii.gz] file.")
    return
  }

  # split the string into filename and extension
  split_img <- strsplit(img, ".", fixed = TRUE)[[1]]
  filename <- split_img[1]
  if (length(split_img) == 2) {
    extension <- paste("", split_img[2], sep = ".")
  } else if (length(split_img) == 3) {
    extension <- paste("", split_img[2], split_img[3], sep = ".")
  }

  bm_img <- paste(filename, "_brainmask", extension, sep = "")

  # N3BiasFieldCorrection( 3 , img , img , 2 ) ; for( x in 1:3 ) {
  # N3BiasFieldCorrection( 3 , img , img , 1 ) ; }

  bm_img<-thresholdImage( img, "Otsu", 3 )
  bm_img<-thresholdImage( bm_img,  2, 3 )
  ImageMath(3, bm_img, "ME", bm_img, 1)
  ImageMath(3, bm_img, "GetLargestComponent", bm_img)
  ImageMath(3, bm_img, "MD", bm_img, 1)
  ImageMath(3, bm_img, "ME", bm_img, 1)
}
