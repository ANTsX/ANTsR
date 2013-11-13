antsImageWrite <- function(image, filename) {
  if (class(image) != "antsImage") {
    print("'image' argument provided is not of class 'antsImage'")
    return(NULL)
  }
  return(.Call("antsImageWrite", image, filename, PACKAGE = "libRantsImageWrite"))
} 
