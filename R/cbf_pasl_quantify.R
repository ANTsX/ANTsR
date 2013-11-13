cbf_pasl_quantify <- function(ratioM, lambda = 0.9, alpha = 0.95, TI1 = 700, TI2 = 1700, T1blood = 1664, sliceTime = 45) {
  
  sliceTimeImg <- new("antsImage", "float", 3)
  ImageMath(3, sliceTimeImg, "m", ratioM, 0)
  
  for (slice in 1:dim(ratioM)[length(dim(ratioM))]) {
    for (x in 1:dim(ratioM)[1]) {
      for (y in 1:dim(ratioM)[2]) {
        TI <- (TI2 - TI1) + sliceTime * (slice - 1)
        value <- 2 * alpha * TI1 * exp(-TI/T1blood)
        sliceTimeImg[x - 1, y - 1, slice - 1] <- value
      }
    }
  }
  
  # Return cbf in mL/100g/min
  cbf <- new("antsImage", "float", 3)
  ImageMath(3, cbf, "m", ratioM, 540000 * lambda)
  ImageMath(3, cbf, "/", cbf, sliceTimeImg)
  
  return(cbf)
  
} 
