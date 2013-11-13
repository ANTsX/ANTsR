# chen 2011 paper PASL --------------------------------------------------------------------------------------
cbf_pasl_Chen2011 <- function(aslimg_filename, m0img_filename) {
  img <- as.array(antsImageRead(aslimg_filename, 4, "double"))
  M0 <- as.array(antsImageRead(m0img_filename, 3, "double"))
  
  numdiffs <- floor(dim(img)[4]/2)
  
  labelimg <- img[, , , seq(1, by = 2, length.out = numdiffs)]
  controlimg <- img[, , , seq(2, by = 2, length.out = numdiffs)]
  
  lambda <- 0.9
  deltaM <- (controlimg - labelimg)
  alpha <- 0.95
  
  M0 <- rep(M0, numdiffs)
  dim(M0) <- dim(controlimg)
  
  TI1 <- 700
  TI2 <- 1700
  T1b <- 1664
  
  cbf <- (lambda * deltaM)/(2 * alpha * M0 * TI1 * exp(-TI2/T1b))
  
  return(cbf)
} 
