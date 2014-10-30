# chen 2011 paper CASL
# --------------------------------------------------------------------------------------
cbf_casl_Chen2011 <- function(aslimg_filename) {
  img <- as.array(antsImageRead(aslimg_filename, 4, "double"))
  
  numdiffs <- floor(dim(img)[4]/2)
  
  labelimg <- img[, , , seq(1, by = 2, length.out = numdiffs)]
  controlimg <- img[, , , seq(2, by = 2, length.out = numdiffs)]
  
  lambda <- 0.9
  deltaM <- (controlimg - labelimg)
  alpha <- 0.68
  
  M0 <- array(0, dim(controlimg)[1:3])
  for (x in 1:(dim(M0)[1])) for (y in 1:(dim(M0)[2])) for (z in 1:(dim(M0)[3])) {
    M0[x, y, z] <- mean(controlimg[x, y, z, ])
  }
  M0 <- rep(M0, numdiffs)
  dim(M0) <- dim(controlimg)
  
  T1b <- 1664
  omega <- 1
  tau <- 2
  
  cbf <- (lambda * deltaM)/(2 * alpha * M0 * T1b * (exp(-omega/T1b) - exp(-(tau + 
    omega)/T1b)))
  
  return(cbf)
} 
