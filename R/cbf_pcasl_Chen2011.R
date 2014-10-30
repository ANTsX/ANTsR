# chen 2011 paper pCASL
# --------------------------------------------------------------------------------------
cbf_pcasl_Chen2011 <- function(motionCorrectedImage, mask) {
  # img <- antsImageRead( aslimg_filename , 4 , 'double' )
  
  # moco_results <- motion_correction( img )
  
  # img <- as.array( moco_results$moco_img )
  
  img <- as.array(motionCorrectedImage)
  
  
  numdiffs <- floor(dim(img)[4]/2)
  
  labelimg <- img[, , , seq(2, by = 2, length.out = numdiffs)]
  controlimg <- img[, , , seq(1, by = 2, length.out = numdiffs)]
  
  lambda <- 0.9
  deltaM <- (controlimg - labelimg)
  alpha <- 0.85
  
  M0 <- array(0, dim(controlimg)[1:3])
  for (x in 1:(dim(M0)[1])) for (y in 1:(dim(M0)[2])) for (z in 1:(dim(M0)[3])) {
    M0[x, y, z] <- mean(controlimg[x, y, z, ])
  }
  meanM0 <- M0
  M0 <- rep(M0, numdiffs)
  dim(M0) <- dim(controlimg)
  
  T1b <- 1664
  omega <- 1
  tau <- 1.5
  
  cbf <- (lambda * deltaM)/(2 * alpha * M0 * T1b * (exp(-omega/T1b) - exp(-(tau + 
    omega)/T1b)))
  cbf[is.nan(cbf)] <- 0
  
  meanvalues <- array(0, dim(controlimg)[1:3])
  
  for (x in 1:(dim(M0)[1])) for (y in 1:(dim(M0)[2])) for (z in 1:(dim(M0)[3])) {
    meanvalues[x, y, z] <- 5400 * mean(cbf[x, y, z, ])
  }
  
  meancbf <- antsImageClone(moco_results$moco_avg_img)
  meancbf[!is.nan(meanvalues)] <- meanvalues[!is.nan(meanvalues)]
  
  # mask <- antsImageClone(moco_results$moco_avg_img) mask[mask < 500] <- 0
  # mask[mask > 0] <- 1
  
  # for( x in 1:(dim(meancbf)[1]) ) for( y in 1:(dim(meancbf)[2]) ) for( z in
  # 1:(dim(meancbf)[3]) ) { val <- meancbf[x,y,z] * mask[x,y,z] meancbf[ x , y , z
  # ] <- val }
  meancbf[(mask < 1)] <- 0
  
  return(meancbf)
  # return ( mask )
} 
