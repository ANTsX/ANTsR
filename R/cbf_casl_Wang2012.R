# Wang 2012 paper CASL ; paper does not mention the parameters so using them form
# the reference [25] mentioned in the paper
# --------------------------------------------------------------------------------------
cbf_casl_Wang2012 <- function(aslimg_filename, Xvar = NULL, Xideal = NULL, c = NULL) {
  Y <- as.array(antsImageRead(aslimg_filename, 4, "double"))
  dimY <- dim(Y)
  dim(Y) <- c(dimY[1] * dimY[2] * dimY[3], dimY[4])
  Y <- t(Y)
  
  if (is.null(Xideal)) {
    Xideal <- c(-0.5, 0.5)
    Xideal <- rep(Xideal, length.out = dimY[4])
  } else {
    if (length(Xideal) != dimY[4]) {
      print("Xideal has length incompatible with ASL image")
      return(NULL)
    }
  }
  
  if (!is.null(Xvar) && dim(Xvar)[1] != dimY[4]) {
    print("Xvar has rows incompatible with ASL image")
    return(NULL)
  }
  
  if (is.null(Xvar)) {
    cbfmodel <- lm(Y ~ Xideal)
  } else {
    cbfmodel <- lm(Y ~ Xideal + Xvar)
  }
  Bideal <- (cbfmodel$coefficients)[2, ]
  dim(Bideal) <- dimY[1:3]
  
  lambda <- 0.9  # stolen
  T1a <- 1.6  # stolen
  R1a <- 1/T1a  # stolen
  w <- 1
  alpha <- 0.95  # stolen
  tau <- 2
  
  if (is.null(c)) {
    c <- colMeans(Y)
    dim(c) <- dimY[1:3]
  }
  if (is.character(c)) {
    c <- as.array(antsImageRead(c, 3, "double"))
  }
  
  cbf <- (Bideal * lambda * R1a * exp(w * R1a))/(2 * c * alpha * (1 - exp(-tau * 
    R1a)))
  
  return(cbf)
} 
