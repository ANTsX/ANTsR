# NewtorkCorrelation.R From - van Wijk, B.C.M., Stam, C.J., Daffertshofer, A.: Comparing brain networks of
# different size and connectivity density using graph theory. PLoS One 5(10) (2010) e13701

networkCorrelation <- function(n1, n2) {
  
  if (sum(abs(dim(n1) - dim(n2))) != 0) {
    print("Inputs must be of same dimension")
    return(NULL)
  }
  
  cov <- NetworkCovariance(n1, n2)
  
  var1 <- var(n1[upper.tri(n1)])
  var2 <- var(n2[upper.tri(n2)])
  
  cor <- cov/sqrt(var1 * var2)
  
  return(cor)
} 
