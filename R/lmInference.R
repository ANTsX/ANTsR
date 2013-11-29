lmInference <- function(mylm) {
  beta <- mylm$coefficients[-1, ]
  myresponse <- model.response(model.frame(mylm))
  X <- model.matrix(mylm)
  dfr <- dim(X)[2] - 1
  dfe <- dim(X)[1] - dfr - 1
  if(is.vector(myresponse)){
    msm <- sum((t(t(mylm$fitted.values) - mean(myresponse)))^2) / dfr
    mse <- sum((mylm$residuals)^2) / dfe
    fstat <- msm / mse
  } else{
    msm <- colSums(( t(t(mylm$fitted.values) - colMeans(myresponse)))^2) / dfr
    mse <- colSums((mylm$residuals)^2) / dfe
    fstat <- msm / mse    
  }
  pval.model <- pf(fstat, dfr, dfe, lower.tail=F)
  XtXinv <- solve(t(X) %*% X)
  if(dim(X)[2] > 2){
    mycoefs <- diag(XtXinv[2:dim(X)[2], 2:dim(X)[2]])
  } else {
    mycoefs <- XtXinv[2,2]
  }
  if(is.vector(mylm$residuals)) {
    beta.std <- sqrt(sum((mylm$residuals)^2) / 
                       mylm$df.residual * mycoefs)   
  } else {
    beta.std <- t(sqrt( as.vector(colSums((mylm$residuals)^2) / 
                       mylm$df.residual) %o% mycoefs))
  }
  beta.t <- mylm$coefficients[-1, ] / beta.std
  beta.pval <- 2*pt(-abs(beta.t),df=mylm$df.residual)
  list(fstat=fstat, pval.model=pval.model, 
       beta=beta, beta.std=beta.std, beta.t=beta.t, beta.pval=beta.pval)
}
