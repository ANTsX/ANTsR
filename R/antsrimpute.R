#' antsrimpute
#'
#' Impute NA's on data frame.
#'
#' Imputes NA's on data frame (column-wise), using a user-specified function
#' (mean is default).  also works on a vector.
#'
#' @param mydat Data frame to be imputed.
#' @param FUN Method to be used for imputation (defaults to mean).
#' @param ... Additional parameters to pass to FUN.
#' @return Returns mydat with NA's replaced by imputed numbers.
#' @author Kandel BM, Avants BB
#' @examples
#'
#' mydat <- data.frame(A=c(1,2,4,5), B=c(1,NA,4,5))
#' mean.impute <- antsrimpute(mydat)
#' median.impute <- antsrimpute(mydat, median)
#'
#' @export antsrimpute
antsrimpute <- function(mydat, FUN = mean, ...) {
  if (is.null(dim(mydat))) {
    mydat2 <- mydat
    mydat2[is.na(mydat)] <- FUN(as.numeric(mydat), na.rm = T, ...)
    mydat2
  } else {
    mydat2 <- mydat
    for (x in 1:ncol(mydat))
      mydat2[is.na(mydat[, x]), x] <-
        FUN(as.numeric(mydat[, x]), na.rm = T, ...)
    mydat3=mydat2
    for (x in 1:nrow(mydat))
      mydat3[x,is.na(mydat2[x, ])] <-
        FUN(as.numeric(mydat2[x,]), na.rm = T, ...)
    mydat3
  }
}
