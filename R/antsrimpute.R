#' Impute NA's on data frame.
#'
#' Imputes NA's on data frame (column-wise), using a user-specified function
#' (mean is default).  also works on a vector.
#'
#' %% ~~ If necessary, more details than the description above ~~
#'
#' @param mydat Data frame to be imputed.
#' @param FUN Method to be used for imputation (defaults to mean).
#' @param ... Additional parameters to pass to FUN.
#' @return Returns mydat with NA's replaced by imputed numbers.
#' @note %% ~~further notes~~
#' @author Kandel BM, Avants BB %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
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
    for (x in 1:ncol(mydat)) mydat2[is.na(mydat[, x]), x] <- FUN(as.numeric(mydat[,
      x]), na.rm = T, ...)
    mydat2
  }
}
