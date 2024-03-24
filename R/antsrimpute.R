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
#' mydat <- data.frame(A = c(1, 2, 4, 5), B = c(1, NA, 4, 5))
#' mean.impute <- antsrimpute(mydat)
#' mean.impute <- antsrimpute(mydat$B)
#' median.impute <- antsrimpute(mydat, median)
#'
#' @export antsrimpute
antsrimpute <- function(mydat, FUN = mean, ...) {
  mostrepeated <- function(x, ...) as(names(which.max(table(x))), mode(x))
  if (is.null(dim(mydat))) {
    mydat2 <- mydat
    if (is.numeric(mydat) || is.integer(mydat)) {
      mydat2[is.na(mydat)] <- FUN((mydat), na.rm = TRUE, ...)
    } else {
      mydat2[is.na(mydat)] <- mostrepeated((mydat), na.rm = TRUE, ...)
    }
    mydat2
  } else {
    mydat2 <- mydat
    for (x in 1:ncol(mydat)) {
      if (is.numeric(mydat[, x]) || is.integer(mydat[, x])) {
        mydat2[is.na(mydat[, x]), x] <-
          FUN((mydat[, x]), na.rm = TRUE, ...)
      } else {
        mydat2[is.na(mydat[, x]), x] <-
          mostrepeated((mydat[, x]), na.rm = TRUE, ...)
      }
    }
    return(mydat2)
  }
}
