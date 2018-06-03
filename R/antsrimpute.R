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
  mostrepeated <- function(x,...) as(names(which.max(table(x))), mode(x))
  if (is.null(dim(mydat))) {
    mydat2 <- mydat
    if ( class( mydat ) == 'numeric' | class( mydat ) == 'integer' )
      mydat2[is.na(mydat)] <- FUN((mydat), na.rm = T, ...) else
        mydat2[is.na(mydat)] <- mostrepeated((mydat), na.rm = T, ...)
    mydat2
  } else {
    mydat2 <- mydat
    for (x in 1:ncol(mydat)) {
      if ( class( mydat[, x] ) == 'numeric' | class( mydat[, x] ) == 'integer' )
        mydat2[is.na(mydat[, x]), x] <-
          FUN((mydat[, x]), na.rm = T, ...) else
          mydat2[is.na(mydat[, x]), x] <-
            mostrepeated((mydat[, x]), na.rm = T, ...)
      }
    return( mydat2 )
    mydat3=mydat2
    for (x in 1:nrow(mydat))
    if ( class( mydat2[x, ] ) == 'numeric' | class( mydat2[x,] ) == 'integer' )
      mydat3[x,is.na(mydat2[x, ])] <-
        FUN((mydat2[x,]), na.rm = T, ...) else mydat3[x,is.na(mydat2[x, ])] <-
          mostrepeated((mydat2[x,]), na.rm = T, ...)
    mydat3
  }
}
