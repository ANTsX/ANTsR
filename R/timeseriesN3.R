#' Run N3 on slices of timeseries.
#'
#' Repeatedly run N3 on slices of time series and return image
#'
#'
#' @param boldimg input matrix
#' @param mask mask to use
#' @param ncorrections levels at which to apply n3
#' @return antsImage is output
#' @author Avants BB
#' @examples
#'
#'   bold<-makeImage( c(10,10,10,4), rnorm(4000) )
#'   mask<-getMask( getAverageOfTimeSeries( bold ) )
#'   boldn3<-timeseriesN3( bold, mask, c(4,2) )
#'
#' @export timeseriesN3
timeseriesN3 <- function(boldimg, mask, ncorrections = c(4, 2, 2)) {
  dim <- 4
  ismatrix <- TRUE
  if (class(boldimg) != "matrix") {
    dim <- boldimg@dimension
    if (dim != 4) {
      return(NA)
    }
    mat <- timeseries2matrix(boldimg, mask)
    ismatrix <- FALSE
  }
  if (class(boldimg) == "matrix") {
    mat <- boldimg
  }
  for (i in 1:nrow(mat)) {
    perf <- makeImage(mask, mat[i, ])
    for (nc in ncorrections) N3BiasFieldCorrection(dim - 1, perf, perf, nc)
    mat[i, ] <- perf[mask == 1]
  }
  if (ismatrix)
    return(mat)
  if (!ismatrix)
    return(matrix2timeseries(boldimg, mask, mat))
}
