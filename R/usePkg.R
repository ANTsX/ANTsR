#' Use any package.  If package is not installed, this will install from CRAN.
#'
#' Use any package.  If package is not installed, this will install from CRAN.
#'
#'
#' @param packageName Name of package as *string*.
#' @return T if package successfully loaded, F otherwise. cbf image
#' @author Benjamin M. Kandel, BB Avants
#' @examples
#'
#' usePkg('randomForest')
#'
#' @export usePkg
usePkg <- function(packageName) {
  # extended based on H Wickham's advice
  success<-requireNamespace(packageName)
  if (!success) {
    install.packages(packageName, repos = "http://cran.r-project.org",
      dependencies=FALSE )
    success <- requireNamespace(packageName)
    attachNamespace(packageName)
  } else {
    temp<-tryCatch({attachNamespace(packageName)},
      error = function(e) {
      }, finally = {
      })
  }
  return(success)
}
