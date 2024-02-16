#' Use any package.  If package is not installed, this will install from CRAN.
#'
#' Use any package.  If package is not installed, this will install from CRAN.
#'
#'
#' @param packageName Name of package as *string*.
#' @param allowInstall let the package be installed from CRAN
#' @return TRUE if package successfully loaded, FALSE otherwise.
#' @author Benjamin M. Kandel, BB Avants
#' @examples
#'
#' usePkg("randomForest")
#' usePkg("stats", allowInstall = TRUE)
#'
#' @export usePkg
usePkg <- function(packageName, allowInstall=FALSE ) {
  # extended based on H Wickham's advice
  success <- requireNamespace(packageName, quietly=TRUE)
  if (!success & allowInstall ) {
    install.packages(packageName, repos = "http://cran.r-project.org",
      dependencies = FALSE )
    success <- requireNamespace(packageName, quietly=TRUE)
    attachNamespace(packageName)
  } else {
    temp <- tryCatch({attachNamespace(packageName)},
      error = function(e) {
      }, finally = {
      })
  }
  return(success)
}
