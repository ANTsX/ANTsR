#' Use any package.  If package is not installed, this will install from CRAN.
#'
#' Use any package.  If package is not installed, this will install from CRAN.
#'
#'
#' @param packageName Name of package as *string*.
#' @return T if package successfully loaded, F otherwise. cbf image
#' @author Benjamin M. Kandel
#' @examples
#'
#' usePkg('randomForest')
#'
#' @export usePkg
usePkg <- function(packageName) {
  # update according to H Wickham's advice
  success<-requireNamespace(packageName, quietly = TRUE)
  if (!success) {
    install.packages(packageName, repos = "http://cran.r-project.org",
      dependencies=FALSE )
    success <- requireNamespace(packageName, character.only = T)
  }
  return(success)
}
