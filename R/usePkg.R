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
usePkg <- function(pkg) {
  success <- try(require(pkg, character.only = T))
  if (!success) {
    install.packages(pkg, repos = "http://cran.r-project.org")
    success <- try(require(pkg, character.only = T))
  }
}
