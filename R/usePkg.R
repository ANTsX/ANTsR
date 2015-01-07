#' @name usePkg 
#' @title Use any package.  If package is not installed, this will install from CRAN. 
#' @usage usePkg(packageName)
#' @param packageName Name of package as *string*.  
#' @return T if package successfully loaded, F otherwise. cbf image
#' @author Benjamin M. Kandel
#' @examples
#' usePkg('extremevalues')
usePkg <- function(pkg){
  success <- try(require(pkg, character.only=T))
  if (!success){
    install.packages(pkg, repos='http://cran.r-project.org')
    success <- try(require(pkg, character.only=T))
  }
  success
} 
