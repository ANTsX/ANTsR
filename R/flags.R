#' return ANTs binary directory information
#'
#' call this to get the binary paths to ANTs executables.
#'
#' @author Avants BB
#' @examples
#'
#' antsDir()
#'
#' @export antsDir
antsDir <- function() {
  antsd <- paste(system.file("bin",
    package = "ANTsRCore"
  ), sep = "")
  if (!file.exists(antsd)) {
    print("antsDir: ants dir does not exist")
  }
  cat(antsd)
}

#' return ANTs installation information
#'
#' call this to get the include path for ANTs headers and libraries
#'
#' @author Avants BB
#' @examples
#'
#' antsIncludes()
#'
#' @export antsIncludes
antsIncludes <- function() {
  antslocation <- paste(system.file("include",
    package = "ANTsRCore"
  ), "/", sep = "")
  if (!file.exists(antslocation)) {
    print("antsIncludes: ants includes do not exist")
  }
  cat(antslocation)
}

#' return ants library location
#'
#' call this to get the path to ants static libaries to which you will link
#'
#' @author Avants BB
#' @examples
#'
#' antsLibs()
#'
#' @export antsLibs
antsLibs <- function() {
  antslibs <- paste(system.file("libs", package = "ANTsRCore"), "/lib/", sep = "")
  if (!file.exists(antslibs)) {
    # Check for libraries under /lib64
    antslibs <- paste(system.file("libs", package = "ANTsRCore"), "/lib64/", sep = "")
    if (!file.exists(antslibs)) {
      print("antsLibs: ants libs not found")
    }
  }
  cat(antslibs)
}


#' return ants compile flags
#'
#' call this to get the compilation flags used for ants and dependent software
#'
#' @author Avants BB
#' @examples
#'
#' antsCompileFlags()
#'
#' @export antsCompileFlags
antsCompileFlags <- function() {
  cat(" -fPIC -O2  ")
}
