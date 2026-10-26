.onAttach <- function(lib, pkg) {
  txt <- paste(pkg, utils::packageDescription(pkg, lib)[["Version"]])
  packageStartupMessage(txt)
}


# Set default options when package is loaded, without overriding any
# values the user has already set
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.ants <- list(
    ants.deterministic = FALSE,
    ants.randomSeed = NULL
  )
  toSet <- !(names(op.ants) %in% names(op))
  if (any(toSet)) {
    options(op.ants[toSet])
  }
  invisible()
}

