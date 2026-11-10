.onAttach <- function(lib, pkg) {
  txt <- paste(pkg, utils::packageDescription(pkg, lib)[["Version"]])
  packageStartupMessage(txt)
}


# Set default options when the package is loaded, without overriding values
# the user has already set. This is the only .onLoad: a second definition in
# another file would silently replace it (the later-collated file wins).
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

