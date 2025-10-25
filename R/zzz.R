.onAttach <- function(lib, pkg) {
  txt <- paste(pkg, utils::packageDescription(pkg, lib)[["Version"]])
  packageStartupMessage(txt)
}


.onLoad <- function(libname, pkgname) {
  py_file <- system.file("python", "nsa_flow_py.py", package = pkgname)
  if (file.exists(py_file)) {
    assign("nsa_env", reticulate::source_python(py_file, convert = FALSE),
           envir = parent.env(environment()))
  } else {
    warning("Could not find nsa_flow_py.py in package")
  }
}

