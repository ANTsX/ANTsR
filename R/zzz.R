.onAttach <- function(lib, pkg) {
  txt <- paste(pkg, utils::packageDescription(pkg, lib)[["Version"]])
  txt <- paste0(
    txt, "\n",
    "Environment variables ",
    "set either in .Renviron or with a seed (e.g. XXX): ",
    "\nSys.setenv(ANTS_RANDOM_SEED = XXX)\n",
    "Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = 1)\nmay ",
    "influence reproducibility in some methods. See\n",
    "https://github.com/ANTsX/ANTs/wiki/",
    "antsRegistration-reproducibility-issues\nfor more information.",
    "Also see *repro methods in antsRegistration."
  )
  packageStartupMessage(txt)
  }
