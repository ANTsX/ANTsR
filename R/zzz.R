.onAttach <- function(lib, pkg) {
  txt <- paste(pkg, utils::packageDescription(pkg, lib)[["Version"]])
  txt <- paste0(
    txt, "\n",
    "Reproducibility issues in some methods. See\n",
    "https://github.com/ANTsX/ANTs/wiki/",
    "antsRegistration-reproducibility-issues\nfor more information.",
    "Also see *repro methods in antsRegistration."
  )
  packageStartupMessage(txt)
}
