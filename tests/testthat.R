# On Windows, with ITK's default thread pool the tests pass but R never exits
# afterwards (seen in R CMD check on GitHub Actions; r-universe and CRAN
# checks run the same way). The Platform threader keeps no persistent worker
# threads, so the process can shut down. ITK reads this when it first creates
# its threader, so it must be set before ANTsR/ANTsRCore are loaded; an
# existing setting is respected.
if (.Platform$OS.type == "windows" &&
  !nzchar(Sys.getenv("ITK_GLOBAL_DEFAULT_THREADER"))) {
  Sys.setenv(ITK_GLOBAL_DEFAULT_THREADER = "Platform")
}

library(methods)
library(testthat)
library(ANTsR)

test_check("ANTsR")
