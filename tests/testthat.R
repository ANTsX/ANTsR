# On Windows, with ITK's default thread pool R never exits after the tests.
# ANTsR's .onLoad sets the Platform threader on Windows; this repeats it
# before ANTsR/ANTsRCore load. An existing setting is respected.
if (.Platform$OS.type == "windows" &&
  !nzchar(Sys.getenv("ITK_GLOBAL_DEFAULT_THREADER"))) {
  Sys.setenv(ITK_GLOBAL_DEFAULT_THREADER = "Platform")
}

library(methods)
library(testthat)
library(ANTsR)

test_check("ANTsR")
