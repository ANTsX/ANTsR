# On Windows, with ITK's default thread pool R never exits after the tests.
# ANTsR's .onLoad sets the Platform threader on Windows; this repeats it
# before ANTsR/ANTsRCore load. An existing setting is respected.
if (.Platform$OS.type == "windows" &&
  !nzchar(Sys.getenv("ITK_GLOBAL_DEFAULT_THREADER"))) {
  Sys.setenv(ITK_GLOBAL_DEFAULT_THREADER = "Platform")
}

# On macOS, R and ANTsRCore can each load their own OpenMP runtime, and the
# second one aborts the process ("OMP: Error #15: Initializing libomp.dylib,
# but found libomp.dylib already initialized"). Allow the duplicate runtime
# for the tests; an existing setting is respected.
if (Sys.info()[["sysname"]] == "Darwin" &&
  !nzchar(Sys.getenv("KMP_DUPLICATE_LIB_OK"))) {
  Sys.setenv(KMP_DUPLICATE_LIB_OK = "TRUE")
}

library(methods)
library(testthat)
library(ANTsR)

test_check("ANTsR")
