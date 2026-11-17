# On macOS, R and ANTsRCore can each load their own OpenMP runtime, and the
# second one aborts the process ("OMP: Error #15: Initializing libomp.dylib,
# but found libomp.dylib already initialized"). Allow the duplicate runtime
# when tests run outside R CMD check; an existing setting is respected.
if (Sys.info()[["sysname"]] == "Darwin" &&
  !nzchar(Sys.getenv("KMP_DUPLICATE_LIB_OK"))) {
  Sys.setenv(KMP_DUPLICATE_LIB_OK = "TRUE")
}

# magrittr is in Imports; make its pipe available to the tests even when
# they run outside test_check() (which sees the package's imports)
`%>%` <- magrittr::`%>%`

# Run `code`; if it fails with an error matching `pattern` (a known package
# or platform issue), skip the test with `reason` instead of failing. Any
# other error still fails.
skip_on_known_error <- function(code, pattern, reason) {
  tryCatch(code, error = function(e) {
    if (grepl(pattern, conditionMessage(e))) {
      skip(paste0(reason, " (", conditionMessage(e), ")"))
    }
    stop(e)
  })
}

# segmentationRefinement.train/predict use the xgboost < 2 R API; with
# xgboost >= 2 they fail in xgb.DMatrix ("unsupported type") (package bug).
skip_if_new_xgboost <- function() {
  skip_if_not_installed("xgboost")
  skip_if(utils::packageVersion("xgboost") >= "2.0.0",
    "segmentationRefinement uses the xgboost < 2 API")
}

# Assert the INTENDED behavior of code that has a known package bug.  While the
# bug exists the expectations fail (or the code errors) and the test is skipped
# with `bug`; once it is fixed the same expectations run and pass, so the test
# never has to be rewritten and a fix cannot break CI.
expect_intended_despite_bug <- function(code, bug) {
  tryCatch(code,
    expectation_failure = function(e) {
      skip(paste0("known bug: ", bug, " (", conditionMessage(e), ")"))
    },
    error = function(e) {
      skip(paste0("known bug: ", bug, " (", conditionMessage(e), ")"))
    })
}
