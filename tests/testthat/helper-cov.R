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
