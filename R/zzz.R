.onAttach <- function(lib, pkg) {
  txt <- paste(pkg, utils::packageDescription(pkg, lib)[["Version"]])
  packageStartupMessage(txt)
}


.onLoad <- function(libname, pkgname) {
  trace_file <- Sys.getenv("ANTSR_TRACE_FILE")
  if (nzchar(trace_file)) {
    .start_expr_trace(trace_file)
  }
  invisible()
}

# Debugging aid for CI: when ANTSR_TRACE_FILE is set, append every completed
# top-level expression to that file with a timestamp and its duration. The
# file is reopened for each line, so nothing is lost to buffering when a
# process hangs and is killed; the hung expression is the one after the last
# line logged (e.g. the next line of ANTsR-Ex.R under R CMD check).
.start_expr_trace <- function(file) {
  last <- Sys.time()
  stamp <- function(t) format(t, "%Y-%m-%d %H:%M:%OS3")
  log_line <- function(...) {
    try(cat(..., "\n", sep = "", file = file, append = TRUE), silent = TRUE)
  }
  log_line(stamp(last), " TRACE START pid ", Sys.getpid(), " ", commandArgs()[1])
  addTaskCallback(function(expr, value, ok, visible) {
    now <- Sys.time()
    txt <- paste(deparse(expr, width.cutoff = 200L, nlines = 3L), collapse = " ")
    log_line(
      stamp(now),
      sprintf(" %9.2fs ", as.numeric(difftime(now, last, units = "secs"))),
      if (!ok) "FAILED ",
      substr(txt, 1L, 300L)
    )
    last <<- now
    TRUE
  }, name = "antsr_expr_trace")
  invisible()
}
