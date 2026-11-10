# Run every Rd example in a child R process, logging each top-level
# expression to a progress file before evaluating it. An example that runs
# longer than `per_example` seconds is killed, recorded as hung with the
# expression it was stuck on, and the run resumes with the next example.
#
# Usage: Rscript .github/scripts/time-examples.R [per_example_seconds]

args <- commandArgs(trailingOnly = TRUE)
per_example <- if (length(args)) as.numeric(args[[1]]) else 300

ex_dir <- file.path(tempdir(), "examples")
dir.create(ex_dir, showWarnings = FALSE)
for (rd in sort(list.files("man", pattern = "\\.Rd$", full.names = TRUE))) {
  out <- file.path(ex_dir, sub("\\.Rd$", ".R", basename(rd)))
  tools::Rd2ex(rd, out, commentDontrun = TRUE, commentDonttest = TRUE)
}
examples <- sort(list.files(ex_dir, pattern = "\\.R$", full.names = TRUE))
cat(length(examples), "examples\n")

progress <- file.path(tempdir(), "progress.txt")
child <- file.path(tempdir(), "child.R")
writeLines(c(
  'a <- commandArgs(trailingOnly = TRUE)',
  'progress <- a[[1]]; files <- readLines(a[[2]])',
  'log <- function(...) cat(..., "\\n", sep = "", file = progress, append = TRUE)',
  'suppressPackageStartupMessages(library(ANTsR))',
  'for (f in files) {',
  '  log("START ", basename(f))',
  '  t0 <- proc.time()[["elapsed"]]',
  '  env <- new.env(parent = globalenv())',
  '  res <- tryCatch({',
  '    for (e in parse(f, keep.source = FALSE)) {',
  '      log("EXPR ", paste(deparse(e, width.cutoff = 200L), collapse = " "))',
  '      eval(e, env)',
  '    }',
  '    "ok"',
  '  }, error = function(err) paste("error:", conditionMessage(err)))',
  '  grDevices::graphics.off()',
  '  log("END ", basename(f), "\\t", round(proc.time()[["elapsed"]] - t0, 2), "\\t", res)',
  '}'
), child)

rscript <- file.path(R.home("bin"), "Rscript")
remaining <- examples
results <- list()
while (length(remaining)) {
  unlink(progress)
  todo <- tempfile(fileext = ".txt")
  writeLines(remaining, todo)
  p <- processx::process$new(rscript, c(child, progress, todo),
    stdout = "", stderr = "2>&1"
  )
  started <- Sys.time()
  current <- NULL
  killed <- FALSE
  repeat {
    p$wait(2000)
    lines <- if (file.exists(progress)) readLines(progress, warn = FALSE) else character()
    starts <- grep("^START ", lines)
    now <- if (length(starts)) sub("^START ", "", lines[max(starts)]) else NULL
    if (!identical(now, current)) {
      current <- now
      started <- Sys.time()
    }
    if (!p$is_alive()) break
    if (as.numeric(difftime(Sys.time(), started, units = "secs")) > per_example) {
      p$kill_tree()
      killed <- TRUE
      break
    }
  }
  ends <- grep("^END ", lines, value = TRUE)
  for (x in ends) {
    f <- strsplit(sub("^END ", "", x), "\t")[[1]]
    results[[f[1]]] <- data.frame(example = f[1], seconds = as.numeric(f[2]), status = f[3])
  }
  done <- sub("\t.*", "", sub("^END ", "", ends))
  if (!is.null(current) && !current %in% done) {
    last_expr <- tail(grep("^EXPR ", lines[max(starts):length(lines)], value = TRUE), 1)
    status <- paste(
      if (killed) sprintf("HUNG (> %ss)", per_example) else
        sprintf("CRASHED (exit %s)", p$get_exit_status()),
      "at:", sub("^EXPR ", "", last_expr)
    )
    cat("\n>>>", current, status, "\n\n")
    results[[current]] <- data.frame(example = current, seconds = NA, status = status)
    done <- c(done, current)
  }
  remaining <- setdiff(remaining, file.path(ex_dir, done))
  if (!length(done)) break
}

res <- do.call(rbind, results)
res <- res[order(-ifelse(is.na(res$seconds), Inf, res$seconds)), ]
rownames(res) <- NULL
cat("\n==== Examples not ok or > 5s ====\n")
print(res[res$status != "ok" | is.na(res$seconds) | res$seconds > 5, ], right = FALSE)
cat("\nTotal examples:", nrow(res), " total seconds:", sum(res$seconds, na.rm = TRUE), "\n")
summary_file <- Sys.getenv("GITHUB_STEP_SUMMARY")
if (nzchar(summary_file)) {
  md <- c("| example | seconds | status |", "|---|---|---|",
    sprintf("| %s | %s | %s |", res$example, res$seconds, gsub("\\|", "\\\\|", res$status)))
  writeLines(c("### Windows example timings", "", md), summary_file)
}
