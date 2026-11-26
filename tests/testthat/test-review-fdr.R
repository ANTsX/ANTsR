context("topological FDR thresholds (statFieldThresh / rftResults)")

# Gaussian field with nothing above the primary height except a few peaks
make_peak_field <- function(peaks = list(c(10, 10, 10, 4.5))) {
  set.seed(1)
  arr <- array(rnorm(20^3, sd = 0.5), dim = c(20, 20, 20))
  for (p in peaks) {
    arr[p[1], p[2], p[3]] <- p[4]
  }
  as.antsImage(arr)
}
res20 <- resels(makeImage(c(20, 20, 20), 1), c(1, 1, 1))

test_that("peak FDR never returns a height below the primary threshold", {
  z <- make_peak_field()
  primary <- qnorm(0.001, lower.tail = FALSE)
  u <- statFieldThresh(z, .05, 1, 1, c(1, 1, 1), res20, c(1, 1), "Z", "pFDR",
    pp = 0.001
  )
  expect_gte(u, primary)
  # the threshold is the observed height of the passing peak
  expect_equal(u, 4.5, tolerance = 1e-6)
})

test_that("peak FDR uses BH on conditional peak p-values, inclusive", {
  z <- make_peak_field(list(
    c(5, 5, 5, 6), c(15, 15, 15, 4.5), c(5, 15, 5, 3.2)
  ))
  primary <- qnorm(0.001, lower.tail = FALSE)
  Eu <- rftPval(3, 1, 0, primary, 1, res20, c(1, 1), "Z")$Ec
  peaks <- c(6, 4.5, 3.2)
  ps <- sapply(peaks, function(h) rftPval(3, 1, 0, h, 1, res20, c(1, 1), "Z")$Ec) / Eu
  pass <- which(sort(ps) <= seq_along(ps) / 3 * .05)
  expected <- if (length(pass)) peaks[order(ps)][max(pass)] else NA
  u <- tryCatch(
    statFieldThresh(z, .05, 1, 1, c(1, 1, 1), res20, c(1, 1), "Z", "pFDR"),
    error = function(e) NA
  )
  expect_equal(as.numeric(u), expected, tolerance = 1e-6)
  # a peak below the primary height is never a candidate
  expect_false(isTRUE(u < primary))
})

test_that("cluster FDR keeps the primary height and returns an extent", {
  arr <- array(0.1, dim = c(20, 20, 20))
  arr[3:7, 3:7, 3:7] <- 5 # 125-voxel cluster
  arr[15, 15, 15] <- 5 # single voxel
  z <- as.antsImage(arr)
  u <- statFieldThresh(z, .05, 1, 1, c(1, 1, 1), res20, c(1, 1), "Z", "cFDR")
  expect_equal(as.numeric(u), qnorm(0.001, lower.tail = FALSE))
  k <- attr(u, "k")
  expect_true(is.numeric(k) && k >= 1)
  expect_true(k <= 125)
  # rftResults drops clusters below the FDR extent
  out <- rftResults(z, res20, c(1, 1, 1), c(1, 1), "Z", threshType = "cFDR")
  expect_true(all(out$ClusterStats[, "Voxels"] >= k))
  expect_equal(as.numeric(out$threshold), as.numeric(u))
})

test_that("rftResults peak Z stays finite for strong peaks", {
  z <- make_peak_field(list(c(10, 10, 10, 10)))
  out <- rftResults(z, res20, c(1, 1, 1), c(1, 1), "Z", threshType = 3)
  expect_true(all(is.finite(out$PeakStats[, "Z"])))
  expect_equal(max(out$PeakStats[, "Z"]), 10, tolerance = 1e-3)
  # T field: the comparable Z matches the upper-tail conversion
  tout <- rftResults(z, res20, c(1, 1, 1), c(1, 20), "T", threshType = 3)
  expect_equal(
    max(tout$PeakStats[, "Z"]),
    round(qnorm(pt(10, 20, lower.tail = FALSE), lower.tail = FALSE), 4)
  )
  expect_true(all(is.finite(tout$PeakStats[, "Z"])))
})
