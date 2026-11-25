test_that("pcasl CBF matches the consensus equation with T1blood as a rate", {
  m0 <- makeImage(c(2, 2, 1), 100)
  mask <- makeImage(c(2, 2, 1), 1)
  perf <- makeImage(c(2, 2, 1), 1)
  R1 <- 0.67 # 1/sec
  pars <- list(
    sequence = "pcasl", m0 = m0, T1blood = R1, omega = 1,
    slicetime = 0, tau = 1.5, alpha = 0.85, lambda = 0.9
  )
  cbf <- quantifyCBF(perf, mask, pars)
  T1 <- 1 / R1
  # Alsop et al. (2015), eq. 1, with PLD = 1 s and tau = 1.5 s
  alsop <- 6000 * 0.9 * 1 * exp(1 / T1) /
    (2 * 0.85 * T1 * 100 * (1 - exp(-1.5 / T1)))
  expect_equal(as.numeric(as.array(cbf$meancbf)), rep(alsop, 4),
    tolerance = 1e-5
  )
  expect_equal(alsop, 65.6052, tolerance = 1e-5)
})

test_that("pasl uses TI2 when given, otherwise TI1 + delaytime", {
  m0 <- makeImage(c(2, 2, 1), 100)
  mask <- makeImage(c(2, 2, 1), 1)
  perf <- makeImage(c(2, 2, 1), 1)
  base <- list(sequence = "pasl", m0 = m0, slicetime = 0)
  capture.output({
    def <- quantifyCBF(perf, mask, base)
    same <- quantifyCBF(perf, mask, c(base, list(TI2 = 700 + 800)))
    later <- quantifyCBF(perf, mask, c(base, list(TI2 = 1700)))
  })
  expect_equal(as.array(def$meancbf), as.array(same$meancbf))
  # CBF scales with exp(TI2 / T1b), T1b = 1150 ms
  expect_equal(
    as.array(later$meancbf) / as.array(def$meancbf),
    array(exp(200 / 1150), dim(as.array(def$meancbf))),
    tolerance = 1e-5
  )
})

test_that("scor never removes the final frame", {
  set.seed(1)
  dims <- c(4, 4, 4, 2)
  vals <- array(500, dim = dims)
  vals[, , , 1] <- 500 + rnorm(64, sd = 50) # high spatial variance
  vals[, , , 2] <- 500 + rnorm(64, sd = 0.1) # nearly flat
  asl <- makeImage(dims, vals)
  mask <- makeImage(dims[1:3], 1)
  out <- aslCensoring(asl, mask, method = "scor")
  expect_equal(out$which.outliers, 1L)
})

test_that("scor stops when no frame has a valid correlation", {
  dims <- c(4, 4, 4, 4)
  asl <- makeImage(dims, array(500, dim = dims)) # constant: cor() is NA
  mask <- makeImage(dims[1:3], 1)
  out <- aslCensoring(asl, mask, method = "scor")
  expect_length(out$which.outliers, 0)
})

# every voxel has a negative cross-validated R2 at every noise level
negativeR2 <- function(x, y, ngroups = 5, covariates = NA, fast = FALSE) {
  matrix(-1, nrow = 2, ncol = ncol(x))
}

test_that("ordered combineNuisancePredictors handles no positive R2", {
  local_mocked_bindings(crossvalidatedR2 = negativeR2, .package = "ANTsR")
  set.seed(2)
  inmat <- matrix(rnorm(200), 20)
  preds <- matrix(rnorm(60), 20)
  expect_warning(
    noise <- combineNuisancePredictors(inmat, rnorm(20), preds,
      maxpreds = 3, ordered = TRUE
    ),
    "zero predictors"
  )
  expect_equal(dim(noise), c(20L, 0L))
  expect_warning(
    noise <- combineNuisancePredictors(inmat, rnorm(20), preds,
      maxpreds = 3, ordered = FALSE
    ),
    "zero predictors"
  )
  expect_equal(dim(noise), c(20L, 0L))
})

test_that("aslDenoiseR falls back to the fewest noise predictors", {
  set.seed(3)
  aslmat <- matrix(rnorm(20 * 50) + 500, 20)
  tc <- rep(c(0, 1), 10)
  # find the noise pool from the real cross-validation, then mock the
  # per-noise-level R2 so that none is positive
  real <- crossvalidatedR2
  ncall <- 0
  local_mocked_bindings(crossvalidatedR2 = function(x, y, ngroups = 5,
                                                    covariates = NA,
                                                    fast = FALSE) {
    ncall <<- ncall + 1
    if (ncall == 1) {
      r <- real(x, y, ngroups, covariates, fast)
      r[] <- seq(-100, 100, length.out = length(r))
      return(r)
    }
    negativeR2(x, y)
  }, .package = "ANTsR")
  expect_warning(
    dnz <- aslDenoiseR(aslmat, tc,
      maxnoisepreds = 3:2, polydegree = 2,
      crossvalidationgroups = 2, selectionthresh = 0.2
    ),
    "smallest value of maxnoisepreds"
  )
  expect_equal(dnz$n, 2)
  expect_equal(dim(dnz$noiseu), c(20L, 2L))
})
