# Regression tests for the PR #445 review of taskFMRI() and temporalwhiten()

.taskfmri_inputs <- function(nt = 80, nv = 20, seed = 1) {
  set.seed(seed)
  hrf <- hemodynamicRF(scans = nt, onsets = c(1, 21, 41, 61), durations = 10, rt = 2)
  mat <- outer(as.numeric(hrf), rep(5, nv)) + matrix(rnorm(nt * nv), nt)
  # motion parameter table in the layout of antsMotionCalculation()
  motionparams <- as.data.frame(matrix(rnorm(nt * 14), nt))
  invisible(capture.output(
    pred <- ANTsR:::.get_perfusion_predictors(mat, motionparams, NULL, 1, 3)
  ))
  list(hrf = hrf, mat = mat, pred = pred)
}

test_that("taskFMRI default formula works with the nuisance helper's output", {
  inp <- .taskfmri_inputs()
  # same list structure that getfMRInuisanceVariables() returns
  myvars <- list(
    nuisancevariables = inp$pred$nuis,
    globalsignal = inp$pred$globalsignal
  )
  expect_true(all(grepl("^denoisingParams", colnames(myvars$nuisancevariables))))
  r <- taskFMRI(inp$mat, inp$hrf, myvars)
  df <- data.frame(
    hrf = as.numeric(inp$hrf), globalsignal = myvars$globalsignal,
    myvars$nuisancevariables
  )
  full <- lm(inp$mat[, 3] ~ ., data = df)
  expect_equal(r$tstat[3], coefficients(summary(full))["hrf", 3])
  expect_equal(r$coef[3], unname(coefficients(full)["hrf"]))
  expect_identical(r$beta, r$tstat)

  # motion regressors are used when supplied
  myvars$motion <- inp$pred$motion
  rm <- taskFMRI(inp$mat, inp$hrf, myvars)
  expect_true(all(paste0("motion", 1:3) %in% colnames(rm$adesmat)))
  df$motion1 <- myvars$motion[, 1]
  df$motion2 <- myvars$motion[, 2]
  df$motion3 <- myvars$motion[, 3]
  fullm <- lm(inp$mat[, 3] ~ ., data = df)
  expect_equal(rm$tstat[3], coefficients(summary(fullm))["hrf", 3])
})

test_that("taskFMRI autocorrelation fits the filtered response on the full filtered design", {
  set.seed(2)
  nt <- 120
  hrf <- hemodynamicRF(scans = nt, onsets = seq(1, nt, 30), durations = 12, rt = 2)
  hrf <- hrf / sd(hrf) # unit-variance regressor so the effect is detectable
  h <- as.numeric(hrf)
  # a nuisance regressor strongly correlated with the task
  nuis <- cbind(n1 = 0.9 * scale(h)[, 1] + 0.3 * rnorm(nt))
  noise <- replicate(10, as.numeric(arima.sim(list(ar = 0.6), nt)))
  mat <- outer(h, rep(3, 10)) + noise
  myvars <- list(nuisancevariables = nuis, globalsignal = rnorm(nt))
  suppressMessages(r <- taskFMRI(mat, hrf, myvars, correctautocorr = TRUE))
  # the response is filtered, not residualized on nuisance terms, so the
  # same AR filter applied to the design reproduces it with the full model
  fm <- r$fmrimat
  refit <- summary(lm(fm ~ hrf + globalsignal + n1, data = data.frame(r$adesmat)))
  expect_equal(
    r$tstat,
    unname(vapply(refit, function(s) coefficients(s)["hrf", 3], 1))
  )
  # hrf coefficient remains close to the true value of 3
  expect_lt(abs(mean(r$coef) - 3), 0.75)
  expect_true(all(r$tstat > 2))
})

test_that("temporalwhiten returns whitened residuals", {
  set.seed(3)
  x <- replicate(20, as.numeric(arima.sim(list(ar = 0.7), 300)))
  w <- temporalwhiten(x, 2)
  lag1 <- function(m) mean(apply(m, 2, function(v) acf(v, plot = FALSE)$acf[2]))
  expect_gt(lag1(x), 0.5)
  expect_lt(abs(lag1(w)), 0.1)
  expect_equal(colMeans(w), colMeans(x), tolerance = 0.05)
})

test_that("temporalwhiten handles fewer selected lags and order 0", {
  x <- cbind(rnorm(30), rnorm(30))
  expect_equal(temporalwhiten(x, 0), x)
  expect_false(anyNA(temporalwhiten(x[1:3, , drop = FALSE], 5)))
})
