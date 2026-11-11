# Coverage tests for nsa_flow PCA / FA variants and milr / mild

nsa_lowrank <- function(n = 30, p = 10, k = 2, seed = 11) {
  set.seed(seed)
  u <- matrix(rnorm(n * k), n, k)
  v <- matrix(rnorm(p * k), p, k)
  x <- u %*% t(v) + matrix(rnorm(n * p, sd = 0.2), n, p)
  colnames(x) <- paste0("v", seq_len(p))
  x
}

# a cheap proximal operator with the nsa_flow return contract
identity_prox <- function(Y0, w = 0.5, ...) list(Y = abs(Y0) + 1e-3)

test_that("nsa_flow_pca basic proximal explains variance", {
  x <- nsa_lowrank()
  out <- capture.output(res <- nsa_flow_pca(x, k = 2, max_iter = 4, verbose = TRUE))
  expect_equal(dim(res$Y), c(10, 2))
  expect_equal(length(res$energy_trace), res$final_iter)
  expect_true(res$expl_var_ratio > 0 && res$expl_var_ratio <= 1 + 1e-8)
  expect_true(any(grepl("Iter", out)))
  res1 <- nsa_flow_pca(x, k = 1, max_iter = 3)
  expect_equal(dim(res1$Y), c(10, 1))
  res2 <- nsa_flow_pca(x, k = 2, max_iter = 3, proximal_type = "nsa_flow",
    nsa_flow_fn = identity_prox, verbose = TRUE) |> suppressMessages() |>
    capture.output() |> invisible()
  res2 <- nsa_flow_pca(x, k = 2, max_iter = 3, proximal_type = "nsa_flow",
    nsa_flow_fn = identity_prox)
  expect_true(all(res2$Y >= 0 & res2$Y <= 1))
  # long run triggers the plateau / lr reduction logic
  res3 <- nsa_flow_pca(x, k = 2, max_iter = 60, proximal_type = "nsa_flow",
    nsa_flow_fn = identity_prox)
  expect_lte(res3$final_iter, 60)
  expect_error(nsa_flow_pca(x, k = 0), "k must be")
  expect_error(nsa_flow_pca(x, k = 2, lambda = -1), "lambda")
  expect_error(nsa_flow_pca(x, k = 2, alpha = 0), "alpha")
  expect_error(nsa_flow_pca(x, k = 2, w_pca = 0), "w_pca")
  expect_error(nsa_flow_pca(x, k = 2, nsa_w = 2), "nsa_w")
  expect_error(nsa_flow_pca(data.frame(x), k = 2), "finite numeric matrix")
  expect_error(nsa_flow_pca(matrix(1, 5, 4), k = 2), "variance")
  bad_prox <- function(Y0, w) list(Z = Y0)
  expect_error(nsa_flow_pca(x, k = 2, max_iter = 2, proximal_type = "nsa_flow",
    nsa_flow_fn = bad_prox), "unexpected")
})

test_that("nsa_flow_pca_fa supports pca/fa objectives and rotations", {
  x <- nsa_lowrank()
  out <- capture.output(res <- nsa_flow_pca_fa(x, k = 2, max_iter = 4,
    lambda = 0.5, verbose = TRUE))
  expect_equal(dim(res$loadings), c(10, 2))
  expect_equal(colnames(res$loadings), c("PA1", "PA2"))
  expect_equal(rownames(res$loadings), colnames(x))
  expect_equal(nrow(res$scores), 30)
  expect_length(res$communalities, 10)
  for (rot in c("varimax", "promax", "oblimin")) {
    r <- nsa_flow_pca_fa(x, k = 2, max_iter = 2, objective = "fa", rotate = rot)
    expect_equal(dim(r$loadings), c(10, 2))
    expect_true(all(is.finite(r$energy_trace)))
  }
  r1 <- nsa_flow_pca_fa(x, k = 1, max_iter = 3, lambda = 0)
  expect_equal(ncol(r1$loadings), 1)
  r2 <- suppressMessages(nsa_flow_pca_fa(x, k = 2, max_iter = 3,
    proximal_type = "nsa_flow", nsa_flow_fn = identity_prox, verbose = TRUE)) |>
    capture.output() |> invisible()
  r2 <- nsa_flow_pca_fa(x, k = 2, max_iter = 40, proximal_type = "nsa_flow",
    nsa_flow_fn = identity_prox, nsa_flow_args = list(w = 0.2))
  expect_lte(r2$final_iter, 40)
  expect_error(nsa_flow_pca_fa(x, k = 2, proximal_type = "nsa_flow"), "nsa_flow_fn")
  expect_error(nsa_flow_pca_fa(x, k = 2, max_iter = 2, proximal_type = "nsa_flow",
    nsa_flow_fn = function(Y0) stop("boom")), "nsa_flow_fn failed")
  expect_error(nsa_flow_pca_fa(x, k = 2, max_iter = 2, proximal_type = "nsa_flow",
    nsa_flow_fn = function(Y0) list(Q = Y0)), "unexpected")
  expect_error(nsa_flow_pca_fa(x, k = 20), "k must be")
  expect_error(nsa_flow_pca_fa(x, k = 2, lambda = -1), "lambda")
  expect_error(nsa_flow_pca_fa(x, k = 2, alpha = -1), "alpha")
  expect_error(nsa_flow_pca_fa(x, k = 2, w_pca = -1), "w_pca")
  expect_error(nsa_flow_pca_fa(matrix(1, 6, 4), k = 2), "variance")
  expect_error(nsa_flow_pca_fa(data.frame(x), k = 2), "finite")
})

test_that("nsa_flow_pca_fa_2 alternates FA updates", {
  x <- nsa_lowrank(seed = 12)
  out <- capture.output(res <- suppressMessages(nsa_flow_pca_fa_2(x, k = 2,
    max_iter = 3, objective = "fa", lambda = 0.5, verbose = TRUE)))
  expect_equal(dim(res$loadings), c(10, 2))
  expect_true(all(res$uniqueness <= 1 + 1e-6))
  rp <- nsa_flow_pca_fa_2(x, k = 2, max_iter = 3, objective = "pca", lambda = 0)
  expect_true(rp$expl_var_ratio > 0.5)
  # k = 1: diag(scalar) builds a d x d identity (bug: non-conformable)
  expect_error(nsa_flow_pca_fa_2(x, k = 1, max_iter = 2, objective = "pca"),
    "non-conformable")
  for (rot in c("varimax", "promax", "oblimin")) {
    r <- nsa_flow_pca_fa_2(x, k = 2, max_iter = 2, objective = "fa", rotate = rot)
    expect_equal(dim(r$loadings), c(10, 2))
  }
  r2 <- nsa_flow_pca_fa_2(x, k = 2, max_iter = 30, proximal_type = "nsa_flow",
    nsa_flow_fn = identity_prox, objective = "fa")
  expect_lte(r2$final_iter, 30)
  r3 <- suppressMessages(nsa_flow_pca_fa_2(x, k = 2, max_iter = 2,
    proximal_type = "nsa_flow", nsa_flow_fn = identity_prox, verbose = TRUE)) |>
    capture.output() |> invisible()
  expect_error(nsa_flow_pca_fa_2(x, k = 2, proximal_type = "nsa_flow"), "nsa_flow_fn")
  expect_error(nsa_flow_pca_fa_2(x, k = 2, max_iter = 2, proximal_type = "nsa_flow",
    nsa_flow_fn = function(Y0) stop("boom")), "nsa_flow_fn failed")
  expect_error(nsa_flow_pca_fa_2(x, k = 2, max_iter = 2, proximal_type = "nsa_flow",
    nsa_flow_fn = function(Y0) list(Q = Y0)), "unexpected")
  expect_error(nsa_flow_pca_fa_2(x, k = 0), "k must be")
  expect_error(nsa_flow_pca_fa_2(x, k = 2, lambda = -1), "lambda")
  expect_error(nsa_flow_pca_fa_2(x, k = 2, alpha = 0), "alpha")
  expect_error(nsa_flow_pca_fa_2(x, k = 2, w_pca = 0), "w_pca")
  expect_error(nsa_flow_pca_fa_2(matrix(2, 6, 4), k = 2), "variance")
  expect_error(nsa_flow_pca_fa_2(list(1), k = 2), "finite")
})

test_that("posthoc_fa_summary computes communalities and scores", {
  x <- nsa_lowrank(seed = 13)
  ld <- svd(scale(x))$v[, 1:2]
  for (m in c("PAF", "NSA", "ML")) {
    s <- posthoc_fa_summary(x, ld, method = m)
    expect_length(s$communalities, 10)
    expect_equal(dim(s$scores), c(30, 2))
  }
  sc <- matrix(rnorm(60), 30, 2)
  s2 <- posthoc_fa_summary(data.frame(x), data.frame(ld), scores = sc)
  expect_equal(s2$scores, sc, ignore_attr = TRUE)
})

milr_data <- function(seed = 14, n = 20, p = 6) {
  set.seed(seed)
  vimg <- matrix(rnorm(n * p), n, p)
  dd <- data.frame(age = rnorm(n), grp = rnorm(n))
  outc <- vimg * 0.5 + dd$age + matrix(rnorm(n * p, sd = 0.1), n, p)
  list(dd = dd, mats = list(vimg = vimg, outc = outc), n = n, p = p)
}

test_that("milr and milr.predict fit voxelwise models", {
  md <- milr_data()
  out <- capture.output(res <- milr(md$dd, md$mats, "outc ~ age + vimg",
    iterations = 3, gamma = 1e-2, verbose = TRUE))
  expect_equal(dim(res$v), c(md$p, 3))
  expect_equal(colnames(res$v), c("(Intercept)", "age", "vimg"))
  expect_equal(dim(res$prediction), c(md$n, md$p))
  expect_true(all(is.finite(res$v)))
  expect_true(all(is.finite(res$prediction)))
  rres <- milr(md$dd, md$mats, "outc ~ age + vimg", iterations = 2,
    gamma = 1e-2, sparsenessQuantile = 0.5, positivity = "either",
    repeatedMeasures = rep(1:10, each = 2), orthogonalize = TRUE)
  expect_equal(nrow(rres$vRan), md$p)
  expect_equal(ncol(rres$vRan), 10)
  # constant (non-matrix) outcome
  md$dd$y <- md$dd$age + rnorm(md$n)
  cres <- milr(md$dd, md$mats, "y ~ vimg", iterations = 2, gamma = 1e-3,
    positivity = TRUE)
  expect_equal(colnames(cres$v), c("(Intercept)", "vimg"))
  tst <- milr_data(seed = 15)
  tst$dd$y <- rnorm(tst$n)
  pr <- milr.predict(cres, md$dd, md$mats, tst$dd, tst$mats, "y ~ vimg")
  expect_length(pr$predictionTest, tst$n)
  expect_length(pr$predictionTrain, md$n)
  pr2 <- milr.predict(res, md$dd, md$mats, tst$dd, tst$mats, "outc ~ age + vimg")
  expect_equal(dim(pr2$lowDimensionalProjectionTest), c(tst$n, 3))
  expect_true(is.na(pr2$predictionTest))
  expect_error(milr(md$dd, unname(md$mats), "outc ~ age"), "name")
  expect_error(milr(md$dd, md$mats, "outc ~ age", positivity = "zz"), "positivity")
  expect_error(milr(md$dd, md$mats, "outc ~ age", repeatedMeasures = 1:3),
    "repeatedMeasures")
  bad <- md$mats
  bad$outc <- bad$outc[, 1:3]
  expect_error(milr(md$dd, bad, "outc ~ age"), "does not have")
  expect_error(milr.predict(res, md$dd, unname(md$mats), tst$dd, tst$mats,
    "outc ~ age"), "name")
  expect_error(milr.predict(res, md$dd, bad, tst$dd, tst$mats, "outc ~ age"),
    "train matrix")
  expect_error(milr.predict(res, md$dd, md$mats, tst$dd, bad, "outc ~ age"),
    "test matrix")
})

test_that("mild learns a low-dimensional basis", {
  md <- milr_data(seed = 16)
  out <- capture.output(res <- mild(md$dd, md$mats, basisK = 2,
    myFormulaK = "outc ~ age + mildBasis1 + mildBasis2", iterations = 3,
    gamma = 1e-2, positivity = "either", verbose = TRUE))
  expect_equal(dim(res$v), c(md$p, 4))
  expect_true(all(c("mildBasis1", "mildBasis2") %in% colnames(res$u)))
  # basis columns are re-orthogonalised
  b <- res$u[, c("mildBasis1", "mildBasis2")]
  expect_lt(abs(sum(b[, 1] * b[, 2])), 1e-8)
  init <- matrix(rnorm(md$n * 2), md$n, 2)
  rres <- mild(md$dd, md$mats, basisK = 2,
    myFormulaK = "outc ~ vimg + mildBasis1 + mildBasis2", iterations = 2,
    gamma = 1e-2, positivity = TRUE, initializationStrategy = init,
    repeatedMeasures = rep(1:10, each = 2), orthogonalize = TRUE,
    smoothingMatrix = diag(md$p))
  expect_equal(ncol(rres$vRan), 10)
  md$dd$y <- rnorm(md$n)
  # a non-matrix outcome is not supported (bug: indexes voxmats by a
  # data-frame column number)
  expect_error(mild(md$dd, md$mats, basisK = 1, myFormulaK = "y ~ mildBasis1",
    iterations = 2, positivity = "positive"), "subscript out of bounds")
  expect_error(mild(md$dd, unname(md$mats), 1, "outc ~ mildBasis1"), "name")
  expect_error(mild(md$dd, md$mats, 1, "outc ~ mildBasis1", positivity = "zz"),
    "positivity")
  expect_error(mild(md$dd, md$mats, 1, "outc ~ mildBasis1",
    initializationStrategy = "x"), "initializationStrategy")
  expect_error(mild(md$dd, md$mats, 1, "outc ~ mildBasis1",
    repeatedMeasures = 1:2), "repeatedMeasures")
  bad <- md$mats
  bad$vimg <- bad$vimg[, 1:2]
  expect_error(mild(md$dd, bad, 1, "outc ~ mildBasis1"), "does not have")
})

test_that("ilr, ilr.predict and bigLMStats give consistent regression stats", {
  md <- milr_data(seed = 17)
  res <- ilr(md$dd, md$mats, "outc ~ age + vimg")
  expect_equal(dim(res$estimate), c(3, md$p))
  # compare voxel 1 against lm
  fit <- lm(md$mats$outc[, 1] ~ md$dd$age + md$mats$vimg[, 1])
  expect_equal(unname(res$estimate[, 1]), unname(coef(fit)), tolerance = 1e-6)
  pr <- ilr.predict(res, md$dd, md$mats, "outc ~ age + vimg")
  expect_equal(pr, res$predictions, tolerance = 1e-6)
  expect_error(ilr(md$dd, unname(md$mats), "outc ~ age"), "name")
  expect_error(ilr.predict(res, md$dd, unname(md$mats), "outc ~ age"), "name")
  bad <- md$mats
  bad$vimg <- bad$vimg[, 1:2]
  expect_error(ilr(md$dd, bad, "outc ~ age"), "does not have")
  expect_error(ilr.predict(res, md$dd, bad, "outc ~ age"), "does not have")
  # bigLMStats: vector response with and without intercept
  set.seed(18)
  df <- data.frame(a = rnorm(40), b = rnorm(40))
  df$y <- 2 * df$a + rnorm(40)
  m1 <- lm(y ~ a + b, data = df)
  s1 <- bigLMStats(m1)
  expect_equal(unname(s1$beta.t), unname(summary(m1)$coefficients[-1, 3]),
    tolerance = 1e-6)
  s2 <- bigLMStats(m1, includeIntercept = TRUE)
  expect_length(s2$beta.pval, 3)
  m0 <- lm(y ~ a, data = df)
  s0 <- bigLMStats(m0)
  expect_equal(unname(s0$beta.t), summary(m0)$coefficients[2, 3], tolerance = 1e-6)
  ymat <- cbind(df$y, rnorm(40))
  m2 <- lm(ymat ~ a + b, data = df)
  s3 <- bigLMStats(m2)
  expect_equal(dim(s3$beta.pval), c(2, 2))
  expect_lt(s3$beta.pval[1, 1], 0.001)
  s4 <- bigLMStats(m2, includeIntercept = TRUE, lambda = 0.01)
  expect_equal(dim(s4$beta.t), c(3, 2))
})
