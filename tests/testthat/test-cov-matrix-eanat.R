# Coverage tests for eigenanatomy / sparse CCA wrappers and related helpers

eanat_mat <- function(n = 20, p = 30, seed = 21) {
  set.seed(seed)
  z <- matrix(rnorm(n * 2), n, 2)
  load <- matrix(0, 2, p)
  load[1, 1:10] <- 1
  load[2, 11:20] <- 1
  z %*% load + matrix(rnorm(n * p, sd = 0.3), n, p)
}

quiet <- function(expr) {
  out <- NULL
  utils::capture.output(out <- suppressMessages(suppressWarnings(expr)))
  out
}

test_that("sparseDecom returns sparse eigenanatomy with and without mask", {
  mat <- eanat_mat()
  r <- sparseDecom(mat, nvecs = 2, sparseness = 0.2, its = 2, cthresh = 0)
  expect_equal(dim(r$eigenanatomyimages), c(2, 30))
  expect_equal(dim(r$umatrix), c(20, 2))
  expect_true(all(rowMeans(r$eigenanatomyimages != 0) <= 0.4))
  mask <- makeImage(c(5, 6), 1)
  r2 <- quiet(sparseDecom(mat, inmask = mask, nvecs = 2, sparseness = 0.2,
    its = 2, cthresh = 0, verbose = TRUE, robust = 1))
  expect_equal(dim(r2$Reconstruction), dim(mat))
  expect_true(r2$varex > 0 && r2$varex <= 1)
  expect_error(sparseDecom(mat, inmask = makeImage(c(5, 5), 1), nvecs = 2),
    "mask size")
  expect_equal(quiet(sparseDecom()), 0)
})

test_that("sparseDecom2 finds correlated sparse canonical variates", {
  mat <- eanat_mat()
  set.seed(22)
  mat2 <- cbind(mat[, 1:3] + matrix(rnorm(60, sd = 0.2), 20, 3),
    matrix(rnorm(20 * 9), 20, 9))
  s2 <- sparseDecom2(list(mat, mat2), nvecs = 2, sparseness = c(0.2, 0.2), its = 3)
  expect_equal(dim(s2$eig1), c(30, 2))
  expect_equal(dim(s2$eig2), c(12, 2))
  expect_equal(colnames(s2$projections), c("Variate000", "Variate001"))
  expect_gt(abs(s2$ccasummary$corrs[1]), 0.5)
  mask <- makeImage(c(5, 6), 1)
  s3 <- sparseDecom2(list(mat, mat2), inmask = list(mask, NA), nvecs = 2,
    sparseness = c(0.2, 0.2), its = 2, perms = 2, robust = 1, rejector = 0.01)
  expect_true(all(s3$ccasummary$pvalues >= 0 & s3$ccasummary$pvalues <= 1))
  s4 <- sparseDecom2(list(mat, mat2), inmask = list(NA, mask * 0 + 1), nvecs = 1,
    sparseness = c(-0.2, -0.2), its = 2)
  expect_length(s4$eig1, 30)
  expect_error(sparseDecom2(list(mat, mat2[1:10, ]), nvecs = 2), "same number of rows")
  expect_error(sparseDecom2(list(mat, mat2), inmask = list(5, NULL)),
    "maskx")
  expect_error(sparseDecom2(list(mat, mat2), inmask = list(NULL, 5)),
    "masky")
})

test_that("sparseDecomboot bootstraps eigenanatomy", {
  mat <- eanat_mat(seed = 23)
  set.seed(24)
  b <- quiet(sparseDecomboot(mat, nvecs = 2, sparseness = 0.2, its = 2,
    cthresh = 0, nboot = 2))
  expect_equal(dim(b$eigenanatomyimages), c(2, 30))
  expect_length(b$bootccalist1, 2)
  expect_equal(dim(b$bootccalist1[[1]]), c(2, 30))
  b2 <- quiet(sparseDecomboot(mat, nvecs = 2, sparseness = 0.2, its = 2,
    cthresh = 0, nboot = 2, nsamp = 1, doseg = FALSE))
  expect_equal(dim(b2$cca1outAuto), c(2, 30))
  # with a mask the function references an undefined variable (bug)
  expect_error(quiet(sparseDecomboot(mat, inmask = makeImage(c(5, 6), 1),
    nvecs = 2, sparseness = 0.2, its = 2, cthresh = 0, nboot = 2)),
    "usefakemask")
})

test_that("sparseDecom2boot bootstraps sparse CCA", {
  mat <- eanat_mat(seed = 25)
  set.seed(26)
  mat2 <- cbind(mat[, 1:4], matrix(rnorm(20 * 8), 20, 8))
  bb <- quiet(sparseDecom2boot(list(mat, mat2), nvecs = 2,
    sparseness = c(0.2, 0.2), its = 2, nboot = 2, verbose = TRUE))
  expect_named(bb, c("bootsccan", "eig1", "eig2", "bootccalist1", "bootccalist2",
    "allmat1", "allmat2", "init1", "init2", "jh"))
  expect_equal(dim(bb$allmat1), c(30, 4))
  expect_equal(dim(bb$jh), c(30, 12))
  expect_true(all(bb$jh >= 0))
  bb2 <- quiet(sparseDecom2boot(list(mat, mat2), nvecs = 2,
    sparseness = c(-0.3, 0.3), its = 2, nboot = 2, nsamp = 0.8, doseg = TRUE,
    estimateSparseness = 0))
  expect_equal(dim(bb2$eig1), c(30, 2))
  expect_true(all(colSums(bb2$eig1 != 0) <= 10))
  expect_equal(quiet(sparseDecom2boot()), 0)
})

test_that("eanatSelect and eanatDef produce sparse pseudo-eigenvectors", {
  mat <- eanat_mat(seed = 27)
  nv <- quiet(eanatSelect(mat, maxNEvec = 4, selectorScale = 1.2))
  expect_true(nv >= 1 && nv <= 4)
  pdf(NULL)
  nv2 <- quiet(eanatSelect(mat, maxNEvec = 4, cthresh = 2, whiten = TRUE,
    selectorScale = 0.5, verbose = TRUE))
  dev.off()
  expect_true(is.numeric(nv2))
  expect_error(eanatSelect(mat, mask = makeImage(c(4, 4), 1)), "Mask must match")
  e <- quiet(eanatDef(mat, nvecs = 2, its = 2))
  expect_equal(dim(e), c(2, 30))
  expect_true(all(rowSums(e != 0) < 30))
  expect_equal(sqrt(rowSums(e^2)), c(1, 1), tolerance = 1e-6)
  mask <- makeImage(c(5, 6), 1)
  e2 <- quiet(eanatDef(mat, nvecs = 2, mask = mask, its = 2, smoother = 0.5,
    cthresh = 2, positivity = TRUE, verbose = TRUE))
  expect_true(all(e2 >= 0))
  set.seed(28)
  pr <- matrix(abs(rnorm(60)), 2, 30)
  e3 <- quiet(eanatDef(mat, mask = mask, its = 2, priors = pr, priorWeight = 0.5,
    nvecs = 50))
  expect_equal(dim(e3), c(2, 30))
  e4 <- quiet(eanatDef(mat, nvecs = 2, its = 1, whiten = TRUE))
  expect_equal(dim(e4), c(2, 30))
  expect_error(eanatDef(mat), "Must set nvecs")
  expect_error(eanatDef(mat, nvecs = 2, mask = makeImage(c(3, 3), 1)), "Mask must")
  # .hyperButt directly: sparseness target and full-density passthrough
  hb <- ANTsR:::.hyperButt
  set.seed(29)
  v <- rnorm(30)
  expect_equal(c(hb(v, 1)), c(if (sum(v < 0) > sum(v > 0)) -v else v))
  hv <- hb(v, 0.3)
  expect_lt(mean(hv != 0), 0.6)
  hv2 <- quiet(hb(abs(v), 0.2, mask = mask, smoother = 1, clustval = 3,
    verbose = TRUE))
  expect_length(hv2, 30)
})

test_that("networkEiganat decomposes data and validates mask handling", {
  mat <- eanat_mat(seed = 30)
  ne <- skip_on_known_error(
    quiet(networkEiganat(mat, nvecs = 2, its = 2, verbose = TRUE)),
    "infinite or missing values",
    "networkEiganat produces non-finite values with this BLAS"
  )
  expect_equal(dim(ne$u), c(20, 2))
  expect_equal(dim(ne$v), c(30, 2))
  expect_equal(dim(ne$myrecon), dim(mat))
  set.seed(31)
  ne2 <- quiet(networkEiganat(mat, nvecs = 2, its = 2, sparseness = c(1, -0.3),
    useregression = FALSE, downsample = 10, dowhite = 5,
    prior = matrix(rnorm(60), 30, 2), verbose = TRUE))
  expect_equal(nrow(ne2$X), 5)
  expect_true(all(colSums(ne2$v != 0) <= 9))
  # supplying an antsImage mask fails inside .eanatsparsify (bug)
  expect_error(networkEiganat(mat, nvecs = 2, its = 1,
    mask = makeImage(c(5, 6), 1)), "not interpretable as logical")
  # .eanatsparsifyv with positive sparseness keeps the largest entries
  sv <- ANTsR:::.eanatsparsifyv(matrix(c(5, 4, 3, 2, 1, -1, 0.5, 0.1), ncol = 1),
    0.5, verbose = TRUE) |> quiet()
  expect_equal(sum(sv != 0), 3)
  expect_equal(sv[1:3], c(5, 4, 3))
})

test_that("joinEigenanatomy merges components via graph communities", {
  skip_if_not_installed("igraph")
  mat <- eanat_mat(seed = 32)
  dec <- sparseDecom(mat, nvecs = 4, sparseness = 0.2, its = 2, cthresh = 0)
  je <- quiet(joinEigenanatomy(mat, mask = NULL, dec$eigenanatomyimages,
    c(0.2, 0.9), verbose = TRUE))
  expect_true(nrow(je$fusedlist) <= 4)
  expect_length(je$memberships, 4)
  expect_equal(ncol(je$fusedproj), nrow(je$fusedlist))
  mask <- makeImage(c(5, 6), 1)
  eimgs <- quiet(matrixToImages(-abs(dec$eigenanatomyimages), mask))
  je2 <- quiet(joinEigenanatomy(mat, mask = mask, eimgs, 0.9))
  expect_true(all(sapply(je2$fusedlist, is.antsImage)))
  expect_equal(je2$bestdensity, 0.9)
  expect_equal(quiet(joinEigenanatomy()), 1)
})

test_that("initializeEigenanatomy and eigSeg build initialisations", {
  set.seed(33)
  il <- initializeEigenanatomy(matrix(rnorm(2 * 30), 2, 30))
  expect_length(il$initlist, 2)
  expect_equal(il$enames, c("init1", "init2"))
  mask <- makeImage(c(5, 6), 1)
  lab <- makeImage(c(5, 6), rep(1:3, 10))
  il2 <- initializeEigenanatomy(lab, mask = mask, nreps = 2, smoothing = 0.5)
  expect_length(il2$initlist, 6)
  il3 <- initializeEigenanatomy(lab)
  expect_length(il3$initlist, 3)
  ilist <- quiet(matrixToImages(matrix(rnorm(3 * 30), 3, 30), mask))
  seg <- quiet(eigSeg(mask, ilist, TRUE, cthresh = 2))
  expect_true(all(seg[mask == 1] %in% 0:3))
  seg2 <- eigSeg(mask, matrix(rnorm(3 * 30), 3, 30), smooth = 0)
  expect_true(all(seg2[mask == 1] %in% 1:3))
  expect_equal(quiet(eigSeg(NULL)), 1)
  expect_error(eigSeg(mask, "a"), "wrong input type")
})

test_that("regressProjections predicts outcomes from eigenvector projections", {
  set.seed(34)
  mask <- makeImage(c(5, 6), 1)
  mat <- matrix(rnorm(40 * 30), 40, 30)
  ev <- matrix(rnorm(3 * 30), 3, 30)
  y <- as.numeric(scale(mat) %*% ev[1, ]) + rnorm(40, sd = 0.5)
  dtr <- data.frame(y = y[1:25], age = rnorm(25))
  dte <- data.frame(y = y[26:40], age = rnorm(15))
  r <- quiet(regressProjections(mat[1:25, ], mat[26:40, ], dtr, dte, ev, mask, "y",
    covariates = c("1", "age")))
  expect_gt(r$stats$corcoeff.test, 0.5)
  expect_length(r$eigenvectors, 3)
  r2 <- quiet(regressProjections(mat[1:25, ], mat[26:40, ], dtr, dte,
    quiet(matrixToImages(ev, mask)), mask, "y", which.eigenvectors = "optimal"))
  expect_true("eigvec1" %in% names(r2$eigenvectors))
  dtr$g <- factor(ifelse(dtr$y > 0, "a", "b"))
  dte$g <- factor(ifelse(dte$y > 0, "a", "b"))
  r3 <- quiet(regressProjections(mat[1:25, ], mat[26:40, ], dtr, dte, ev, mask,
    "g", family = binomial))
  expect_true(r3$stats$error.train >= 0 && r3$stats$error.train <= 1)
  expect_s3_class(r3$outcome.comparison$predicted, "factor")
  expect_error(quiet(regressProjections(mat[1:25, ], mat[26:40, ], dtr, dte, ev,
    mask, "y", which.eigenvectors = "bad")), "optimal")
})

test_that("multichannelPCA decomposes multichannel images", {
  old <- Sys.getenv("ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS", unset = NA)
  Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = "1")
  on.exit(if (is.na(old)) Sys.unsetenv("ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS") else
    Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = old), add = TRUE)
  set.seed(35)
  mk <- makeImage(c(4, 4), 1)
  # multichannelPCA calls gc() once per image, so keep the inputs tiny
  imgs <- lapply(1:3, function(i) mergeChannels(list(
    makeImage(c(4, 4), rnorm(16)), makeImage(c(4, 4), rnorm(16)))))
  v <- multichannelToVector(imgs[[1]], mk)
  expect_length(v, 32)
  back <- vectorToMultichannel(v, mk)
  expect_equal(back@components, 2)
  expect_equal(multichannelToVector(back, mk), v, tolerance = 1e-6)
  expect_error(vectorToMultichannel(1:5, mk), "dimensions")
  m1 <- quiet(multichannelPCA(imgs, mk, k = 1, pcaOption = "randPCA",
    verbose = TRUE))
  expect_length(m1$pcaWarps, 1)
  expect_equal(m1$pcaWarps[[1]]@components, 2)
  m4 <- quiet(multichannelPCA(imgs, mk, k = 1, pcaOption = 2, sigma = 10,
    center = FALSE, auxiliaryModality = matrix(rnorm(3 * 32), 3, 32)))
  expect_length(m4$pcaWarps, 1)
  # rsvd::rrpca has no `k` argument (bug)
  expect_error(quiet(multichannelPCA(imgs, mk, k = 1, pcaOption = "rrPCAL")),
    "unused argument")
  expect_error(multichannelPCA(imgs, makeImage(c(3, 3), 1)), "dimensionality")
})

test_that("randomized svd / whitening / cca helpers", {
  set.seed(36)
  A <- matrix(rnorm(20 * 15), 20, 15)
  s <- randsvd(A, 3, seed = 1)
  expect_length(s$d, 3)
  expect_equal(dim(s$u), c(20, 3))
  expect_true(all(s$d <= svd(A)$d[1] + 1e-8))
  w <- randwhiten(A, 3, seed = 1)
  expect_equal(dim(w), c(3, 15))
  cc <- randcca(A, matrix(rnorm(20 * 10), 20, 10), 3, seed = 1)
  expect_equal(dim(cc$whitened), c(3, 10))
  q <- ANTsR:::.gramschmidt(A[, 1:3])
  expect_equal(crossprod(q), diag(3), tolerance = 1e-8)
})

test_that("nsa learning-rate estimation and retraction helpers", {
  set.seed(37)
  Y0 <- matrix(abs(rnorm(30)), 10, 3)
  X0 <- Y0 + 0.1
  for (m in c("brent", "grid", "armijo", "golden", "adaptive")) {
    r <- quiet(estimate_learning_rate_nsa(Y0, X0, method = m, verbose = TRUE,
      plot = m == "grid"))
    expect_true(r$estimated_learning_rate > 0)
  }
  expect_equal(estimate_learning_rate_nsa(Y0, X0, method = "default")$estimated_learning_rate,
    0.001)
  expect_equal(estimate_learning_rate_nsa(Y0, X0, method = "default",
    apply_nonneg = FALSE)$estimated_learning_rate, 1)
  r <- estimate_learning_rate_nsa(Y0, X0, method = "grid", apply_nonneg = FALSE,
    verbose = FALSE, plot = FALSE, nsa_energy = function(V) sum(V^2))
  expect_true(is.numeric(r$estimated_learning_rate))
  P <- nsa_flow_retract_auto(Y0, 1, "polar")
  cp <- crossprod(P)
  expect_lt(max(abs(cp - diag(diag(cp)))), 1e-8)
  expect_equal(sqrt(sum(P^2)), sqrt(sum(Y0^2)), tolerance = 1e-8)
  expect_equal(dim(nsa_flow_retract_auto(Y0, 0.5, "soft_polar")), c(10, 3))
  expect_identical(nsa_flow_retract_auto(Y0, 0.5, "none"), Y0)
  expect_error(nsa_flow_retract_auto(1:3, 0.5, "polar"), "matrix")
})

test_that("column-winner and sum-preserving sparsifiers", {
  mat <- matrix(c(-5, 0.1, 0.2, 0.3, 0.4, 1, 2.0, 0.1, 0.2, 0.3,
    -1, -0.2, -3, -0.4, -0.5, 1, -2, 3, -4, 5), 5, 4)
  r1 <- sparsify_by_column_winner(mat, "either", "negative",
    ensure_row_membership = FALSE)
  expect_equal(colSums(r1 != 0), c(1, 0, 1, 1))
  expect_equal(r1[1, 1], -5)
  r2 <- sparsify_by_column_winner(mat, "positive", "positive")
  expect_true(all(rowSums(r2 != 0) >= 1))
  r3 <- sparsify_by_column_winner(mat, "none", "either")
  expect_equal(r3[, 1], mat[, 1])
  expect_identical(sparsify_by_column_winner(matrix(numeric(0), 0, 2), "none",
    "either"), matrix(numeric(0), 0, 2))
  s1 <- sum_preserving_matrix_partition(mat, 1)
  # each row keeps a single entry and rows end up with equal absolute sums
  expect_true(all(rowSums(s1 != 0) == 1))
  expect_equal(length(unique(round(rowSums(abs(s1)), 6))), 1)
  s2 <- sum_preserving_matrix_partition(mat, 2)
  expect_equal(dim(s2), dim(mat))
  expect_error(sum_preserving_matrix_partition(mat, 3), "Invalid option")
  o <- orthogonalizeAndQSparsify(mat, sparsenessAlg = "spmp", positivity = "either")
  expect_equal(dim(o), dim(mat))
})
