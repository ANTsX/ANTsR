# Coverage tests for simlr(), backup_simlr(), simlrU(), initializeSimlr(),
# optimal_simlr_initializer(), plot_energy_decomposition() and a few
# optimizer step methods that are not dispatched by simlr() itself.

.cov_simlr_data <- function(n = 30, p = c(10, 12), k = 2, seed = 11, sd = 0.2) {
  set.seed(seed)
  U <- matrix(rnorm(n * k), n, k)
  ml <- lapply(seq_along(p), function(i) {
    m <- U %*% matrix(rnorm(k * p[i]), k, p[i]) +
      matrix(rnorm(n * p[i], sd = sd), n, p[i])
    colnames(m) <- paste0("m", i, "f", seq_len(p[i]))
    m
  })
  names(ml) <- letters[seq_along(p)]
  list(ml = ml, U = U)
}

.quiet <- function(expr) {
  out <- NULL
  invisible(utils::capture.output(
    out <- suppressWarnings(suppressMessages(expr))
  ))
  out
}

test_that("simlr handles NA/constant columns, chained scalings, domain priors and verbose output", {
  d <- .cov_simlr_data()
  ml <- d$ml
  ml$a[3, 2] <- NA         # forces imputation
  ml$b[, 4] <- 5           # zero variance column
  k <- 2
  dom <- lapply(ml, function(m) matrix(runif(2 * ncol(m)), 2, ncol(m)))
  set.seed(1)
  initU <- matrix(rnorm(nrow(ml$a) * k), ncol = k)
  res <- .quiet(simlr(
    ml,
    iterations = 4,
    initialUMatrix = initU,
    scale = c("norm", "np", "sqrtnp", "center", "centerAndScale",
              "eigenvalue", "robust", "whiten"),
    energyType = "acc",
    mixAlg = "svd",
    constraint = "orthox0.1x1",
    optimizationStyle = "adam",
    domainMatrices = dom,
    domainLambdas = c(0.5, 0.5),
    randomSeed = 3,
    verbose = 2
  ))
  expect_named(res$v, names(ml))
  expect_equal(dim(res$v$a), c(ncol(ml$a), k))
  expect_equal(dim(res$v$b), c(ncol(ml$b), k))
  # whitening drops column names, so simlr falls back to generic feature names
  expect_equal(rownames(res$v$b), paste0("x", seq_len(ncol(ml$b))))
  expect_equal(colnames(res$v$a), c("PC1", "PC2"))
  expect_equal(res$domainLambdas, c(0.5, 0.5))
  expect_true(all(is.finite(res$energyPath$total_energy)))
  expect_true(any(res$energyPath$domain_energy_raw != 0))
  expect_true(all(c("similarity_energy", "domain_energy", "feature_orthogonality") %in%
                    names(res$energyPath)))
  expect_identical(res$provenance$constraint, "orthox0.1x1")
  expect_true(is.matrix(res$initialRandomMatrix))
  # provenance records the chained scalings
  expect_true(any(grepl("prescaling", names(res$provenance))))
  expect_equal(res$constraint[1], "ortho")
})

test_that("simlr supports separate initializations, provided vmats, connectors and orthogonalized U", {
  d <- .cov_simlr_data(p = c(10, 12, 8))
  ml <- d$ml
  k <- 2
  vm <- lapply(ml, function(m) matrix(rnorm(ncol(m) * k), ncol(m), k))
  res <- .quiet(simlr(
    ml,
    iterations = 3,
    initialUMatrix = k,
    jointInitialization = FALSE,
    vmats = vm,
    connectors = list(2:3, c(1, 3), 1:2),
    positivities = "either",
    sparsenessQuantiles = 0.3,
    energyType = "regression",
    mixAlg = "svd",
    scale = "center",
    constraint = "none",
    orthogonalizeU = TRUE,
    optimizationStyle = "gd",
    verbose = FALSE
  ))
  expect_length(res$u, 3)
  for (u in res$u) {
    expect_equal(dim(u), c(nrow(ml$a), k))
    # Gram-Schmidt output has orthonormal columns
    expect_equal(crossprod(u), diag(k), tolerance = 1e-6, ignore_attr = TRUE)
  }
  expect_equal(res$connectors, list(2:3, c(1, 3), 1:2))
  expect_identical(res$energyType, "regression")
  expect_true(is.finite(res$finalError))
})

test_that("simlr energy aliases and mixing algorithms run and report finite energies", {
  d <- .cov_simlr_data(n = 24, p = c(8, 9), seed = 5)
  cfgs <- list(
    list(energyType = "nc", mixAlg = "ica", scale = "rank"),
    list(energyType = "lrr", mixAlg = "newton-schulz", scale = "none"),
    list(energyType = "exp", mixAlg = "svd", scale = c("center", "lowrank")),
    list(energyType = "logcosh", mixAlg = "ica-newton", scale = "impute"),
    list(energyType = "kurtosis", mixAlg = "rrpca-l", scale = "centerAndScale"),
    list(energyType = "gauss", mixAlg = "rrpca-s", scale = "center")
  )
  expected <- c("normalized_correlation", "lowRankRegression", "exp", "logcosh", "kurtosis", "gauss")
  for (i in seq_along(cfgs)) {
    res <- .quiet(do.call(simlr, c(list(d$ml, initialUMatrix = 2L, iterations = 2,
                                         constraint = "none", optimizationStyle = "adam"),
                                    cfgs[[i]])))
    expect_identical(res$energyType, expected[i])
    expect_identical(res$mixAlg, cfgs[[i]]$mixAlg)
    expect_true(all(is.finite(res$energyPath$total_energy)))
    expect_equal(ncol(res$v[[1]]), 2)
  }
})

test_that("simlr reports informative errors on bad inputs", {
  d <- .cov_simlr_data(n = 20, p = c(6, 7))
  expect_error(
    simlr(d$ml, initialUMatrix = 2L, iterations = 2, positivities = "bogus",
          energyType = "acc", mixAlg = "svd", optimizationStyle = "adam"),
    "positivities must be one of"
  )
  smo <- list(diag(6), matrix(NA, 7, 7))
  expect_error(
    simlr(d$ml, smo, initialUMatrix = 2L, iterations = 2,
          energyType = "acc", mixAlg = "svd", optimizationStyle = "adam"),
    "smoothing-matrix 2 is null or NA"
  )
  expect_error(
    simlr(d$ml, initialUMatrix = 2L, iterations = 2, optimizationStyle = "not_an_optimizer",
          energyType = "acc", mixAlg = "svd")
  )
  # 'ucca' is accepted by match.arg but has no energy implementation
  expect_error(
    .quiet(simlr(d$ml, initialUMatrix = 2L, iterations = 2, energyType = "ucca",
                 mixAlg = "svd", constraint = "none", optimizationStyle = "adam")),
    "Unknown energy_type"
  )
  # mixAlg = 'avg' currently fails inside simlrU (nc undefined) - package bug
  expect_error(
    .quiet(simlr(d$ml, initialUMatrix = 2L, iterations = 2, energyType = "acc",
                 mixAlg = "avg", constraint = "none", optimizationStyle = "adam")),
    "nc"
  )
})

test_that("backup_simlr fits embeddings across scalings, constraints and options", {
  d <- .cov_simlr_data(p = c(10, 12, 8))
  ml <- d$ml
  ml$a[2, 2] <- NA
  ml$c[, 3] <- 1
  k <- 2
  dom <- lapply(ml, function(m) matrix(runif(2 * ncol(m)), 2, ncol(m)))
  r1 <- .quiet(backup_simlr(
    ml,
    iterations = 4,
    initialUMatrix = matrix(rnorm(nrow(ml$a) * k), ncol = k),
    scale = c("norm", "np", "sqrtnp", "center", "centerAndScale",
              "eigenvalue", "robust", "whiten"),
    energyType = "acc",
    mixAlg = "svd",
    constraint = "orthox0.1x1",
    optimizationStyle = "adam",
    domainMatrices = dom,
    domainLambdas = c(0.2, 0.2, 0.2),
    randomSeed = 1,
    verbose = 2
  ))
  expect_named(r1$v, names(ml))
  expect_equal(dim(r1$v$c), c(ncol(ml$c), k))
  expect_true(all(is.finite(r1$energyPath$total_energy)))
  expect_equal(r1$domainLambdas, c(0.2, 0.2, 0.2))

  vm <- lapply(ml, function(m) matrix(rnorm(ncol(m) * k), ncol(m), k))
  r2 <- .quiet(backup_simlr(
    d$ml,
    iterations = 3,
    initialUMatrix = k,
    jointInitialization = FALSE,
    vmats = vm,
    connectors = list(2:3, c(1, 3), 1:2),
    positivities = "either",
    sparsenessQuantiles = 0.3,
    energyType = "regression",
    mixAlg = "ica",
    scale = "center",
    constraint = "none",
    orthogonalizeU = TRUE,
    optimizationStyle = "gd"
  ))
  for (u in r2$u) {
    expect_equal(crossprod(u), diag(k), tolerance = 1e-6, ignore_attr = TRUE)
  }
  expect_identical(r2$energyType, "regression")

  r3 <- .quiet(backup_simlr(d$ml[1:2], iterations = 3, initialUMatrix = 2L,
                            energyType = "nc", mixAlg = "newton-schulz",
                            scale = "rank", constraint = "none",
                            optimizationStyle = "lars"))
  expect_identical(r3$energyType, "normalized_correlation")
  expect_true(is.finite(r3$finalError))

  expect_error(
    backup_simlr(d$ml, initialUMatrix = 2L, iterations = 2, positivities = "bogus",
                 energyType = "acc", mixAlg = "svd", optimizationStyle = "adam"),
    "positivities must be one of"
  )
  expect_error(
    backup_simlr(d$ml[1:2], list(diag(10), matrix(NA, 12, 12)), initialUMatrix = 2L,
                 iterations = 2, energyType = "acc", mixAlg = "svd",
                 optimizationStyle = "adam"),
    "smoothing-matrix 2"
  )
})

test_that("simlrU mixes projections with every supported algorithm", {
  set.seed(7)
  n <- 25; k <- 2
  Z <- matrix(rnorm(n * k), n, k)
  proj <- lapply(1:3, function(i) Z + matrix(rnorm(n * k, sd = 0.1), n, k))
  for (alg in c("svd", "pca", "ica", "rrpca-l", "rrpca-s", "newton-schulz", "ica-newton")) {
    out <- .quiet(simlrU(proj, alg, initialW = NULL))
    expect_length(out, 3)
    expect_equal(dim(out[[1]]), c(n, k))
    expect_equal(colnames(out[[1]]), c("PC1", "PC2"))
    # each mixed basis spans the shared latent space
    cc <- cancor(out[[1]], Z)$cor
    expect_gt(min(cc), 0.8)
  }
  # ica with a valid (k x k) un-mixing init: needs a single connector per modality
  outw <- .quiet(simlrU(proj, "ica", initialW = diag(k), connectors = list(2, 3, 1)))
  expect_equal(dim(outw[[2]]), c(n, k))
  outp <- .quiet(simlrU(proj, "ica", initialW = proj))
  expect_equal(dim(outp[[3]]), c(n, k))
  # orthogonalize + connectors + expBeta smoothing toward previous U
  out2 <- .quiet(simlrU(proj, "svd", initialW = proj, orthogonalize = TRUE,
                        connectors = list(2, 3, 1), expBeta = 0.5))
  for (u in out2) {
    expect_equal(crossprod(u), diag(k), tolerance = 1e-6, ignore_attr = TRUE)
  }
  expect_error(simlrU(proj, "svd", NULL, connectors = list(2, 1)),
               "length\\( connectors \\)")
  # 'avg' and 'stochastic' reference an undefined variable (package bug)
  expect_error(simlrU(proj, "avg", NULL), "nc")
  expect_error(simlrU(proj, "stochastic", NULL), "nc")
})

test_that("initializeSimlr supports joint and per-modality algorithms", {
  d <- .cov_simlr_data(n = 20, p = c(6, 7))
  ml <- d$ml
  ml$a[1, 1] <- NA
  ml$b[, 2] <- 3
  k <- 2
  for (alg in c("pca", "ica", "random")) {
    u <- .quiet(initializeSimlr(ml, k, jointReduction = TRUE, uAlgorithm = alg,
                                addNoise = 0.01, zeroUpper = TRUE))
    expect_equal(dim(u), c(20, k))
    expect_equal(unname(u[1, 2]), 0)
  }
  u_avg <- initializeSimlr(ml, k, jointReduction = TRUE, uAlgorithm = "avg")
  expect_equal(dim(u_avg), c(20, k))
  for (alg in c("svd", "ica", "randomProjection", "other")) {
    ul <- .quiet(initializeSimlr(ml, k, jointReduction = FALSE, uAlgorithm = alg,
                                 addNoise = 0.01, zeroUpper = TRUE))
    expect_length(ul, 2)
    expect_equal(dim(ul[[2]]), c(20, k))
    expect_equal(unname(ul[[1]][1, 2]), 0)
  }
  # randomProjection forces per-modality output even when joint is requested
  urp <- initializeSimlr(ml, k, jointReduction = TRUE, uAlgorithm = "randomProjection")
  expect_true(is.list(urp))
  expect_equal(norm(urp[[1]], "F"), 1, tolerance = 1e-8)
})

test_that("optimal_simlr_initializer picks the lowest-energy start", {
  d <- .cov_simlr_data(n = 20, p = c(6, 7))
  ml <- lapply(d$ml, scale)
  dom <- lapply(ml, function(m) matrix(runif(2 * ncol(m)), 2, ncol(m)))
  res <- .quiet(optimal_simlr_initializer(ml, n_init = 3, basisK = 2, energyType = "acc",
                                          domainMatrices = dom, domainLambdas = c(1, 0),
                                          seed = 4, verbose = TRUE))
  expect_named(res, c("bestU", "bestV", "bestEnergy"))
  expect_equal(dim(res$bestU), c(20, 2))
  expect_equal(norm(res$bestU, "F"), 1, tolerance = 1e-8)
  expect_length(res$bestV, 2)
  expect_true(is.finite(res$bestEnergy))
  res2 <- optimal_simlr_initializer(ml, n_init = 1, basisK = 2, verbose = FALSE)
  expect_true(is.finite(res2$bestEnergy))
  expect_error(optimal_simlr_initializer(ml, n_init = 1, basisK = 2, seed = -1),
               "non-negative integer")
})

test_that("plot_energy_decomposition builds ggplot objects from a simlr fit", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("tidyr")
  d <- .cov_simlr_data(n = 20, p = c(6, 7))
  res <- .quiet(simlr(d$ml, initialUMatrix = 2L, iterations = 4, energyType = "acc",
                      mixAlg = "svd", constraint = "none", optimizationStyle = "adam"))
  p1 <- suppressWarnings(plot_energy_decomposition(res, modality = "a"))
  p2 <- suppressWarnings(plot_energy_decomposition(res, modality = "b", show_weights = TRUE))
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
  expect_true(all(p1$data$value_rescaled >= 0 & p1$data$value_rescaled <= 1, na.rm = TRUE))
  expect_length(p2$layers, 2)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_no_error(suppressWarnings(print(p2)))
})

test_that("unregistered optimizer steps (lbfgs, random_search) update parameters", {
  set.seed(3)
  target <- matrix(2, 6, 2)
  V0 <- matrix(rnorm(12, mean = 5), 6, 2)
  energy <- function(V, return_raw = FALSE) sum((V - target)^2)
  grad <- function(V) 2 * (V - target)

  opt <- create_optimizer("lbfgs", vmats = list(V0), learning_rate = 0.1)
  s1 <- ANTsR:::step.lbfgs(opt, 1, V0, grad(V0))
  expect_equal(dim(s1$updated_V), dim(V0))
  expect_lt(energy(s1$updated_V), energy(V0))
  s2 <- ANTsR:::step.lbfgs(s1$optimizer, 1, s1$updated_V, grad(s1$updated_V))
  expect_lt(energy(s2$updated_V), energy(V0))
  expect_true(is.matrix(s2$optimizer$state[[1]]$H))
  # vector parameters are also supported
  sv <- ANTsR:::step.lbfgs(create_optimizer("lbfgs", vmats = list(1:3)), 1, c(1, 2, 3), c(1, 1, 1))
  expect_length(sv$updated_V, 3)

  ropt <- create_optimizer("random_search", vmats = list(V0), n_trials = 20L,
                           noise_scale = 5, learning_rate = 0.5)
  r1 <- ANTsR:::step.random_search(ropt, 1, V0, grad(V0), full_energy_function = energy)
  expect_lte(energy(r1$updated_V), energy(V0))
  r2 <- ANTsR:::step.random_search(ropt, 1, V0, grad(V0), full_energy_function = NULL)
  expect_false(isTRUE(all.equal(r2$updated_V, V0)))
})
