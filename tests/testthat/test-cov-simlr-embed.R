# Coverage tests for NNHEmbed(), NNHEmbed_update_residuals(),
# extend_simlr_embedding_with_new_modalities(), apply_simlr_matrices*()
# and transform_prior_rows_to_uncorrelated().

.quiet_e <- function(expr) {
  out <- NULL
  invisible(utils::capture.output(
    out <- suppressWarnings(suppressMessages(expr))
  ))
  out
}

# A small synthetic ANTsPyMM-style data frame with T1, DTI, rsfMRI, asymmetry
# and cognitive columns all driven by a shared 2-d latent signal.
.make_pymm_blaster <- function(n = 30, seed = 2) {
  set.seed(seed)
  lat <- matrix(rnorm(n * 2), n, 2)
  mkcols <- function(prefix, regions) {
    m <- lat %*% matrix(rnorm(2 * length(regions)), 2) +
      matrix(rnorm(n * length(regions), sd = 0.3), n)
    colnames(m) <- paste0(prefix, regions)
    m
  }
  t1 <- mkcols("T1Hier_vol_", paste0("reg", 1:6))
  t1a <- mkcols("T1Hier_vol_Asym", paste0("reg", 1:3))
  dt <- mkcols("DTI_mean_fa_", paste0("tract", 1:5))
  rsf <- mkcols("rsfMRI_fcnxpro122_", c("DefaultA_2_DefaultB", "DefaultB_2_DefaultC",
                                         "Vis_2_Mot", "Mot_2_Sal"))
  cog <- mkcols("cog", 1:3)
  nuis <- matrix(rnorm(n), n, 1, dimnames = list(NULL, "T1Hier_vol_snr_x"))
  blaster <- data.frame(T1Hier_resnetGrade = runif(n, 1.5, 2), t1, t1a, dt, rsf,
                        nuis, cog, check.names = FALSE)
  blaster$age <- rnorm(n)
  list(blaster = blaster, lat = lat)
}

test_that("NNHEmbed returns feature lists and matrices when requested", {
  b <- .make_pymm_blaster()
  bl <- b$blaster
  n <- nrow(bl)
  idl <- .quiet_e(NNHEmbed(bl, rep(TRUE, n), energy = "cca", nsimlr = 2,
                           constraint = "none", returnidps = 1, verbose = TRUE))
  expect_true(all(c("t1", "dt", "rsf") %in% names(idl)))
  expect_false("T1Hier_vol_snr_x" %in% unlist(idl))
  expect_true(all(grepl("^DTI_mean_fa", idl$dt)))

  # doAsym = 2 splits asymmetry features into their own modality
  idl2 <- .quiet_e(NNHEmbed(bl, rep(TRUE, n), energy = "cca", nsimlr = 2,
                            constraint = "none", returnidps = 1, doAsym = 2,
                            connect_cog = c("cog1", "cog2", "cog3")))
  expect_true("t1a" %in% names(idl2))
  expect_true(all(grepl("Asym", idl2$t1a)))
  expect_false(any(grepl("Asym", idl2$t1)))
  expect_equal(idl2$cg, c("cog1", "cog2", "cog3"))

  # doAsym = 0 drops asymmetry; inclusions/exclusions and restrictDFN filter features
  idl3 <- .quiet_e(NNHEmbed(bl, rep(TRUE, n), energy = "cca", nsimlr = 2,
                            constraint = "none", returnidps = 1, doAsym = 0,
                            exclusions = c("tract5"), inclusions = c("_"),
                            restrictDFN = TRUE))
  expect_false(any(grepl("Asym", unlist(idl3))))
  expect_false("DTI_mean_fa_tract5" %in% idl3$dt)
  expect_true(all(grepl("Default", idl3$rsf)))

  mr <- .quiet_e(NNHEmbed(bl, rep(TRUE, n), energy = "cca", nsimlr = 2,
                          constraint = "none", returnidps = 2,
                          covariates = c("mean", "centerAndScale"), doperm = TRUE))
  expect_named(mr, c("matrices", "regs"))
  expect_true(all(diag(mr$regs$t1) == 1))
  expect_equal(nrow(mr$matrices$t1), n)
  expect_equal(unname(colMeans(mr$matrices$dt)), rep(0, ncol(mr$matrices$dt)),
               tolerance = 1e-8)
})

test_that("NNHEmbed baseline energies return loading matrices", {
  b <- .make_pymm_blaster()
  bl <- b$blaster
  n <- nrow(bl)
  for (en in c("base.pca", "base.spca", "base.rand.0", "base.rand.1", "base.mcca")) {
    r <- .quiet_e(NNHEmbed(bl, rep(TRUE, n), energy = en, nsimlr = 2, constraint = "none"))
    expect_identical(r$simlrX$energy, en)
    expect_true(all(c("t1", "dt", "rsf") %in% names(r$simlrX$v)))
    expect_equal(ncol(r$simlrX$v$t1), 2)
    expect_equal(nrow(r$simlrX$v$dt), 5)
  }
  # nsimlr larger than the smallest modality is shrunk for baselines
  r <- .quiet_e(NNHEmbed(bl, rep(TRUE, n), energy = "base.pca", nsimlr = 5,
                         constraint = "none"))
  expect_equal(ncol(r$simlrX$v$rsf), 4)
  expect_error(.quiet_e(NNHEmbed(bl, rep(TRUE, n), energy = "base.unknown", nsimlr = 2,
                                 constraint = "none")),
               "Unknown baseline")
})

test_that("NNHEmbed runs a full SiMLR fit and projects all subjects", {
  b <- .make_pymm_blaster()
  bl <- b$blaster
  n <- nrow(bl)
  r <- .quiet_e(NNHEmbed(bl, rep(TRUE, n), energy = "cca", nsimlr = 2,
                         constraint = "none", iterations = 3, optimizationStyle = "adam",
                         sparseness = c(0.5, 0.5, 0.5), verbose = TRUE))
  expect_named(r, c("demog", "mats", "simnames", "simlrX", "energy"))
  expect_equal(unname(r$energy), "acc")
  expect_equal(r$simnames, c("t1PC1", "t1PC2", "dtPC1", "dtPC2", "rsfPC1", "rsfPC2"))
  expect_true(all(r$simnames %in% names(r$demog)))
  expect_equal(nrow(r$demog), n)
  expect_true(all(grepl("^T1Hier_vol_", rownames(r$simlrX$v$t1))))
  # the joint embedding recovers the simulated latent space
  cc <- cancor(as.matrix(r$demog[, c("t1PC1", "t1PC2")]), b$lat)$cor
  expect_gt(cc[1], 0.7)
})

test_that("NNHEmbed handles path modeling, relative and estimated rank", {
  b <- .make_pymm_blaster()
  bl <- b$blaster
  n <- nrow(bl)
  r <- .quiet_e(NNHEmbed(bl, rep(TRUE, n), energy = "reg", nsimlr = 0.1,
                         constraint = "none", iterations = 2, optimizationStyle = "adam",
                         sparseness = 0.6, mixAlg = "svd", verbose = TRUE))
  # 0.1 * total features (24) rounds to k = 2
  expect_equal(ncol(r$simlrX$v$t1), 2)
  expect_identical(r$simlrX$mixAlg, "svd")

  pm <- list(2:3, c(1, 3), 1:2)
  r2 <- .quiet_e(NNHEmbed(bl, rep(TRUE, n), energy = "lrr",
                          constraint = "none", iterations = 2, optimizationStyle = "adam",
                          path_modeling = pm, verbose = TRUE))
  expect_equal(r2$simlrX$connectors, pm)
  expect_gte(ncol(r2$simlrX$v$t1), 1)
  expect_message(
    NNHEmbed(bl, rep(TRUE, n), energy = "made_up", nsimlr = 2,
             constraint = "none", returnidps = 1),
    "energy should be one of"
  )
  # connect_cog builds a sparse Matrix regularizer, which NNHEmbed then fails
  # to NA-clean with `[<-` (package bug)
  expect_error(
    .quiet_e(NNHEmbed(bl, rep(TRUE, n), energy = "cca", nsimlr = 2, constraint = "none",
                      returnidps = 2, connect_cog = c("cog1", "cog2", "cog3")))
  )
})

test_that("NNHEmbed_update_residuals implements every covariate option", {
  set.seed(9)
  n <- 20
  m1 <- matrix(rnorm(n * 6), n, 6, dimnames = list(NULL, paste0("v", 1:6)))
  mats <- list(m1 = m1)
  bl2 <- data.frame(age = rnorm(n), sex = rnorm(n))
  ok <- rep(TRUE, n)
  out <- utils::capture.output(res <- NNHEmbed_update_residuals(opt = "opt"))
  expect_null(res)
  expect_true(any(grepl("whiten", out)))
  expect_identical(NNHEmbed_update_residuals(mats, 1, NULL, bl2, ok, 2), m1)
  nr <- NNHEmbed_update_residuals(mats, 1, "norm", bl2, ok, 2)
  expect_equal(norm(nr, "F"), 1)
  wh <- NNHEmbed_update_residuals(mats, 1, "whiten", bl2, ok, 2)
  expect_equal(unname(cov(wh) * (n - 1)), diag(6), tolerance = 1e-4)
  lr <- NNHEmbed_update_residuals(mats, 1, "lowrank", bl2, ok, 2)
  expect_equal(dim(lr), c(4, 6))
  ev <- NNHEmbed_update_residuals(mats, 1, "eigenvalue", bl2, ok, 2)
  expect_equal(sum(svd(ev)$d), 1, tolerance = 1e-8)
  rb <- NNHEmbed_update_residuals(mats, 1, "robust", bl2, ok, 2)
  expect_equal(dim(rb), dim(m1))
  ce <- NNHEmbed_update_residuals(mats, 1, "center", bl2, ok, 2)
  expect_equal(unname(colMeans(ce)), rep(0, 6), tolerance = 1e-10)
  rk <- NNHEmbed_update_residuals(mats, 1, "rank", bl2, ok, 2)
  expect_equal(range(rk), c(-1, 1))
  sc <- NNHEmbed_update_residuals(mats, 1, "scale", bl2, ok, 2)
  expect_equal(dim(sc), dim(m1))
  cs <- NNHEmbed_update_residuals(mats, 1, "centerAndScale", bl2, ok, 2)
  expect_equal(unname(apply(cs, 2, sd)), rep(1, 6), tolerance = 1e-10)
  np <- NNHEmbed_update_residuals(mats, 1, "np", bl2, ok, 2)
  expect_equal(np, m1 / (n * 6))
  mn <- NNHEmbed_update_residuals(mats, 1, "mean", bl2, ok, 2)
  expect_equal(dim(mn), dim(m1))
  fr <- NNHEmbed_update_residuals(mats, 1, "age + sex", bl2, ok, 2)
  # residuals are orthogonal to the covariates
  expect_equal(unname(crossprod(as.matrix(bl2), fr)), matrix(0, 2, 6), tolerance = 1e-8)
})

.make_extend_inputs <- function(n = 30, seed = 3) {
  set.seed(seed)
  lat <- matrix(rnorm(n * 2), n, 2)
  mk <- function(prefix, p) {
    m <- lat %*% matrix(rnorm(2 * p), 2) + matrix(rnorm(n * p, sd = 0.3), n)
    colnames(m) <- paste0(prefix, 1:p)
    m
  }
  t1 <- mk("t1feat", 6); dt <- mk("dtfeat", 5); pet <- mk("petfeat", 4); mri <- mk("mrifeat", 3)
  pymm <- data.frame(t1, dt, pet, mri)
  pymm$petconst <- 1
  v <- list(
    t1 = matrix(abs(rnorm(12)), 6, 2, dimnames = list(colnames(t1), c("PC1", "PC2"))),
    dt = matrix(abs(rnorm(10)), 5, 2, dimnames = list(colnames(dt), c("PC1", "PC2")))
  )
  list(pymm = pymm, sr = list(v = v), pet = colnames(pet), mri = colnames(mri))
}

.ext_args <- list(iterations = 2, nperms = 0, optimizationStyle = "adam",
                  energyType = "acc", mixAlg = "pca", constraint = "none",
                  scale = "center", sparsenessAlg = "soft")

test_that("extend_simlr_embedding_with_new_modalities builds blocks in all modes", {
  e <- .make_extend_inputs()
  r <- .quiet_e(do.call(extend_simlr_embedding_with_new_modalities,
                        c(list(e$pymm, e$sr, list(pet = c(e$pet, "petconst")),
                               mode = "concatenate", cor_threshold = 0.5, verbose = TRUE),
                          .ext_args)))
  expect_named(r, c("updated_simlr_result", "simlr_permutations", "blocks", "adjacency",
                    "projected_data", "k_new_used", "joint_k", "diagnostics"))
  expect_equal(names(r$blocks), c("sim", "pet"))
  expect_equal(ncol(r$blocks$sim), 4)
  # the constant column is dropped by default
  expect_equal(r$diagnostics$preprocessing$pet$dropped_columns, "petconst")
  expect_equal(ncol(r$blocks$pet), 4)
  expect_true(all(diag(r$adjacency$pet) == 1))
  expect_true(all(r$adjacency$pet == 0 | abs(r$adjacency$pet) >= 0.5))
  expect_true("pet" %in% names(r$updated_simlr_result$v))
  expect_false("pet" %in% names(e$sr$v))  # input not mutated
  expect_equal(r$joint_k, max(2L, r$k_new_used[["pet"]]))

  r2 <- .quiet_e(do.call(extend_simlr_embedding_with_new_modalities,
                         c(list(e$pymm, e$sr, list(pet = c(e$pet, "petconst"), mri = e$mri),
                                mode = "split", split_prefixes = c("t1", "dt", "zz"),
                                preprocess = "zero", k_new = c(pet = 3, mri = 2),
                                joint_k_policy = "median", use_abs_correlation = FALSE,
                                verbose = TRUE),
                           .ext_args)))
  expect_equal(names(r2$blocks), c("t1", "dt", "pet", "mri"))
  expect_equal(r2$diagnostics$preprocessing$pet$zeroed_columns, "petconst")
  expect_equal(ncol(r2$blocks$pet), 5)
  expect_equal(r2$k_new_used, c(pet = 3L, mri = 2L))
  expect_equal(r2$joint_k, 2L)

  r3 <- .quiet_e(do.call(extend_simlr_embedding_with_new_modalities,
                         c(list(e$pymm, e$sr, list(mri = e$mri), mode = "auto",
                                k_method = "elbow", joint_k_policy = "min", k_new = NULL,
                                min_k = 1L, verbose = TRUE),
                           .ext_args)))
  expect_equal(r3$diagnostics$split_prefixes_used, c("t1", "dt"))
  expect_equal(r3$diagnostics$mode_used, "auto")
  expect_true(r3$k_new_used[["mri"]] >= 1L)

  r4 <- .quiet_e(do.call(extend_simlr_embedding_with_new_modalities,
                         c(list(e$pymm, e$sr, list(mri = e$mri), k_new = 2),
                           .ext_args)))
  expect_equal(r4$k_new_used, c(mri = 2L))
})

test_that("extend_simlr_embedding_with_new_modalities validates its inputs", {
  e <- .make_extend_inputs()
  f <- function(...) extend_simlr_embedding_with_new_modalities(e$pymm, e$sr, ...)
  nm <- list(pet = e$pet)
  expect_error(f(nm, cor_threshold = 2), "cor_threshold")
  expect_error(f(nm, cor_threshold = c(0.1, 0.2)), "finite numeric scalar")
  expect_error(f(nm, cumvar_threshold = 0), "cumvar_threshold")
  expect_error(f(nm, min_k = 1.5), "positive integer scalar")
  expect_error(f(list(e$pet)), "named list")
  expect_error(f(list(pet = character(0))), "non-empty character")
  expect_error(f(list(pet = c("petfeat1", "petfeat1"))), "duplicated")
  expect_error(f(nm, k_new = c(foo = 2, pet = 2)), "not present")
  # a single *named* k_new with a wrong name passes validation but then fails
  # with a subscript error (package bug)
  expect_error(f(nm, k_new = c(foo = 2)))
  expect_error(f(nm, k_new = c(pet = 1.5)), "positive integers")
  expect_error(f(nm, k_new = "a"), "must be NULL")
  expect_error(f(nm, split_prefixes = character(0)), "split_prefixes")
  expect_error(extend_simlr_embedding_with_new_modalities(e$pymm, list(a = 1), nm),
               "\\$v")
  expect_error(f(list(pet = "t1PC1")), "overlap")
  expect_error(f(list(pet = "nothere")), "not found")
  expect_error(f(nm, mode = "split", split_prefixes = "zz"), "No existing SIMLR blocks")
  sr_noprefix <- list(v = list(abc = e$sr$v$t1))
  colnames(sr_noprefix$v$abc) <- c("X1", "X2")
  expect_error(extend_simlr_embedding_with_new_modalities(e$pymm, sr_noprefix, nm,
                                                          mode = "auto"),
               "auto-detect")
  pm <- e$pymm
  pm$petna <- NA_real_
  expect_error(extend_simlr_embedding_with_new_modalities(pm, e$sr, list(pet = "petna")),
               "no usable columns")
})

test_that("apply_simlr_matrices projects data and versions clashing names", {
  set.seed(4)
  df <- data.frame(a = rnorm(10), b = rnorm(10), c = rnorm(10))
  W <- matrix(1:6, 3, 2, dimnames = list(c("a", "b", "c"), NULL))
  r <- apply_simlr_matrices(df, list(blk = W))
  expect_equal(r$new_colnames, c("blkC1", "blkC2"))
  expect_equal(unname(as.matrix(r$extended_df[, "blkC1"])), unname(as.matrix(df) %*% W[, 1]))
  # applying again forces versioned names
  r2 <- expect_message(apply_simlr_matrices(r$extended_df, list(blk = W), n_limit = 1,
                                            verbose = TRUE), "blkC1")
  expect_equal(r2$new_colnames, "blkCr00.1")
  # many clashes roll the minor version into the major one
  df3 <- r$extended_df
  for (i in 1:10) df3 <- apply_simlr_matrices(df3, list(blk = W[, 1, drop = FALSE]))$extended_df
  expect_true(any(grepl("r01\\.", names(df3))))
  # names without a trailing number get the tag appended
  Wn <- matrix(1, 3, 1, dimnames = list(c("a", "b", "c"), "X"))
  df4 <- apply_simlr_matrices(df, list(q = Wn))$extended_df
  expect_true("qXr00." %in% apply_simlr_matrices(df4, list(q = Wn))$new_colnames)
  expect_error(apply_simlr_matrices(as.matrix(df), list(blk = W)), "data frame")
  expect_error(apply_simlr_matrices(df, list(W)), "named list")
  expect_error(apply_simlr_matrices(df, list(blk = unname(W))), "no rownames")
  Wz <- matrix(1, 2, 1, dimnames = list(c("x", "y"), "PC1"))
  w <- testthat::capture_warnings(r0 <- apply_simlr_matrices(df, list(z = Wz)))
  expect_true(any(grepl("No overlapping", w)))
  expect_true(any(grepl("No projections", w)))
  expect_identical(r0$extended_df, df)
  r5 <- suppressWarnings(apply_simlr_matrices(df, list(notmat = 1:3, blk = W)))
  expect_equal(r5$new_colnames, c("blkC1", "blkC2"))
})

test_that("apply_simlr_matrices_dtfix maps shortened DTI names", {
  set.seed(5)
  longnm <- c("DTI_mean_fa_body_of_corpus_callosum_jhu_icbm_labels_1mm",
              "DTI_mean_fa_genu_of_corpus_callosum_jhu_icbm_labels_1mm")
  df <- as.data.frame(matrix(rnorm(20), 10, 2, dimnames = list(NULL, longnm)))
  df$other <- rnorm(10)
  W <- matrix(c(1, 2), 2, 1, dimnames = list(longnm, "PC1"))
  r <- .quiet_e(apply_simlr_matrices_dtfix(df, list(dt = W, dta = W)))
  expect_true("dtPC1" %in% r$new_colnames)
  # original column names are restored
  expect_true(all(longnm %in% names(r$extended_df)))
  r0 <- apply_simlr_matrices_dtfix(df[, "other", drop = FALSE],
                                   list(o = matrix(1, 1, 1, dimnames = list("other", "PC1"))))
  expect_equal(r0$new_colnames, "oPC1")
})

test_that("transform_prior_rows_to_uncorrelated decorrelates prior rows", {
  set.seed(6)
  base <- matrix(rnorm(3 * 12), 3, 12)
  P <- rbind(base, base + matrix(rnorm(3 * 12, sd = 0.05), 3, 12),
             matrix(rnorm(2 * 12), 2, 12))
  r <- .quiet_e(transform_prior_rows_to_uncorrelated(P, verbose = TRUE))
  expect_named(r, c("P_orig", "P_processed", "P_trans", "U", "S", "Vt", "rotation",
                    "transform_fn", "diagnostics"))
  j <- r$diagnostics$chosen_j
  expect_equal(dim(r$P_trans), c(j, 12))
  expect_gte(sum(r$diagnostics$var_explained[1:j]), 0.9)
  G <- tcrossprod(r$P_trans)
  expect_equal(G[upper.tri(G)], rep(0, sum(upper.tri(G))), tolerance = 1e-8)
  expect_equal(dim(r$transform_fn(P[1:2, ])), c(2, j))
  expect_error(r$transform_fn(P[, 1:5]), "same number of columns")

  r2 <- transform_prior_rows_to_uncorrelated(P, j = 2, row_center = FALSE,
                                              row_scale = TRUE, varimax = TRUE,
                                              verbose = FALSE)
  expect_equal(r2$diagnostics$chosen_j, 2)
  expect_equal(dim(r2$rotation), c(2, 2))
  expect_equal(crossprod(r2$rotation), diag(2), tolerance = 1e-8)

  r3 <- .quiet_e(transform_prior_rows_to_uncorrelated(P, method = "cluster_then_svd",
                                                       row_scale = TRUE, verbose = TRUE))
  expect_equal(r3$diagnostics$method, "cluster_then_svd")
  expect_length(r3$diagnostics$cluster_info$clusters, nrow(P))
  r4 <- transform_prior_rows_to_uncorrelated(P, method = "cluster_then_svd", cluster_k = 3,
                                              j = 2, verbose = FALSE)
  expect_equal(nrow(r4$diagnostics$cluster_info$centroids), 3)
  expect_equal(dim(r4$transform_fn(P)), c(nrow(P), 2))
})
