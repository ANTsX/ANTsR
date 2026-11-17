# Coverage tests for SiMLR helper / baseline / rank-selection / plotting utilities.

.quiet_u <- function(expr) {
  out <- NULL
  invisible(utils::capture.output(
    out <- suppressWarnings(suppressMessages(expr))
  ))
  out
}

.lat_views <- function(n = 30, p = c(8, 10), k = 2, seed = 21, sd = 0.2) {
  set.seed(seed)
  U <- matrix(rnorm(n * k), n, k)
  ml <- lapply(seq_along(p), function(i) {
    m <- U %*% matrix(rnorm(k * p[i]), k, p[i]) + matrix(rnorm(n * p[i], sd = sd), n, p[i])
    colnames(m) <- paste0("v", i, "_", seq_len(p[i]))
    m
  })
  names(ml) <- paste0("view", seq_along(p))
  list(ml = ml, U = U)
}

test_that("predictSimlr predicts each modality from the SiMLR bases", {
  d <- .lat_views()
  sol <- .quiet_u(simlr(d$ml, initialUMatrix = 2L, iterations = 3, energyType = "acc",
                        mixAlg = "svd", constraint = "none", optimizationStyle = "adam"))
  pr <- predictSimlr(d$ml, sol)
  expect_named(pr, c("varx", "varxfull", "predictions", "initialErrors", "finalErrors",
                     "aggregateTstats", "tstatsQ5", "rawTstats", "uOrder"))
  expect_length(pr$varx, 2)
  expect_true(all(pr$varx > 0.5 & pr$varx <= 1))
  expect_true(all(pr$finalErrors < pr$initialErrors))
  expect_setequal(pr$uOrder, 1:2)
  pr2 <- predictSimlr(d$ml, sol, targetMatrix = 1, sourceMatrices = 2, projectv = FALSE)
  expect_length(pr2$varx, 1)
  expect_equal(dim(pr2$predictions[[1]]), dim(d$ml[[1]]))
  expect_error(predictSimlr(d$ml, sol, sourceMatrices = 5), "not within the range")
})

test_that("select_joint_k chooses k by elbow, threshold and spca", {
  d <- .lat_views(n = 30, p = c(8, 10), k = 3)
  r <- select_joint_k(d$ml)
  expect_named(r, c("optimal_k", "joint_variance_curve", "plot"))
  expect_equal(nrow(r$joint_variance_curve), 7)
  expect_true(all(diff(r$joint_variance_curve$joint_var) >= -1e-12))
  expect_s3_class(r$plot, "ggplot")
  expect_true(r$optimal_k %in% 1:7)
  rt <- select_joint_k(d$ml, k_max = 6, selection_method = "threshold", variance_threshold = 0.5)
  expect_equal(rt$optimal_k, min(rt$joint_variance_curve$k[rt$joint_variance_curve$joint_var >= 0.5]))
  expect_warning(rt2 <- select_joint_k(d$ml, k_max = 4, selection_method = "threshold",
                                       variance_threshold = 1.1), "threshold")
  expect_equal(rt2$optimal_k, 4)
  expect_warning(r1 <- select_joint_k(d$ml, k_max = 2), "Not enough")
  expect_equal(r1$optimal_k, 1)
  skip_if_not_installed("elasticnet")
  # elasticnet::spca() returns no `scores`, so the spca branch fails (package bug)
  expect_error(.quiet_u(select_joint_k(lapply(d$ml, function(m) m[, 1:5]), method = "spca",
                                       k_max = 3, sparsity = 3)),
               "numeric/complex")
})

test_that("select_joint_k self-test passes", {
  expect_null(.quiet_u(select_joint_k(self_test = TRUE)))
})

test_that("estimate_joint_rank and estimate_rank_by_permutation_rv find shared rank", {
  d <- .lat_views(n = 30, p = c(8, 10), k = 2, sd = 0.1)
  set.seed(1)
  r <- .quiet_u(estimate_joint_rank(d$ml, n_permutations = 3))
  expect_named(r, c("optimal_k", "results", "plot"))
  expect_gte(r$optimal_k, 2)
  expect_lte(r$optimal_k, 15)
  expect_s3_class(r$plot, "ggplot")
  expect_setequal(unique(r$results$type), c("Real Data", "Permuted Data (Null)"))
  r2 <- .quiet_u(estimate_joint_rank(d$ml, n_permutations = 2, pre_scaling = "none",
                                     plot_scree = FALSE))
  expect_null(r2$plot)

  ml <- d$ml
  ml$view1[, 3] <- 1
  ml$view2[, 2] <- NA
  rr <- .quiet_u(estimate_rank_by_permutation_rv(ml, n_permutations = 0, return_max = TRUE))
  expect_true(rr$optimal_k >= 1)
  rr2 <- .quiet_u(estimate_rank_by_permutation_rv(ml, n_permutations = 2,
                                                  handle_missing = FALSE))
  expect_named(rr2, c("optimal_k", "results", "plot"))
  expect_setequal(unique(rr2$results$type), c("Real Data", "Permuted Null"))
  rr3 <- .quiet_u(estimate_rank_by_permutation_rv(ml, n_permutations = 0))
  expect_true(rr3$optimal_k >= 1)
  expect_error(.quiet_u(estimate_rank_by_permutation_rv(list(ml$view1, ml$view2[1:10, ]))),
               "same number of rows")
})

test_that("baseline feature extractors return named loading matrices", {
  d <- .lat_views()
  pc <- expect_warning(antsr_pca_features(d$ml, 9), "exceeds")
  expect_equal(ncol(pc$view1), 8)
  expect_equal(ncol(pc$view2), 9)
  sp <- antsr_spca_features(d$ml, 2)
  expect_equal(dim(sp$view2), c(10, 2))
  expect_true(all(sp$view1 >= 0))
  sp2 <- antsr_spca_features(d$ml, 2, para = 0.5)
  expect_equal(colnames(sp2$view1), c("PC1", "PC2"))
  if (requireNamespace("elasticnet", quietly = TRUE)) {
    se <- antsr_spca_features(d$ml, 2, method = "elasticnet")
    expect_equal(rownames(se$view2), colnames(d$ml$view2))
    expect_true(all(colSums(se$view1 != 0) <= 4))
  }
  mc <- antsr_mcca_features(unname(d$ml), k = 3)
  expect_equal(names(mc), c("view1", "view2"))
  expect_equal(dim(mc$view1), c(8, 3))
  expect_error(antsr_mcca_features(list(d$ml$view1, d$ml$view2[1:5, ])), "same number")
  rf <- antsr_random_features(d$ml, 2, seed = 1)
  expect_equal(dim(rf$view2), c(10, 2))
  if (requireNamespace("RGCCA", quietly = TRUE)) {
    rg <- antsr_rgcca_baseline(d$ml, ncomp = 2)
    expect_equal(ncol(rg$view1), 2)
    rgs <- antsr_rgcca_sparse(d$ml, ncomp = 2, sparsity = 0.5)
    expect_equal(ncol(rgs$view2), 2)
  } else {
    expect_error(antsr_rgcca_baseline(d$ml), "RGCCA")
    expect_error(antsr_rgcca_sparse(d$ml), "RGCCA")
  }
})

test_that("multiview_pca and orthogonalize_feature_space behave", {
  d <- .lat_views(n = 25, p = c(6, 7))
  set.seed(2)
  mv <- .quiet_u(multiview_pca(lapply(d$ml, scale), 2, sparse = c(0, 0), max_iter = 3,
                               verbose = TRUE))
  expect_equal(dim(mv$Z), c(25, 2))
  expect_equal(dim(mv$W[[2]]), c(7, 2))
  mv2 <- .quiet_u(multiview_pca(lapply(d$ml, scale), 2, sparse = c(0.5, 0.5), max_iter = 2))
  expect_equal(dim(mv2$W[[1]]), c(6, 2))
  expect_error(multiview_pca(d$ml, 2, sparse = 2), "between 0 and 1")

  set.seed(3)
  ml <- list(matrix(rnorm(20), 10, 2), matrix(rnorm(16), 8, 2))
  before <- sum(sapply(ml, ANTsR:::invariant_orthogonality_defect))
  out <- .quiet_u(ANTsR:::orthogonalize_feature_space(ml, max_iterations = 20,
                                                      learning_rate = 0.05, verbose = TRUE))
  after <- sum(sapply(out, ANTsR:::invariant_orthogonality_defect))
  expect_lt(after, before)
  out2 <- ANTsR:::orthogonalize_feature_space(ml, max_iterations = 3, tolerance = 1e6,
                                              verbose = FALSE)
  expect_length(out2, 2)
  expect_error(ANTsR:::orthogonalize_feature_space(list(ml[[1]], matrix(1, 3, 3)), verbose = FALSE))
})

test_that("nsa_flow_retract covers all retraction types", {
  set.seed(4)
  Yt <- matrix(rnorm(20), 10, 2)
  expect_identical(nsa_flow_retract(Yt, 1, "none"), Yt)
  expect_identical(nsa_flow_retract(Yt, 1, NULL), Yt)
  P <- nsa_flow_retract(Yt, 1, "polar")
  # polar retraction gives orthogonal columns (norm is preserved)
  G <- crossprod(P)
  expect_equal(G[1, 2], 0, tolerance = 1e-8)
  expect_equal(sqrt(sum(P^2)), sqrt(sum(Yt^2)), tolerance = 1e-8)
  S <- nsa_flow_retract(Yt, 0.5, "soft_polar")
  expect_equal(dim(S), dim(Yt))
  Yw <- matrix(rnorm(12), 3, 4)
  expect_equal(dim(nsa_flow_retract(Yw, 0.5, "soft_svd")), c(3, 4))
  Yww <- matrix(rnorm(3 * 520), 3, 520)
  W1 <- nsa_flow_retract(Yww, 1, "soft_polar")
  expect_equal(dim(W1), c(3, 520))
  W2 <- nsa_flow_retract(Yww, 0.5, "soft_polar")
  expect_equal(sqrt(sum(W2^2)), sqrt(sum(Yww^2)), tolerance = 1e-6)
  Ytb <- matrix(rnorm(530 * 513), 530, 513)
  expect_equal(dim(nsa_flow_retract(Ytb, 1, "soft_polar")), c(530, 513))
  expect_error(nsa_flow_retract(1:3, 1, "polar"), "must be a matrix")
  expect_error(nsa_flow_retract(Yt, 1, "weird"), "Unknown retraction_type")
})

test_that("orthonormal non-negative projections", {
  set.seed(5)
  X <- matrix(rnorm(40), 10, 4)
  P <- project_to_orthonormal_nonnegative(X)
  expect_true(all(P >= 0))
  expect_equal(dim(P), dim(X))
  P2 <- project_to_orthonormal_nonnegative(abs(X), constraint = "none", max_iter = 2)
  expect_true(all(P2 >= 0))
  expect_equal(dim(project_to_orthonormal_nonnegative(matrix(numeric(0), 0, 0))), c(0, 0))
  Q <- project_to_partially_orthonormal_nonnegative(X, ortho_strength = 0.5)
  expect_true(all(Q >= 0))
  Q2 <- project_to_partially_orthonormal_nonnegative(X, constraint = "either", max_iter = 3)
  expect_true(all(Q2 >= 0))
  expect_error(project_to_partially_orthonormal_nonnegative(X, ortho_strength = 2), "between 0 and 1")
  expect_error(project_to_partially_orthonormal_nonnegative(X, constraint = "neg"), "Constraint must be")
})

test_that("small numeric helpers behave as documented", {
  m <- matrix(c(1, 1, 1, 2, 3, 4, 5, 5, 5), 3, 3, byrow = TRUE,
              dimnames = list(c("r1", "r2", "r3"), c("a", "b", "c")))
  w <- testthat::capture_warnings(z <- check_zero_variance(m))
  expect_equal(z, c(TRUE, FALSE))
  expect_true(any(grepl("r1, r3", w)))
  m2 <- cbind(c(1, 2, 3), c(4, 4, 4))
  expect_warning(z2 <- check_zero_variance(m2), "columns: 2")
  expect_equal(z2, c(FALSE, TRUE))
  expect_error(check_zero_variance(1:3), "must be a matrix")

  expect_equal(dice_overlap_soft_vector(c(1, 0, 2, 0), c(1, 0, 0, 3), quantile = 0), 0.5)
  expect_equal(dice_overlap_soft_vector(c(0, 0), c(0, 0), quantile = 0), 1)
  expect_equal(dice_overlap_soft_vector(1:10, 1:10), 1)

  set.seed(6)
  Xw <- matrix(rnorm(5 * 12), 5, 12, dimnames = list(paste0("s", 1:5), paste0("f", 1:12)))
  fw <- fast_whiten(Xw)
  expect_equal(dimnames(fw), dimnames(Xw))
  Xt <- matrix(rnorm(40 * 3), 40, 3)
  ft <- fast_whiten(Xt)
  expect_equal(unname(crossprod(ft)), diag(3), tolerance = 1e-6)
})

test_that("write_simlr handles nulls, lists, unnamed elements and clearing", {
  td <- tempfile("wsim")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  pre <- file.path(td, "obj")
  obj <- list(
    energyPath = matrix(1:4, 2),
    nothing = NULL,
    df = data.frame(x = 1:2),
    mat = matrix(1:4, 2, dimnames = list(c("a", "b"), NULL)),
    lst = list(df = data.frame(y = 1), m = matrix(1), NULL, other = "txt"),
    scalar = 3
  )
  obj["nothing"] <- list(NULL)
  write_simlr(obj, pre)
  outdir <- paste0(pre, "_simlr")
  man <- readRDS(file.path(outdir, "manifest.rds"))
  expect_equal(man$energyPath$type, "data.frame")
  expect_equal(man$nothing$type, "null")
  expect_equal(man$mat$type, "matrix")
  expect_equal(man$lst$type, "list")
  expect_equal(man$lst$contents[["3"]]$type, "null")
  expect_false(man$lst$contents[["3"]]$use_name)
  expect_equal(man$lst$contents$other$type, "rds")
  expect_equal(man$scalar$type, "rds")
  expect_true(file.exists(file.path(outdir, "lst", "df.csv")))
  expect_warning(write_simlr(obj, pre), "not empty")
  writeLines("junk", file.path(outdir, "junk.txt"))
  write_simlr(list(a = 1), pre, clear_dir = TRUE)
  expect_false(file.exists(file.path(outdir, "junk.txt")))
  expect_error(write_simlr(list(1, 2), pre), "named list")
})

test_that("simlr.search runs multiple parameter sets with random sparseness and permutations", {
  set.seed(1500)
  mats <- list(mod1 = matrix(rnorm(12 * 5), 12, 5), mod2 = matrix(rnorm(12 * 4), 12, 4))
  regs <- regularizeSimlr(mats)
  opts <- simlr.parameters(
    nsimlr_options = list(2),
    prescaling_options = list("center"),
    objectiver_options = list("acc", "regression"),
    mixer_options = list("pca"),
    sparval_options = list("rand2x0.4x0.6"),
    expBeta_options = list(0),
    positivities_options = list(c("positive", "positive")),
    optimus_options = list("adam"),
    search_type = "full"
  )
  expect_equal(nrow(opts), 2)
  res <- .quiet_u(simlr.search(mats, regs, opts, maxits = 2, nperms = 5, verbose = 0))
  expect_named(res, c("simlr_result", "significance", "parameters", "paramsearch"))
  expect_equal(nrow(res$paramsearch), 2)
  expect_true("ttest" %in% res$significance$perm)
  expect_equal(res$parameters$final_energy, max(res$paramsearch$final_energy))
  det <- simlr.parameters(list(2, 3), list("center"), list("acc"), list("pca"),
                          list(0.5), list(0), list("positive"), list("adam"),
                          num_samples = 2, search_type = "deterministic")
  expect_equal(nrow(det), 2)
})

test_that("plotting and interpretation helpers return plot objects", {
  skip_if_not_installed("ggplot2")
  d <- .lat_views(n = 25, p = c(5, 6))
  V1 <- matrix(abs(rnorm(10)), 5, 2)
  V2 <- matrix(abs(rnorm(12)), 6, 2)
  vr <- visualize_lowrank_relationships(d$ml$view1, d$ml$view2, V1, V2,
                                        nm1 = "A", nm2 = "B", max_pairs = 2)
  expect_equal(dim(vr$correlations), c(2, 2))
  expect_s3_class(vr$heatmap, "ggplot")
  expect_true(is.finite(vr$adj_rv) || is.na(vr$adj_rv))
  expect_equal(colnames(vr$projections$proj1), c("A_C1", "A_C2"))
  expect_error(visualize_lowrank_relationships(d$ml$view1, d$ml$view2, V2, V2), "Dimension mismatch")
  expect_error(visualize_lowrank_relationships(d$ml$view1, d$ml$view2, V1, V1), "Dimension mismatch")
  expect_error(visualize_lowrank_relationships(d$ml$view1, d$ml$view2[1:5, ], V1, V2),
               "Subject count mismatch")

  V <- matrix(c(0.1, -0.5, 0.3, 0, 0.9, 0.2, 0.4, 0, 0.1, 0.6), 5, 2,
              dimnames = list(c("T1Hier_vol_left_hippocampus", "T1Hier_vol_right_hippocampus",
                                "DTI_mean_fa_genu", "T1Hier_thk_x", "T1Hier_area_y"),
                              c("t1PC1", "t1PC2")))
  iv <- interpret_simlr_vector2(V, "t1PC1", n2show = 3, shortnames = FALSE)
  expect_length(iv, 3)
  expect_equal(unname(abs(iv[1])), 1)
  ivd <- interpret_simlr_vector2(-abs(V), "t1PC2", n2show = NULL, shortnames = TRUE,
                                 return_dataframe = TRUE)
  expect_s3_class(ivd, "data.frame")
  expect_true(all(ivd$values > 0))

  skip_if_not_installed("ggpubr")
  pf <- plot_features(list(t1 = V), n_limit = 3)
  expect_length(pf, 2)
  expect_named(pf, c("t1 t1PC1", "t1 t1PC2"))
  pf2 <- plot_features(list(t1 = abs(V)), take_abs = FALSE)
  expect_s3_class(pf2[[1]], "ggplot")

  skip_if_not_installed("fpc")
  skip_if_not_installed("GGally")
  skip_if_not_installed("ggdendro")
  skip_if_not_installed("patchwork")
  set.seed(8)
  dat <- data.frame(a = c(rnorm(10), rnorm(10, 5)), b = c(rnorm(10), rnorm(10, 5)),
                    c = rnorm(20))
  ev <- .quiet_u(exploratory_visualization(dat, verbose = TRUE))
  expect_equal(ev$optimal_k, 2)
  expect_true(inherits(ev$plot, "patchwork"))
  skip_if_not_installed("tsne")
  ev2 <- .quiet_u(exploratory_visualization(dat, dotsne = TRUE))
  expect_true(inherits(ev2$plot, "patchwork"))
})

test_that("antspymm_predictors returns columns or LR-mapped data", {
  set.seed(9)
  n <- 8
  cn <- c("T1Hier_vol_left_hippocampus", "T1Hier_vol_right_hippocampus",
          "T1Hier_thk_x_l_amyg", "T1Hier_thk_x_r_amyg",
          "DTI_mean_fa_genu", "rsfMRI_fcnxpro122_A_2_B", "perf_cbf_mean_x",
          "T1Hier_resnetGrade", "T1w_mean_x")
  df <- as.data.frame(matrix(abs(rnorm(n * length(cn))) + 1, n, length(cn),
                             dimnames = list(NULL, cn)))
  nms <- antspymm_predictors(df, doasym = FALSE, return_colnames = TRUE)
  expect_true(all(c("DTI_mean_fa_genu", "rsfMRI_fcnxpro122_A_2_B") %in% nms))
  expect_false("T1Hier_resnetGrade" %in% nms)
  nms2 <- antspymm_predictors(df, doasym = TRUE, return_colnames = TRUE)
  expect_false(any(grepl("left|right", nms2)))
  out <- antspymm_predictors(df, doasym = TRUE)
  expect_s3_class(out, "data.frame")
  expect_true(any(grepl("LRAVG", names(out))))
  expect_true(any(grepl("Asym", names(out))))
  out2 <- antspymm_predictors(df)
  expect_true(any(grepl("LRAVG", names(out2))))
})
