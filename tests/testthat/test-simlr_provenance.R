
library(ANTsR)
library(testthat)

context("SiMLR Provenance and Metadata")

test_that("SiMLR captures provenance correctly", {
  set.seed(1500)
  nsub <- 10
  npix <- c(5, 4)
  mats <- list(
    mod1 = matrix(rnorm(nsub * npix[1]), ncol = npix[1]),
    mod2 = matrix(rnorm(nsub * npix[2]), ncol = npix[2])
  )
  regs <- regularizeSimlr(mats)
  
  # Manual SiMLR call
  res <- simlr(mats, regs, iterations = 2, energyType = "acc", scale = c("robust", "np"))
  
  expect_true("provenance" %in% names(res))
  prov <- res$provenance
  expect_s3_class(prov, "data.frame")
  expect_equal(prov$objectiver, "acc")
  expect_equal(prov$mixer, "pca")
  expect_equal(prov$prescaling1, "robust")
  expect_equal(prov$prescaling2, "np")
})

test_that("SiMLR provenance survives write/read roundtrip", {
  set.seed(1500)
  nsub <- 10
  npix <- c(5, 4)
  mats <- list(
    mod1 = matrix(rnorm(nsub * npix[1]), ncol = npix[1]),
    mod2 = matrix(rnorm(nsub * npix[2]), ncol = npix[2])
  )
  regs <- regularizeSimlr(mats)
  res <- simlr(mats, regs, iterations = 2)
  
  file_prefix <- tempfile("simlr_prov_test")
  write_simlr(res, file_prefix)
  res_read <- read_simlr(paste0(file_prefix, "_simlr"))
  
  expect_true("provenance" %in% names(res_read))
  expect_equal(res$provenance, res_read$provenance)
  
  # Cleanup
  unlink(paste0(file_prefix, "_simlr"), recursive = TRUE)
})

test_that("simlr.search produces unified metadata using provenance", {
  set.seed(1500)
  nsub <- 10
  npix <- c(5, 4)
  mats <- list(
    mod1 = matrix(rnorm(nsub * npix[1]), ncol = npix[1]),
    mod2 = matrix(rnorm(nsub * npix[2]), ncol = npix[2])
  )
  regs <- regularizeSimlr(mats)
  
  best_params <- simlr.parameters(
    nsimlr_options = list(2),
    prescaling_options = list(c("robust", "np")),
    objectiver_options = list("acc"),
    mixer_options = list("pca"),
    sparval_options = list(c(0.5)),
    expBeta_options = list(0),
    positivities_options = list(c("positive", "positive")),
    optimus_options = list('adam'),
    search_type = 'full'
  )
  
  sim_res <- simlr.search(
    mats,
    regs,
    best_params,
    nperms = 1,
    maxits = 2,
    verbose = 0
  )
  
  expect_true("paramsearch" %in% names(sim_res))
  ps <- sim_res$paramsearch
  # Ensure it has both provenance columns and search-specific columns
  expect_true("nsimlr" %in% colnames(ps))
  expect_true("prescaling1" %in% colnames(ps))
  expect_true("final_energy" %in% colnames(ps))
  expect_true("perm" %in% colnames(ps))
})
