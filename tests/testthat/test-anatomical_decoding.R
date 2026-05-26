
library(ANTsR)
library(testthat)

context("Anatomical Shorthand Decoding")

test_that("decode_pymm_names translates to human language", {
  # CIT168 match: BN_STR_Pu_Left -> L Putamen (stripped BN and STR)
  expect_equal(decode_pymm_names("cit168.bn.str.pu.left"), "L Putamen")
  
  # AAL match: Precentral_L -> L Precentral
  expect_match(decode_pymm_names("aal.precentral.l"), "L Precentral")
  
  # DKT match
  dkt_decoded <- decode_pymm_names("dkt.labels.left.lateral.ventricle")
  expect_match(dkt_decoded, "L Lateral Ventricle", ignore.case = TRUE)
  
  # Stripping parentheticals check
  # JHU: FA-Posterior_thalamic_radiation_(include_optic_radiation)-Left -> L Posterior Thalamic Radiation
  expect_equal(humanize_anatomical_name("FA-Posterior_thalamic_radiation_(include_optic_radiation)-Left"), "L Posterior Thalamic Radiation")
  
  # Redundancy check
  expect_equal(humanize_anatomical_name("BN_STR_Pu_Left"), "L Putamen")
  expect_equal(humanize_anatomical_name("STR_Ca_Right"), "R Caudate")
})

test_that("interpret_simlr_vector uses decoding", {
  set.seed(1)
  # Actual SiMLR naming: modNamePC1
  v_mat <- matrix(0, nrow = 10, ncol = 2)
  v_mat[1, 1] <- 1.0 # High weight for first region
  v_mat[2, 1] <- 0.9 # High weight for second region
  
  simlrResult <- list(v = list(mod1 = v_mat))
  simlrMats <- list(mod1 = matrix(0, nrow = 50, ncol = 10))
  colnames(simlrMats[[1]]) <- c(
    "cit168.bn.str.pu.left", "aal.precentral.l", "dkt.labels.left.lateral.ventricle",
    paste0("unknown", 4:10)
  )
  
  interpreted <- interpret_simlr_vector(simlrResult, simlrMats, "mod1PC1", n2show = 3, shortnames = TRUE)
  
  # Check if top names are decoded AND humanized with L/R
  nms <- names(interpreted)
  expect_true("L Putamen" %in% nms || "L Precentral" %in% nms)
})
