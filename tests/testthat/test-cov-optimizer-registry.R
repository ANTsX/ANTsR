# Only optimizers ANTsR itself defines are listed / creatable, whichever name
# (`step` or `optimizer_step`) the generic has.

test_that("list_simlr_optimizers ignores optimizer_step methods from other packages", {
  # a visible `optimizer_step.*` method defined outside ANTsR (the generic
  # discovery must not pick it up via methods("optimizer_step"))
  assign("optimizer_step.not_antsr", function(optimizer, ...) NULL, envir = globalenv())
  on.exit(rm("optimizer_step.not_antsr", envir = globalenv()), add = TRUE)
  expect_true("not_antsr" %in% sub("^optimizer_step\\.", "", as.character(methods("optimizer_step"))))
  opts <- list_simlr_optimizers()
  expect_false("not_antsr" %in% opts)
  expect_false(ANTsR:::.simlr_optimizer_exists("not_antsr"))
  expect_error(create_optimizer("not_antsr", vmats = list(matrix(0, 2, 2))),
               "Unknown optimizer")
  expect_false("default" %in% opts)
  expect_true(all(c("adam", "nadam", "hybrid_adam") %in% opts))
  expect_false(any(c("lbfgs", "random_search", "random_gradient") %in% opts))
  for (o in opts) expect_true(ANTsR:::.simlr_optimizer_exists(o), info = o)
})

test_that("legacy step.* methods from other packages are ignored too", {
  # simulate robust::step.lmRob on the `step` generic
  assign("step.lmRob", function(object, ...) NULL, envir = globalenv())
  on.exit(rm("step.lmRob", envir = globalenv()), add = TRUE)
  expect_false("lmRob" %in% list_simlr_optimizers())
  expect_false(ANTsR:::.simlr_optimizer_exists("lmRob"))
})

test_that(".simlr_optimizer_exists is strict", {
  ex <- ANTsR:::.simlr_optimizer_exists
  expect_true(ex("adam"))
  expect_true(ex("lbfgs"))
  expect_false(ex("lmRob"))
  expect_false(ex("default"))
  expect_false(ex("not_an_antsr_optimizer"))
  expect_false(ex(c("adam", "nadam")))
  expect_false(ex(NA_character_))
  expect_false(ex(1))
})

test_that("create_optimizer rejects unknown optimizers", {
  v <- list(matrix(0, 2, 2))
  expect_error(create_optimizer("not_an_antsr_optimizer", vmats = v),
               "Unknown optimizer")
  expect_error(create_optimizer("lmRob", vmats = v), "Available optimizers")
  expect_s3_class(create_optimizer("adam", vmats = v), "simlr_optimizer")
})
