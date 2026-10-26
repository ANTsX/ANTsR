# Generics exported by ANTsR must not break the stats/base functions they mask.

test_that("stats::step still works on lm objects after loading ANTsR", {
  fit <- lm(mpg ~ wt + hp + qsec, data = mtcars)
  res <- step(fit, trace = 0)
  expect_s3_class(res, "lm")
  expect_equal(coef(res), coef(stats::step(fit, trace = 0)))
})

test_that("ANTsR does not export a step() generic", {
  expect_false("step" %in% getNamespaceExports("ANTsR"))
  expect_true("optimizer_step" %in% getNamespaceExports("ANTsR"))
})

test_that("sd() on non-image input matches stats::sd, including na.rm", {
  x <- c(1, 2, NA, 4)
  expect_identical(sd(x, na.rm = TRUE), stats::sd(x, na.rm = TRUE))
  expect_identical(sd(x), stats::sd(x))
  expect_true(is.na(sd(x)))
  expect_identical(sd(x, TRUE), stats::sd(x, TRUE))
  expect_identical(sd(1:10), stats::sd(1:10))
  expect_identical(names(formals(sd))[1:2], names(formals(stats::sd)))
})

test_that("var() on non-image input matches stats::var, including na.rm", {
  x <- c(1, 2, NA, 4)
  expect_identical(var(x, na.rm = TRUE), stats::var(x, na.rm = TRUE))
  expect_identical(var(1:10, 10:1), stats::var(1:10, 10:1))
})

test_that("sd() and var() on an antsImage still work, with and without a mask", {
  set.seed(1)
  vals <- rnorm(1000)
  img <- makeImage(c(10, 10, 10), vals)
  expect_equal(sd(img), stats::sd(vals), tolerance = 1e-5)
  expect_equal(var(img), stats::var(vals), tolerance = 1e-5)
  mask <- img > 0
  expect_equal(sd(img, mask = mask), stats::sd(vals[vals > 0]), tolerance = 1e-5)
})

test_that("optimizer_step dispatches on the optimizer class", {
  V <- matrix(1, 3, 2)
  opt <- create_optimizer("adam", list(V))
  expect_s3_class(opt, "adam")
  res <- optimizer_step(opt, i = 1, V_current = V,
                        descent_gradient = -V, learning_rate = 0.1)
  expect_named(res, c("updated_V", "optimizer"))
  expect_true(all(res$updated_V < V))
  expect_true("adam" %in% list_simlr_optimizers())
})
