# Closed-form multiplicative soft-polar operator:
#   Y %*% ((1 - w) I + w (Y'Y + eps I)^{-1/2}), rescaled to the norm of Y
multiplicative_soft_polar <- function(Y, w, eps = 1e-8) {
  e <- eigen(crossprod(Y), symmetric = TRUE)
  Tm <- e$vectors %*% diag(1 / sqrt(pmax(e$values, 0) + eps), ncol(Y)) %*% t(e$vectors)
  Yt <- Y %*% ((1 - w) * diag(ncol(Y)) + w * Tm)
  Yt / sqrt(sum(Yt^2)) * sqrt(sum(Y^2))
}

test_that("nsa_flow_retract_auto soft_polar blends Y with its polar factor", {
  set.seed(1)
  for (dims in list(c(20, 4), c(4, 10))) {
    Y <- matrix(rnorm(prod(dims)), dims[1])
    nrm <- sqrt(sum(Y^2))
    for (w in c(0, 0.25, 0.5, 1)) {
      sp <- nsa_flow_retract_auto(Y, w, "soft_polar")
      expect_equal(dim(sp), dim(Y))
      expect_equal(sqrt(sum(sp^2)), nrm, tolerance = 1e-10)
      # same operator as the closed-form multiplicative soft-polar
      expect_equal(sp, multiplicative_soft_polar(Y, w), tolerance = 1e-6)
    }
    # endpoints: w = 0 leaves Y alone, w = 1 is the polar retraction
    expect_equal(nsa_flow_retract_auto(Y, 0, "soft_polar"), Y, tolerance = 1e-10)
    expect_equal(nsa_flow_retract_auto(Y, 1, "soft_polar"),
                 nsa_flow_retract_auto(Y, 1, "polar"), tolerance = 1e-10)
    # strictly in between for 0 < w < 1
    mid <- nsa_flow_retract_auto(Y, 0.5, "soft_polar")
    expect_gt(max(abs(mid - Y)), 1e-3)
    expect_gt(max(abs(mid - nsa_flow_retract_auto(Y, 1, "polar"))), 1e-3)
  }
  Y <- matrix(rnorm(12), 4)
  expect_identical(nsa_flow_retract_auto(Y, 0.5, "none"), Y)
  expect_error(nsa_flow_retract_auto(1:3, 0.5, "polar"), "must be a matrix")
})

test_that("polar retraction has orthonormal columns up to the preserved norm", {
  set.seed(2)
  Y <- matrix(rnorm(30), 10)
  P <- nsa_flow_retract_auto(Y, 1, "polar")
  s <- sqrt(sum(Y^2)) / sqrt(ncol(Y))
  expect_equal(crossprod(P) / s^2, diag(ncol(Y)), tolerance = 1e-8)
})
