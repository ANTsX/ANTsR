test_that("B-spline fitting preserves displacement weights", {
  fit <- function(weights = NULL) {
    fitBsplineDisplacementField(
      displacementOrigins = rbind(c(30, 32), c(34, 32)),
      displacements = rbind(c(6, 0), c(-6, 0)),
      displacementWeights = weights,
      origin = c(0, 0), spacing = c(1, 1), size = c(64, 64),
      direction = diag(2), numberOfFittingLevels = 3, meshSize = 1,
      enforceStationaryBoundary = FALSE
    )
  }
  # R indices are 1-based; this is voxel [32, 32, 0] in ANTsPy.
  ux <- function(weights) as.array(fit(weights))[1, 33, 33]
  expect_gt(ux(c(1, 1e-3)), 1)
  expect_lt(ux(c(1e-3, 1)), -1)
  expect_equal(as.array(fit()), as.array(fit(c(1, 1))))
  expect_error(fit(1), "length of displacement weights")
})

registration_test_labels <- function() {
  fixed <- array(0, c(32, 32))
  moving <- fixed
  fixed[10:18, 10:18] <- 1
  moving[12:20, 10:18] <- 1
  list(fixed = as.antsImage(fixed), moving = as.antsImage(moving))
}

test_that("NULL and transform files preserve label registration metrics", {
  images <- registration_test_labels()
  # Capture the completed command before entering the native registration code.
  capture_args <- function(initial, intensity = TRUE) {
    args <- NULL
    registration <- labelImageRegistration
    env <- new.env(parent = environment(registration))
    env$.int_antsProcessArguments <- function(x) {
      args <<- unlist(x)
      stop("captured registration arguments")
    }
    environment(registration) <- env
    expect_error(registration(
      images$fixed, images$moving, initialTransforms = initial,
      fixedIntensityImages = if (intensity) images$fixed else NULL,
      movingIntensityImages = if (intensity) images$moving else NULL
    ), "captured registration arguments")
    args
  }
  identity <- tempfile(fileext = ".mat")
  on.exit(unlink(identity))
  writeAntsrTransform(createAntsrTransform(dimension = 2), identity)
  reference <- capture_args("identity")
  metrics <- function(args) {
    values <- args[grepl("^MSQ\\[", args)]
    sub("^MSQ\\[[^,]+,[^,]+,", "MSQ[fixed,moving,", values)
  }
  expect_length(metrics(reference), 1)
  for (initial in list(NULL, identity, list(identity), c(identity, identity))) {
    args <- capture_args(initial)
    expect_equal(metrics(args), metrics(reference))
    expect_equal(sum(args == "-r"), length(initial))
  }
  expect_equal(metrics(capture_args(NULL, FALSE)), metrics(reference))
})

test_that("label-only registration runs without an initial transform", {
  images <- registration_test_labels()
  directory <- tempfile()
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE))
  result <- labelImageRegistration(
    images$fixed, images$moving, initialTransforms = NULL,
    typeOfDeformableTransform = list("SyN", 0.2, "CC", 2, c(2, 0), c(1, 0), c(2, 1)),
    outputPrefix = file.path(directory, "reg")
  )
  expect_length(result$fwdtransforms, 1)
  expect_true(all(file.exists(result$fwdtransforms)))
})
