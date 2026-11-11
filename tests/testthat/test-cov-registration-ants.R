# Coverage tests for antsRegistration(): argument assembly for every
# typeofTransform (captured before native code runs) plus a handful of real,
# fast registrations on small 2D images.

reg_small_pair <- function() {
  fi <- resampleImage(ri(1), c(4, 4))
  mi <- resampleImage(ri(2), c(4, 4))
  list(fixed = fi, moving = mi)
}

# Run antsRegistration but stop right before the native call and return the
# assembled argument vector.
capture_reg_args <- function(...) {
  captured <- NULL
  registration <- antsRegistration
  env <- new.env(parent = environment(registration))
  env$.int_antsProcessArguments <- function(x) {
    captured <<- unlist(x)
    stop("captured registration arguments")
  }
  environment(registration) <- env
  expect_error(
    suppressMessages(registration(...)),
    "captured registration arguments"
  )
  captured
}

arg_after <- function(args, flag) args[which(args == flag) + 1]

test_that("antsRegistration builds expected stages for linear transforms", {
  p <- reg_small_pair()
  for (tt in c("Translation", "Rigid", "Similarity", "Affine")) {
    a <- capture_reg_args(p$fixed, p$moving, typeofTransform = tt)
    expect_equal(arg_after(a, "-t"), paste0(tt, "[0.25]"))
    expect_equal(arg_after(a, "-c"), "2100x1200x1200x10")
    expect_equal(arg_after(a, "-d"), "2")
    expect_true("--float" %in% a)
    expect_equal(arg_after(a, "--write-composite-transform"), "0")
    expect_equal(arg_after(a, "-x"), "[NA,NA]")
  }
  a <- capture_reg_args(p$fixed, p$moving, typeofTransform = "AffineFast")
  expect_equal(arg_after(a, "-t"), "Affine[0.25]")
  expect_equal(arg_after(a, "-c"), "2100x1200x0x0")
  a <- capture_reg_args(p$fixed, p$moving, typeofTransform = "BOLDAffine")
  expect_equal(arg_after(a, "-c"), "100x20")
  expect_equal(arg_after(a, "-f"), "2x1")
  a <- capture_reg_args(p$fixed, p$moving, typeofTransform = "QuickRigid")
  expect_equal(arg_after(a, "-t"), "Rigid[0.25]")
  expect_equal(arg_after(a, "-c"), "20x20x0x0")
  a <- capture_reg_args(p$fixed, p$moving, typeofTransform = "DenseRigid")
  expect_match(arg_after(a, "-m"), ",0.8\\]$")
  a <- capture_reg_args(p$fixed, p$moving, typeofTransform = "BOLDRigid")
  expect_equal(arg_after(a, "-s"), "1x0")
  # affIterations controls levels, a single level means no pyramid.
  a <- capture_reg_args(p$fixed, p$moving, typeofTransform = "Affine",
                        affIterations = c(30, 20, 10))
  expect_equal(arg_after(a, "-c"), "30x20x10")
  expect_equal(arg_after(a, "-s"), "2x1x0")
  expect_equal(arg_after(a, "-f"), "4x2x1")
  a <- capture_reg_args(p$fixed, p$moving, typeofTransform = "Affine",
                        affIterations = 15)
  expect_equal(arg_after(a, "-c"), "15")
  expect_equal(arg_after(a, "-s"), "0")
  expect_equal(arg_after(a, "-f"), "1")
})

test_that("antsRegistration builds expected stages for deformable transforms", {
  p <- reg_small_pair()
  expectations <- list(
    SyN = c("Affine[0.25]", "SyN[0.2,3,0]"),
    SyNRA = c("Rigid[0.25]", "Affine[0.25]", "SyN[0.2,3,0]"),
    SyNOnly = c("SyN[0.2,3,0]"),
    ElasticOnly = c("GaussianDisplacementField[0.2,3,0]"),
    SyNCC = c("Rigid[1]", "Affine[1]", "SyN[0.15,3,0]"),
    SyNabp = c("Rigid[0.1]", "Affine[0.1]", "SyN[0.1,3,0]"),
    SyNBold = c("Rigid[0.25]", "SyN[0.2,3,0]"),
    SyNBoldAff = c("Rigid[0.25]", "Affine[0.25]", "SyN[0.2,3,0]"),
    SyNAggro = c("Affine[0.25]", "SyN[0.2,3,0]"),
    SyNLessAggro = c("Affine[0.25]", "SyN[0.2,3,0]"),
    ElasticSyN = c("Affine[0.25]", "SyN[0.2,3,0]"),
    Elastic = c("GaussianDisplacementField[0.2,3,0]"),
    TRSAA = c("Translation[1]", "Rigid[1]", "Similarity[1]", "Affine[1]", "Affine[1]")
  )
  for (tt in names(expectations)) {
    a <- capture_reg_args(p$fixed, p$moving, typeofTransform = tt,
                          regIterations = c(10, 5, 2))
    expect_equal(a[which(a == "-t") + 1], expectations[[tt]], info = tt)
    expect_true(any(grepl("^\\[", a[which(a == "-o") + 1])), info = tt)
  }
  # SyN pyramid derived from regIterations; single level -> no pyramid
  a <- capture_reg_args(p$fixed, p$moving, typeofTransform = "SyN",
                        regIterations = c(10, 5, 2))
  expect_equal(tail(a[which(a == "-s") + 1], 1), "2x1x0")
  expect_equal(tail(a[which(a == "-f") + 1], 1), "4x2x1")
  a <- capture_reg_args(p$fixed, p$moving, typeofTransform = "SyN",
                        regIterations = 7)
  expect_equal(tail(a[which(a == "-s") + 1], 1), "0")
  expect_equal(tail(a[which(a == "-f") + 1], 1), "1")
  # SyNOnly and deformable-only types start from identity
  a <- capture_reg_args(p$fixed, p$moving, typeofTransform = "SyNOnly")
  expect_equal(arg_after(a, "-r"), "Identity")
  a <- capture_reg_args(p$fixed, p$moving, typeofTransform = "SyN")
  expect_match(arg_after(a, "-r"), ",1\\]$")
})

test_that("antsRegistration builds time-varying velocity field stages", {
  p <- reg_small_pair()
  a <- capture_reg_args(p$fixed, p$moving, typeofTransform = "TVMSQ")
  expect_match(arg_after(a, "-t"), "^TimeVaryingVelocityField\\[0.2, 4,3")
  expect_equal(arg_after(a, "-r"), "Identity")
  a <- capture_reg_args(p$fixed, p$moving, typeofTransform = "TVMSQ", gradStep = NA)
  expect_match(arg_after(a, "-t"), "^TimeVaryingVelocityField\\[1, 4")
  a <- capture_reg_args(p$fixed, p$moving, typeofTransform = "TVMSQC")
  expect_match(arg_after(a, "-t"), "8, 1.0,0.0, 0.05")
  expect_equal(sum(a == "-m"), 2)
  a <- capture_reg_args(p$fixed, p$moving, typeofTransform = "TVMSQC", gradStep = NA)
  expect_match(arg_after(a, "-t"), "Field\\[2, 8")
  a <- capture_reg_args(p$fixed, p$moving, typeofTransform = "TV[3]", gradStep = NA)
  expect_match(arg_after(a, "-t"), "^TimeVaryingVelocityField\\[1,3,3")
  a <- capture_reg_args(p$fixed, p$moving, typeofTransform = "Elastic", gradStep = NA)
  expect_equal(arg_after(a, "-t"), "GaussianDisplacementField[0.25,3,0]")
})

test_that("antsRegistration builds antsRegistrationSyN-style stages", {
  p <- reg_small_pair()
  tx_of <- function(a) a[which(a == "--transform") + 1]
  a <- capture_reg_args(p$fixed, p$moving, typeofTransform = "antsRegistrationSyN[s]")
  expect_equal(tx_of(a), c("Rigid[0.1]", "Affine[", "SyN[0.2,3,0]"))
  expect_true(any(grepl("^CC\\[", a[which(a == "--metric") + 1])))
  expect_true("[100x70x50x20,1e-6,10]" %in% a)
  a <- capture_reg_args(p$fixed, p$moving, typeofTransform = "antsRegistrationSyNQuick[s]")
  expect_true(any(grepl("^MI\\[.*,1,32\\]$", a[which(a == "--metric") + 1])))
  expect_true("[1000x500x250x0,1e-6,10]" %in% a)
  a <- capture_reg_args(p$fixed, p$moving, typeofTransform = "antsRegistrationSyN[r]")
  expect_equal(tx_of(a), "Rigid[0.1]")
  a <- capture_reg_args(p$fixed, p$moving, typeofTransform = "antsRegistrationSyN[t]")
  expect_equal(tx_of(a), "Translation[0.1]")
  a <- capture_reg_args(p$fixed, p$moving, typeofTransform = "antsRegistrationSyNRepro[a]")
  expect_equal(tx_of(a), c("Rigid[0.1]", "Affine["))
  expect_true(any(grepl("^GC\\[", a[which(a == "--metric") + 1])))
  a <- capture_reg_args(p$fixed, p$moving, typeofTransform = "antsRegistrationSyN[b]")
  expect_equal(tail(tx_of(a), 1), "BSplineSyN[0.2,26,0,3]")
  a <- capture_reg_args(p$fixed, p$moving, typeofTransform = "antsRegistrationSyNQuick[br]")
  expect_equal(tx_of(a), c("Rigid[0.1]", "BSplineSyN[0.2,26,0,3]"))
  expect_true("[50x0,1e-6,10]" %in% a)
  a <- capture_reg_args(p$fixed, p$moving, typeofTransform = "antsRegistrationSyN[sr]")
  expect_true("[50x20,1e-6,10]" %in% a)
  expect_equal(arg_after(a, "--shrink-factors")[2], "2x1")
  a <- capture_reg_args(p$fixed, p$moving, typeofTransform = "antsRegistrationSyN[so]")
  expect_equal(tx_of(a), "SyN[0.2,3,0]")
  expect_equal(arg_after(a, "-r"), "Identity")
  a <- capture_reg_args(p$fixed, p$moving, typeofTransform = "antsRegistrationSyNQuickRepro[bo]")
  expect_equal(tx_of(a), "BSplineSyN[0.2,26,0,3]")
  a <- capture_reg_args(p$fixed, p$moving, typeofTransform = "antsRegistrationSyN[b,4,30]")
  expect_equal(tail(tx_of(a), 1), "BSplineSyN[0.2,30,0,3]")
  expect_true(any(grepl("^CC\\[.*,1,4\\]$", a[which(a == "--metric") + 1])))
  a <- capture_reg_args(p$fixed, p$moving, typeofTransform = "antsRegistrationSyN[s,8]")
  expect_true(any(grepl("^CC\\[.*,1,8\\]$", a[which(a == "--metric") + 1])))
  expect_error(
    suppressMessages(antsRegistration(p$fixed, p$moving,
      typeofTransform = "antsRegistrationSyN[a,4]")),
    "only valid for B-spline SyN"
  )
  # multivariate extras added to the SyN stage
  extras <- list(list("MSQ", p$fixed, p$moving, 0.5, 0))
  a <- capture_reg_args(p$fixed, p$moving, typeofTransform = "antsRegistrationSyN[so]",
                        multivariateExtras = extras)
  expect_true(any(grepl("^MSQ\\[.*,0.5,0\\]$", a)))
  expect_error(
    suppressMessages(antsRegistration(p$fixed, p$moving,
      typeofTransform = "antsRegistrationSyN[so]",
      multivariateExtras = list(list("MSQ", p$fixed)))),
    "5 entries"
  )
})

test_that("antsRegistration handles masks, initial transforms and options", {
  p <- reg_small_pair()
  fmask <- getMask(p$fixed)
  mmask <- getMask(p$moving)
  a <- capture_reg_args(p$fixed, p$moving, typeofTransform = "SyN", mask = fmask)
  x <- a[which(a == "-x") + 1]
  expect_equal(x[1], "[NA,NA]")
  expect_false(grepl("NA,", x[2]))
  expect_match(x[2], ",NA\\]$")
  a <- capture_reg_args(p$fixed, p$moving, typeofTransform = "SyN",
                        mask = fmask, movingMask = mmask, maskAllStages = TRUE)
  x <- a[which(a == "-x") + 1]
  expect_equal(x[1], x[2])
  expect_false(grepl("NA", x[1]))
  a <- capture_reg_args(p$fixed, p$moving, typeofTransform = "Affine",
                        mask = list(fmask, mmask))
  expect_false(grepl("NA", arg_after(a, "-x")))
  expect_error(
    suppressMessages(antsRegistration(p$fixed, p$moving, "Affine",
      mask = list(fmask, mmask), movingMask = mmask)),
    "Ambiguous"
  )
  # antsrTransform initial transform gets written to a .mat
  tx <- createAntsrTransform(type = "AffineTransform", dimension = 2,
                             translation = c(1, 2))
  a <- capture_reg_args(p$fixed, p$moving, typeofTransform = "Rigid",
                        initialTransform = tx)
  expect_match(arg_after(a, "-r"), "\\.mat$")
  expect_true(file.exists(arg_after(a, "-r")))
  # restrictTransformation, verbose, random seed, composite
  old <- options(randomSeed = 42)
  on.exit(options(old), add = TRUE)
  a <- capture_reg_args(p$fixed, p$moving, typeofTransform = "Affine",
                        restrictTransformation = c(1, 0, 1, 0, 1, 1),
                        verbose = TRUE, writeCompositeTransform = TRUE)
  expect_equal(arg_after(a, "-g"), "1x0x1x0x1x1")
  expect_equal(arg_after(a, "-v"), "1")
  expect_equal(arg_after(a, "--random-seed"), "42")
  expect_equal(arg_after(a, "--write-composite-transform"), "1")
  # SyNOnly with multivariate extras
  extras <- list(list("CC", p$fixed, p$moving, 1, 2))
  a <- capture_reg_args(p$fixed, p$moving, typeofTransform = "SyNOnly",
                        multivariateExtras = extras)
  expect_equal(sum(a == "-m"), 2)
  expect_true(any(grepl("^CC\\[.*,1,2\\]$", a)))
  expect_error(
    suppressMessages(antsRegistration(p$fixed, p$moving, "SyNOnly",
      multivariateExtras = list(list("CC", p$fixed, p$moving)))),
    "5 entries"
  )
  # printArgs echoes the command line
  out <- capture.output(capture_reg_args(p$fixed, p$moving, "Rigid", printArgs = TRUE))
  expect_true(any(grepl("^antsRegistration -d 2", out)))
})

test_that("antsRegistration validates its input", {
  p <- reg_small_pair()
  expect_error(suppressMessages(antsRegistration(p$fixed, p$moving, "NotATransform")),
               "Unrecognized transform type")
  img3 <- makeImage(c(4, 4, 4), 1)
  expect_error(antsRegistration(p$fixed, img3, "Rigid"), "dimensions are not the same")
  # empty transform string defaults to SyN
  a <- capture_reg_args(p$fixed, p$moving, typeofTransform = "")
  expect_equal(a[which(a == "-t") + 1], c("Affine[0.25]", "SyN[0.2,3,0]"))
})

test_that("antsRegistration emits determinism message for non-Repro types", {
  p <- reg_small_pair()
  registration <- antsRegistration
  env <- new.env(parent = environment(registration))
  env$.int_antsProcessArguments <- function(x) stop("stop here")
  environment(registration) <- env
  old <- options(ants.deterministic = TRUE)
  on.exit(options(old))
  expect_message(try(registration(p$fixed, p$moving, "Rigid"), silent = TRUE),
                 "not deterministic")
})

test_that("real linear registrations reduce the image mismatch", {
  set.seed(1)
  p <- reg_small_pair()
  before <- mean(abs(p$fixed - p$moving))
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  for (tt in c("Translation", "Rigid", "Affine")) {
    out <- file.path(dir, tt)
    reg <- suppressMessages(antsRegistration(p$fixed, p$moving, typeofTransform = tt,
                                             outprefix = out, affIterations = c(40, 20)))
    expect_named(reg, c("warpedmovout", "warpedfixout", "fwdtransforms",
                        "invtransforms", "prev_transforms"))
    expect_length(reg$fwdtransforms, 1)
    expect_match(reg$fwdtransforms, "GenericAffine\\.mat$")
    expect_true(all(file.exists(reg$fwdtransforms)))
    expect_equal(dim(reg$warpedmovout), dim(p$fixed))
    expect_equal(antsGetSpacing(reg$warpedmovout), antsGetSpacing(p$fixed))
    expect_lt(mean(abs(p$fixed - reg$warpedmovout)), before)
    # applying the forward transform reproduces the warped image
    w <- antsApplyTransforms(p$fixed, p$moving, reg$fwdtransforms)
    expect_lt(mean(abs(w - reg$warpedmovout)), 1)
  }
  # rerun with same prefix: earlier transforms reported as prev_transforms
  reg2 <- suppressMessages(antsRegistration(p$fixed, p$moving, "Rigid",
    outprefix = file.path(dir, "Rigid"), affIterations = c(10, 5)))
  expect_true(length(reg2$prev_transforms) >= 1)
})

test_that("real deformable registrations return warps and inverse warps", {
  set.seed(2)
  p <- reg_small_pair()
  before <- mean(abs(p$fixed - p$moving))
  reg <- suppressMessages(antsRegistration(p$fixed, p$moving, typeofTransform = "SyNOnly",
                                           regIterations = c(20, 10)))
  expect_length(reg$fwdtransforms, 2)
  expect_match(reg$fwdtransforms[1], "Warp\\.nii\\.gz$")
  expect_match(reg$invtransforms[2], "InverseWarp\\.nii\\.gz$")
  expect_lt(mean(abs(p$fixed - reg$warpedmovout)), before)
  expect_equal(dim(reg$warpedfixout), dim(p$moving))
  # composite transform output
  reg <- suppressMessages(antsRegistration(p$fixed, p$moving, typeofTransform = "Rigid",
                                           writeCompositeTransform = TRUE,
                                           affIterations = c(10, 5)))
  expect_match(reg$fwdtransforms, "Composite\\.h5$")
  expect_match(reg$invtransforms, "InverseComposite\\.h5$")
  expect_true(file.exists(reg$fwdtransforms))
  # initial transform from a previous registration
  lin <- suppressMessages(antsRegistration(p$fixed, p$moving, "Affine",
                                           affIterations = c(20, 10)))
  reg <- suppressMessages(antsRegistration(p$fixed, p$moving, "SyNOnly",
                                           initialTransform = lin$fwdtransforms,
                                           regIterations = c(10, 5)))
  expect_lt(mean(abs(p$fixed - reg$warpedmovout)), before)
})

test_that("time-varying registration returns a velocity field", {
  fi <- resampleImage(ri(1), c(8, 8))
  mi <- resampleImage(ri(2), c(8, 8))
  reg <- suppressMessages(antsRegistration(fi, mi, typeofTransform = "TV[2]",
                                           regIterations = c(5, 2)))
  expect_true("velocityfield" %in% names(reg))
  expect_true(file.exists(reg$velocityfield))
  vf <- antsImageRead(reg$velocityfield)
  expect_equal(vf@dimension, 3)
  expect_equal(vf@components, 2)
})

test_that("antsRegistration prints usage when called without images", {
  out <- capture.output(res <- antsRegistration())
  expect_equal(res, 0)
  expect_true(any(grepl("simplified mode", out)))
})
