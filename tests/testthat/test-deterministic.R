# Run `fun` with its native call replaced so the assembled antsRegistration
# command line is returned instead of executed.
capture_ants_args <- function(fun, ...) {
  args <- NULL
  env <- new.env(parent = environment(fun))
  env$.int_antsProcessArguments <- function(x) {
    args <<- unlist(x)
    stop("captured registration arguments")
  }
  environment(fun) <- env
  expect_error(fun(...), "captured registration arguments")
  args
}

seed_arg <- function(args) {
  i <- which(args == "--random-seed")
  if (length(i)) args[i + 1] else NULL
}

deterministic_test_images <- function() {
  fixed <- array(0, c(32, 32))
  moving <- fixed
  fixed[10:18, 10:18] <- 1
  moving[12:20, 10:18] <- 1
  list(fixed = as.antsImage(fixed), moving = as.antsImage(moving))
}

test_that(".onLoad sets ants.* defaults without overriding user values", {
  old <- options(ants.deterministic = NULL, ants.randomSeed = NULL)
  on.exit(options(old))
  ANTsR:::.onLoad("", "ANTsR")
  expect_true("ants.deterministic" %in% names(options()))
  expect_false(getOption("ants.deterministic"))
  expect_null(getOption("ants.randomSeed"))
  options(ants.deterministic = TRUE)
  ANTsR:::.onLoad("", "ANTsR")
  expect_true(getOption("ants.deterministic"))
})

test_that("setANTsDeterministic seeds antsRegistration and can be undone", {
  old <- options(ants.deterministic = FALSE, ants.randomSeed = NULL)
  old_env <- Sys.getenv(c("ANTS_RANDOM_SEED", "ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS"),
    unset = NA
  )
  on.exit({
    options(old)
    for (v in names(old_env)) {
      if (is.na(old_env[[v]])) Sys.unsetenv(v) else do.call(Sys.setenv, as.list(old_env[v]))
    }
  })
  images <- deterministic_test_images()
  reg <- function(...) {
    capture_ants_args(antsRegistration, images$fixed, images$moving,
      typeofTransform = "Rigid", ...
    )
  }
  expect_silent(args <- reg())
  expect_null(seed_arg(args))

  setANTsDeterministic(TRUE, seedValue = 42)
  expect_true(getOption("ants.deterministic"))
  expect_identical(getOption("ants.randomSeed"), 42)
  expect_identical(Sys.getenv("ANTS_RANDOM_SEED"), "42")
  expect_message(args <- reg(), "not deterministic")
  expect_identical(seed_arg(args), "42")

  setANTsDeterministic(FALSE)
  expect_null(getOption("ants.randomSeed"))
  expect_identical(Sys.getenv("ANTS_RANDOM_SEED"), "")
  expect_null(seed_arg(reg()))
})

test_that("labelImageRegistration appends the random seed", {
  old <- options(ants.randomSeed = NULL)
  on.exit(options(old))
  images <- deterministic_test_images()
  reg <- function(...) {
    capture_ants_args(labelImageRegistration, images$fixed, images$moving,
      initialTransforms = NULL,
      typeOfDeformableTransform = "antsRegistrationSyNQuick[so]", ...
    )
  }
  base <- reg()
  expect_null(seed_arg(base))
  explicit <- reg(randomSeed = 7)
  expect_identical(seed_arg(explicit), "7")
  # the seed is appended, not substituted for the whole command line
  expect_length(explicit, length(base) + 2)
  expect_true(all(c("-x", "--float") %in% explicit))
  options(ants.randomSeed = 99)
  expect_identical(seed_arg(reg()), "99")
  expect_identical(seed_arg(reg(randomSeed = 7)), "7")
})
