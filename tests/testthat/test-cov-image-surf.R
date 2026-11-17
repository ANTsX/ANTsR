skip_on_os("windows")

# Fake ANTs command line tools: the converter does nothing and the renderers
# copy a small template png to any "-d <file>.png[...]" target.
make_fake_antspath <- function() {
  d <- tempfile("fakeants")
  dir.create(d)
  tpl <- file.path(d, "tpl.png")
  grDevices::png(tpl, width = 8, height = 6)
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  grDevices::dev.off()
  render <- c(
    "#!/bin/sh",
    "while [ $# -gt 0 ]; do",
    "  if [ \"$1\" = \"-d\" ]; then",
    "    shift",
    "    case \"$1\" in",
    paste0("      *.png*) cp '", tpl, "' \"${1%%\\[*}\" ;;"),
    "    esac",
    "  fi",
    "  shift",
    "done",
    "exit 0"
  )
  for (tool in c("antsSurf", "antsVol")) {
    writeLines(render, file.path(d, tool))
  }
  writeLines(c("#!/bin/sh", "exit 0"), file.path(d, "ConvertScalarImageToRGB"))
  Sys.chmod(file.path(d, c("antsSurf", "antsVol", "ConvertScalarImageToRGB")), "755")
  d
}

make_vol_data <- function() {
  set.seed(1)
  x <- makeImage(c(8, 8, 8), 0)
  x[2:7, 2:7, 2:7] <- 1
  ov <- makeImage(c(8, 8, 8), 0)
  ov[3:6, 3:6, 3:6] <- runif(64, 1, 5)
  list(x = x, ov = ov)
}

test_that("antsrVol builds commands and stitches multi-view pngs", {
  skip_if_not_installed("png")
  skip_if_not_installed("abind")
  ap <- make_fake_antspath()
  on.exit(unlink(ap, recursive = TRUE), add = TRUE)
  d <- make_vol_data()
  fn <- tempfile("vol")
  rot <- rbind(c(270, 0, 270), c(90, 180, 90))
  file.create(paste0(fn, c("001.png", "002.png"))) # avoid noisy `rm` of missing files
  invisible(capture.output(suppressWarnings(
    antsrVol(d$x, list(d$ov),
      rotationParams = rot, filename = fn, antspath = ap,
      intensityTruncation = c(0.01, 0.99), overlayLimits = c(1, 4), verbose = TRUE
    )
  ), type = "output"))
  expect_true(file.exists(paste0(fn, ".png")))
  out <- png::readPNG(paste0(fn, ".png"))
  expect_equal(dim(out)[1:2], c(6, 16))
  # single view, no filename
  expect_null(antsrVol(d$x, list(d$ov), antspath = ap, colormap = c("jet", "hot")))
  old <- Sys.getenv("ANTSPATH", unset = NA)
  Sys.unsetenv("ANTSPATH")
  expect_error(antsrVol(d$x), "ANTSPATH")
  if (!is.na(old)) Sys.setenv(ANTSPATH = old)
  unlink(paste0(fn, c("001.png", "002.png", ".png")))
})

test_that("antsrSurf builds commands and stitches multi-view pngs", {
  skip_if_not_installed("png")
  skip_if_not_installed("abind")
  ap <- make_fake_antspath()
  on.exit(unlink(ap, recursive = TRUE), add = TRUE)
  d <- make_vol_data()
  fn <- tempfile("surf")
  rot <- rbind(c(270, 0, 270), c(90, 180, 90))
  file.create(paste0(fn, c("001.png", "002.png"))) # avoid noisy `rm` of missing files
  invisible(capture.output(suppressWarnings(
    antsrSurf(d$x, list(d$ov), list(thresholdImage(d$ov, 0.5, Inf)),
      rotationParams = rot, filename = fn, antspath = ap,
      backgroundColor = "black", overlayLimits = c(1, 4), verbose = TRUE
    )
  ), type = "output"))
  out <- png::readPNG(paste0(fn, ".png"))
  expect_equal(dim(out)[1:2], c(6, 16))
  expect_error(antsrSurf(d$x, list(d$ov), list()), "mask in z")
  old <- Sys.getenv("ANTSPATH", unset = NA)
  Sys.unsetenv("ANTSPATH")
  expect_error(antsrSurf(d$x, list(d$ov), list(d$ov)), "ANTSPATH")
  if (!is.na(old)) Sys.setenv(ANTSPATH = old)
  expect_error(antsrSurf(d$x, list(d$ov), list(d$ov), antspath = tempdir()), "antsSurf")
  unlink(paste0(fn, c("001.png", "002.png", ".png")))
})
