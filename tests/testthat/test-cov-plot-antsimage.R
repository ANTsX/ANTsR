# Coverage tests for plot.antsImage branches: overlays, axes, slices,
# colorbars, windows, outname, domain maps and text annotations.

cov_plot_vol <- function() {
  set.seed(1)
  d <- c(16, 16, 12)
  g <- as.matrix(expand.grid(1:d[1], 1:d[2], 1:d[3]))
  r <- array(sqrt(rowSums(sweep(g, 2, (d + 1) / 2)^2 / (d / 2.2)^2)), d)
  arr <- ifelse(r < 1, 100 + 50 * (1 - r), 0) + abs(rnorm(prod(d)))
  img <- as.antsImage(arr)
  ov <- as.antsImage(ifelse(r < 0.5, 10 * (1 - r), 0))
  list(img = img, ov = ov)
}

test_that("plot.antsImage writes jpeg output for 2D and 3D images", {
  skip_if_not_installed("pixmap")
  v <- cov_plot_vol()
  fn <- tempfile(fileext = ".jpg")
  on.exit(unlink(fn), add = TRUE)
  plot(v$img, outname = fn, axis = 3, nslices = 4, ncolumns = 2)
  expect_gt(file.size(fn), 0)
  unlink(fn)
  r16 <- antsImageRead(getANTsRData("r16"))
  plot(r16, r16 > 150, outname = fn, window.overlay = c(0.5, 1))
  expect_gt(file.size(fn), 0)
})

test_that("plot.antsImage overlay options across axes and colormaps", {
  skip_if_not_installed("pixmap")
  skip_if_not_installed("viridis")
  v <- cov_plot_vol()
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  for (ax in 1:3) {
    expect_null(plot(v$img, v$ov, axis = ax, slices = c(4, 6, 8), window.overlay = c(1, 10)))
  }
  for (col in c("red", "jet", "viridis", "magma", "plasma", "inferno")) {
    expect_null(plot(v$img, v$ov,
      color.overlay = col, slices = "3x9x2", axis = 3,
      alpha = 0.5, useAbsoluteScale = TRUE, title.img = "t", title.line = 1
    ))
  }
  # several overlays, no cropping
  expect_null(plot(v$img, list(v$ov, v$ov * 0.5),
    color.overlay = c("red", "blue"), doCropping = FALSE,
    nslices = 3
  ))
  # a character window ("1x10") is documented but eps is subtracted before
  # it is parsed (package bug)
  expect_error(plot(v$img, v$ov, window.overlay = "1x10", nslices = 2), "non-numeric")
  # overlay window entirely invalid -> returns early
  expect_null(plot(v$img, v$ov, window.overlay = c(10, 1), nslices = 2))
  # overlay with a different physical space is resampled
  ov2 <- resampleImage(v$ov, c(2, 2, 2), useVoxels = FALSE)
  expect_null(plot(v$img, ov2, nslices = 2))
  expect_null(plot(v$img,
    text = list(x = 0.5, y = 0.5, label = "x", cex = 1, col = "red"),
    nslices = 2
  ))
})

test_that("plot.antsImage colorbars, windows and domain maps", {
  skip_if_not_installed("pixmap")
  v <- cov_plot_vol()
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_null(plot(v$img, colorbar = TRUE, title.colorbar = "cb", nslices = 2))
  expect_null(plot(v$img, v$ov,
    colorbar = TRUE, window.overlay = c(1, 10),
    title.colorbar = "ov", nslices = 2, color.overlay = "jet"
  ))
  expect_null(plot(v$img, window.img = c(10, 10), nslices = 2))
  # small-valued image is rescaled to 0-255
  expect_null(plot(v$img / 1000, nslices = 2))
  dom <- resampleImage(v$img, c(2, 2, 2), useVoxels = FALSE)
  expect_null(plot(v$img, v$ov, domainImageMap = dom, nslices = 2))
  tx <- createAntsrTransform(type = "AffineTransform", dimension = 3)
  f <- tempfile(fileext = ".mat")
  on.exit(unlink(f), add = TRUE)
  writeAntsrTransform(tx, f)
  expect_null(plot(v$img, v$ov, domainImageMap = list(dom, f), nslices = 2))
  expect_error(plot(v$img, domainImageMap = list(dom)), "length 2")
  expect_error(plot(v$img, domainImageMap = list(1, f)), "antsImage")
  expect_error(plot(v$img, slices = 50), "do not fit")
  expect_error(plot(v$img * 0 + 1, doCropping = FALSE, nslices = 2), "No variability")
  rgb <- mergeChannels(list(v$img, v$img))
  expect_error(plot(rgb), "multichannel")
  # 2D image with an overlay and newwindow (pdf device)
  r16 <- antsImageRead(getANTsRData("r16"))
  expect_null(plot(r16, list(r16 > 100), color.overlay = "green", window.overlay = c(0.5, 1)))
})
