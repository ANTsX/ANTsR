# download.file is mocked, so these tests never touch the network.

test_that("an HTML page from figshare is reported, deleted and replaced", {
  saved <- NULL
  local_mocked_bindings(.package = "ANTsR", download.file = function(url, destfile, ...) {
    saved <<- destfile
    writeLines("<!DOCTYPE html><html>Just a moment...</html>", destfile)
    0L
  })
  expect_warning(
    f <- getANTsRData("ch2", backup = TRUE),
    "figshare.*HTML page.*synthetic makeImage"
  )
  expect_false(file.exists(saved))
  img <- antsImageRead(f)
  expect_equal(dim(img), c(32, 32, 32))
  expect_error(
    getANTsRData("ch2"),
    "figshare.*HTML page"
  )
})

test_that("checksum failures delete the download and fall back", {
  saved <- NULL
  local_mocked_bindings(.package = "ANTsR", download.file = function(url, destfile, ...) {
    saved <<- destfile
    writeBin(as.raw(c(0x1f, 0x8b, 1:50)), destfile)
    0L
  })
  expect_warning(f <- getANTsRData("ch2", backup = TRUE), "checksum failure \\(md5 ")
  expect_false(file.exists(saved))
  expect_true(file.exists(f))
  expect_error(getANTsRData("ch2"), "checksum failure")
})

test_that("download errors are reported with the host", {
  local_mocked_bindings(.package = "ANTsR", download.file = function(url, destfile, ...) {
    stop("cannot open URL: HTTP status was '403 Forbidden'")
  })
  expect_warning(f <- getANTsRData("rsbold", backup = TRUE), "figshare.*403 Forbidden")
  expect_equal(dim(antsImageRead(f)), c(16, 16, 16, 20))
  # no image stand-in for tabular data
  expect_error(getANTsRData("rsboldpts", backup = TRUE), "403 Forbidden")
})

test_that("label stand-ins hold integer labels", {
  local_mocked_bindings(.package = "ANTsR", download.file = function(...) stop("offline"))
  f <- suppressWarnings(getANTsRData("ch2a", backup = TRUE))
  vals <- unique(as.numeric(as.array(antsImageRead(f))))
  expect_setequal(vals, 0:3)
})
