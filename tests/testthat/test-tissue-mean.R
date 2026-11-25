test_that(".tissueMean handles zero, one, and many voxels", {
  m <- matrix(1:12, nrow = 3)
  expect_equal(ANTsR:::.tissueMean(m, 1:2, "wm"), rowMeans(m[, 1:2]))
  expect_equal(ANTsR:::.tissueMean(m, 3, "wm"), m[, 3])
  expect_warning(out <- ANTsR:::.tissueMean(m, integer(0), "wm"), "No wm voxels")
  expect_equal(out, rep(NA_real_, 3))
})
