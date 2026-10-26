rot2 <- function(theta) {
  matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), 2, 2)
}

rot3 <- function(theta) {
  d <- diag(3)
  d[1:2, 1:2] <- rot2(theta)
  d
}

make_test_image <- function(dimension) {
  if (dimension == 2) {
    img <- makeImage(c(10, 12), 0)
    antsSetSpacing(img, c(1.5, 2.5))
    antsSetOrigin(img, c(-3, 7))
    antsSetDirection(img, rot2(pi / 6))
  } else {
    img <- makeImage(c(10, 12, 8), 0)
    antsSetSpacing(img, c(1.5, 2.5, 0.75))
    antsSetOrigin(img, c(-3, 7, 2))
    antsSetDirection(img, rot3(pi / 6))
  }
  img
}

# ANTsR indices are 1-based: index 1 maps to the origin
manual_physical <- function(img, index) {
  index <- matrix(index, ncol = img@dimension)
  t(apply(index, 1, function(idx) {
    antsGetOrigin(img) +
      antsGetDirection(img) %*% (antsGetSpacing(img) * (idx - 1))
  }))
}

index_sets <- list(
  "2" = list(
    vector = c(2, 3),
    one = rbind(c(2, 3)),
    square = rbind(c(1, 1), c(2, 2)),
    tall = rbind(c(1, 1), c(2, 2), c(4, 7))
  ),
  "3" = list(
    vector = c(2, 3, 4),
    one = rbind(c(2, 3, 4)),
    square = rbind(c(1, 1, 1), c(2, 2, 2), c(3, 5, 7)),
    tall = rbind(c(1, 1, 1), c(2, 2, 2), c(3, 5, 7), c(9, 4, 2)),
    wide = rbind(c(1, 1, 1), c(5, 6, 2))
  )
)

for (dimension in c(2, 3)) {
  for (nm in names(index_sets[[as.character(dimension)]])) {
    test_that(paste0("index <-> physical point round trip, ", dimension, "D, ", nm), {
      img <- make_test_image(dimension)
      index <- index_sets[[as.character(dimension)]][[nm]]
      expected <- manual_physical(img, index)

      pt <- antsTransformIndexToPhysicalPoint(img, index)
      expect_equal(dim(pt), dim(expected))
      expect_equal(unname(pt), expected, tolerance = 1e-5)

      back <- antsTransformPhysicalPointToIndex(img, pt)
      expect_equal(dim(back), dim(expected))
      expect_equal(unname(back), matrix(index, ncol = dimension), tolerance = 1e-5)
    })
  }
}

test_that("index/point functions validate their input", {
  img <- make_test_image(2)
  expect_error(antsTransformIndexToPhysicalPoint(img, c(1, 2, 3)), "N x 2")
  expect_error(antsTransformPhysicalPointToIndex(img, rbind(c(1, 2, 3))), "N x 2")
  expect_error(antsTransformPhysicalPointToIndex(img, c("2", 2)), "point must be")
})

test_that("applyAntsrTransform* handle vectors and N x d matrices", {
  shift <- c(1, 2, 3)
  tx <- createAntsrTransform(type = "Euler3DTransform", translation = shift)

  # vector in -> vector out
  expect_equal(applyAntsrTransformToPoint(tx, c(1, 2, 3)), c(2, 4, 6))
  expect_equal(applyAntsrTransform(tx, c(1, 2, 3)), c(2, 4, 6))

  for (pts in list(
    rbind(c(1, 2, 3)),
    rbind(c(1, 2, 3), c(4, 5, 6)),
    rbind(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9)),
    rbind(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9), c(-1, 0, 1))
  )) {
    expected <- sweep(pts, 2, shift, "+")
    expect_equal(applyAntsrTransformToPoint(tx, pts), expected)
    expect_equal(applyAntsrTransform(tx, pts), expected)
    # translations do not change vectors
    expect_equal(applyAntsrTransformToVector(tx, pts), pts)
    expect_equal(applyAntsrTransform(tx, pts, dataType = "vector"), pts)
  }
})

for (nLabels in c(3, 4)) {
  test_that(paste0("labelImageCentroids(physical = TRUE) with ", nLabels, " labels"), {
    img <- make_test_image(3)
    arr <- array(0, dim = dim(img))
    arr[1:2, 1:2, 1:2] <- 1
    arr[5:6, 3:4, 2:3] <- 2
    arr[9:10, 11:12, 7:8] <- 3
    if (nLabels == 4) arr[3:4, 8:9, 5:6] <- 4
    lab <- as.antsImage(arr, reference = img)

    capture.output({
      voxel <- labelImageCentroids(lab, physical = FALSE)
      phys <- labelImageCentroids(lab, physical = TRUE)
    })

    expect_equal(dim(phys$vertices), c(nLabels, 3))
    expect_equal(
      unname(phys$vertices),
      manual_physical(img, unname(voxel$vertices)),
      tolerance = 1e-5
    )
  })
}
