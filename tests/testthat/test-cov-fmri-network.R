# Coverage tests for BOLD network analysis, filtering, denoising and graphs.

make_cov_bold <- function(d = c(12, 12, 8), nt = 60, tr = 2, nlab = 4,
                          seed = 1) {
  set.seed(seed)
  mask <- array(0, d)
  mask[3:(d[1] - 2), 3:(d[2] - 2), 2:(d[3] - 1)] <- 1
  labs <- array(0, d)
  half1 <- (1:d[1]) <= d[1] / 2
  half2 <- (1:d[2]) <= d[2] / 2
  for (i in 1:d[1]) for (j in 1:d[2]) {
    labs[i, j, ] <- 1 + (!half1[i]) + 2 * (!half2[j])
  }
  labs <- labs * mask
  tt <- seq_len(nt) * tr
  # one smooth signal per region in the 0.01-0.1 Hz band
  sigs <- sapply(1:nlab, function(k) {
    sin(2 * pi * (0.02 + 0.015 * k) * tt + k) + 0.3 * rnorm(nt)
  })
  sigs[, 2] <- sigs[, 1] + 0.3 * rnorm(nt) # regions 1 & 2 connected
  arr <- array(rnorm(prod(d) * nt, 0, 1), c(d, nt))
  for (k in 1:nlab) {
    idx <- which(labs == k)
    for (t in 1:nt) {
      sl <- arr[, , , t]
      sl[idx] <- sl[idx] + 100 + 5 * sigs[t, k]
      arr[, , , t] <- sl
    }
  }
  img <- as.antsImage(arr)
  antsSetSpacing(img, c(3, 3, 3, tr))
  m <- as.antsImage(mask)
  antsSetSpacing(m, c(3, 3, 3))
  l <- as.antsImage(labs)
  antsSetSpacing(l, c(3, 3, 3))
  list(img = img, mask = m, labels = l, signals = sigs)
}

make_cov_motion <- function(nt, seed = 2) {
  set.seed(seed)
  mp <- t(sapply(1:nt, function(i) {
    c(0, 0, as.numeric(diag(3)) + rnorm(9, 0, 1e-3), rnorm(3, 0, 0.05))
  }))
  mp[25, 12:14] <- mp[25, 12:14] + 3 # one big motion spike
  colnames(mp) <- c("MetricPre", "MetricPost", paste0("MOCOparam", 1:12))
  mp
}

test_that("makeGraph computes graph metrics and communities", {
  skip_if_not_installed("igraph")
  set.seed(3)
  x <- matrix(rnorm(200), 40, 5)
  x[, 2] <- x[, 1] + rnorm(40, sd = 0.3)
  x[, 4] <- x[, 3] + rnorm(40, sd = 0.3)
  cm <- cor(x)
  g <- makeGraph(cm, graphdensity = 0.5, getEfficiency = TRUE)
  expect_s3_class(g$mygraph, "igraph")
  expect_length(g$degree, 5)
  expect_length(g$pagerank, 5)
  expect_length(g$effinv, 5)
  expect_equal(dim(g$adjacencyMatrix), c(5, 5))
  expect_true(all(diag(g$adjacencyMatrix) == 0))
  g2 <- makeGraph(as.data.frame(cm), communityMethod = "walktrap")
  expect_true(inherits(g2$community, "communities"))
  for (m in c("multilevel", "leading.eigenvector", "label.propagation",
              "edge.betweenness", "fastgreedy")) {
    gm <- suppressWarnings(makeGraph(cm, communityMethod = m))
    expect_true(inherits(gm$community, "communities"))
  }
  g3 <- suppressWarnings(makeGraph(abs(cm), graphdensity = 0.6,
    inverseValuesAsWeights = TRUE))
  expect_length(g3$closeness, 5)
  expect_equal(makeGraph(matrix(numeric(0), 0, 0)), 0)
})

test_that("frequencyFilterfMRI trig/butt/stl options and argument checks", {
  set.seed(4)
  nt <- 61
  tt <- (1:nt) * 2
  mat <- cbind(sin(2 * pi * 0.05 * tt) + sin(2 * pi * 0.2 * tt), rnorm(nt))
  f1 <- frequencyFilterfMRI(mat, tr = 2, freqLo = 0.01, freqHi = 0.1, opt = "trig")
  expect_equal(dim(f1), dim(mat))
  # high-frequency component is removed
  expect_true(var(f1[, 1]) < var(mat[, 1]))
  f2 <- frequencyFilterfMRI(mat[-1, ], tr = 2, opt = "trig")
  expect_equal(nrow(f2), nt - 1)
  f3 <- frequencyFilterfMRI(mat, tr = 2, opt = "butt")
  expect_equal(dim(f3), dim(mat))
  f4 <- frequencyFilterfMRI(mat[1:60, ], tr = 2, freqLo = 0.05, freqHi = 0.2, opt = "stl")
  expect_equal(dim(f4), c(60, 2))
  expect_error(frequencyFilterfMRI(mat, tr = 2, opt = "wav"), "wmtsa")
  expect_null(frequencyFilterfMRI())
  capture.output(expect_null(frequencyFilterfMRI(mat, tr = "2")))
  capture.output(expect_null(frequencyFilterfMRI(mat, tr = 2, freqLo = "a")))
})

test_that("filterfMRIforNetworkAnalysis builds label networks", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("magic")
  b <- make_cov_bold(nt = 60)
  mat <- timeseries2matrix(b$img, b$mask)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  capture.output(expect_null(filterfMRIforNetworkAnalysis()))
  capture.output(expect_null(filterfMRIforNetworkAnalysis(mat, tr = "x")))
  capture.output(expect_null(filterfMRIforNetworkAnalysis(mat, tr = 2, freqLo = "x")))
  res <- filterfMRIforNetworkAnalysis(mat,
    tr = 2, cbfnetwork = "BOLD",
    mask = b$mask, labels = b$labels, graphdensity = 0.5, usesvd = TRUE,
    robustcorr = TRUE, useglasso = 0.1
  )
  expect_equal(dim(res$network), c(4, 60))
  expect_equal(dim(res$corrmat), c(4, 4))
  expect_equal(dim(res$partialcorrmat), c(4, 4))
  expect_equal(dim(res$rcormat), c(4, 4))
  expect_true(res$corrmat[1, 2] > 0.5) # regions 1 & 2 were made to correlate
  expect_length(res$graph$degree, 4)
  # segmentation restricts to seg == 2 voxels
  seg <- b$mask * 2
  res2 <- filterfMRIforNetworkAnalysis(mat,
    tr = 2, cbfnetwork = "ASLCBF",
    mask = b$mask, labels = b$labels, seg = seg
  )
  expect_equal(nrow(res2$network), 4)
  res3 <- filterfMRIforNetworkAnalysis(mat, tr = 2, cbfnetwork = "ASLBOLD")
  expect_equal(dim(res3$filteredTimeSeries), dim(mat))
  expect_null(res3$mask)
  expect_length(res3$temporalvar, ncol(mat))
})

test_that("antsBOLDNetworkAnalysis produces subject summary with nuisance model", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("magic")
  b <- make_cov_bold(nt = 60)
  mot <- make_cov_motion(60)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  res <- antsBOLDNetworkAnalysis(
    bold = b$img, mask = b$mask, labels = b$labels,
    motion = mot, gdens = 0.5, threshLo = 1, threshHi = 4
  )
  expect_equal(nrow(res$subjectSummary), 1)
  expect_true(all(c("MatrixMotion", "NTimePoints", "meanFD", "meanDVARS",
                    "Deg1", "PR4") %in% colnames(res$subjectSummary)))
  # default throwaway = round(10 / tr) = 5 -> 56 time points kept
  expect_equal(unname(res$subjectSummary[, "NTimePoints"]), 56)
  expect_true(all(c("dmatrix", "dtran", "FD", "DVARS") %in%
    colnames(res$nuisanceVariables)))
  expect_equal(nrow(res$nuisanceVariables), 56)
  expect_equal(dim(res$mynetwork$corrmat), c(4, 4))
  expect_error(antsBOLDNetworkAnalysis(bold = NULL))
})

test_that("rsfDenoise selects noise regressors", {
  set.seed(5)
  nt <- 48
  nv <- 60
  y <- rep(c(rep(0, 6), rep(1, 6)), 4)
  mat <- matrix(rnorm(nt * nv), nt, nv)
  mat[, 1:30] <- mat[, 1:30] + outer(y, rep(2, 30))
  mot <- matrix(rnorm(nt * 2), nt, 2)
  capture.output(r <- rsfDenoise(mat, y,
    motionparams = mot, selectionthresh = 0.3,
    maxnoisepreds = 1:3, polydegree = 2, debug = TRUE, scalemat = TRUE
  ))
  expect_true(r$n %in% 1:3)
  expect_length(r$noisepool, nv)
  expect_equal(nrow(r$noiseu), nt)
  expect_equal(dim(r$R2final), c(nv, 3))
  capture.output(r2 <- rsfDenoise(mat, y,
    selectionthresh = 0.3, maxnoisepreds = 2,
    crossvalidationgroups = rep(1:4, each = 12)
  ))
  expect_equal(r2$n, 2)
  capture.output(r3 <- rsfDenoise(mat, y, selectionthresh = 1e-6))
  expect_true(is.na(r3))
})

test_that("clusterTimeSeries clusters columns of a matrix", {
  skip_if_not_installed("fpc")
  set.seed(6)
  mat <- rbind(
    matrix(rnorm(15 * 5, 0), 15, 5),
    matrix(rnorm(15 * 5, 5), 15, 5)
  )
  cl <- clusterTimeSeries(mat, krange = 2:3)
  expect_length(cl$clusters, 30)
  expect_equal(length(unique(cl$clusters)), 2)
  expect_length(cl$kimgs, 2)
  capture.output(expect_equal(clusterTimeSeries(), 1))
})

test_that("plotBasicNetwork draws nodes and edges with a null rgl device", {
  skip_if_not_installed("rgl")
  old <- options(rgl.useNULL = TRUE)
  on.exit(options(old), add = TRUE)
  # plotBasicNetwork calls rgl::rgl.bg, which newer rgl makes defunct
  skip_on_known_error(rgl::rgl.bg(color = "white"), "defunct",
    "plotBasicNetwork uses rgl.bg, defunct in this rgl version")
  set.seed(7)
  # a tiny "brain" surface: 4 triangles given as v1, v2, v3 vertex matrices
  v <- lapply(1:3, function(i) matrix(rnorm(12), 4, 3))
  brain <- list(v)
  cents <- matrix(rnorm(15, sd = 5), 5, 3)
  w <- matrix(runif(25), 5, 5)
  w <- (w + t(w)) / 2
  diag(w) <- 0
  capture.output(expect_equal(plotBasicNetwork(cents), 1))
  suppressWarnings({
    expect_equal(plotBasicNetwork(cents, brain), 1)
    expect_equal(plotBasicNetwork(cents, brain, radius = 1, nodecolors = "red"), 1)
  })
  # weighted networks: the weights check uses `&&` on a length-2 vector,
  # which is an error in R >= 4.3 (package bug)
  expect_error(plotBasicNetwork(cents, brain, weights = w), "length = 2")
  rgl::close3d()
})

test_that("plotPrettyGraph writes a png", {
  skip_if_not_installed("sna")
  skip_if_not_installed("igraph")
  set.seed(8)
  adj <- matrix(rbinom(36, 1, 0.5), 6, 6)
  adj <- adj * t(adj)
  rownames(adj) <- colnames(adj) <- letters[1:6]
  fn <- tempfile(fileext = ".png")
  on.exit(unlink(fn), add = TRUE)
  out <- plotPrettyGraph(adj, runif(6), pngfn = fn, figScale = 8)
  expect_equal(out, fn)
  expect_gt(file.size(fn), 0)
})
