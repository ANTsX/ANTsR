# Internal helpers for turning antsMotionCorr parameters (MOCOparams) into
# point mappings and framewise displacements.
#
# antsMotionCorr writes, per time point, MetricPre, MetricPost and then the
# ITK transform parameters (GetParameters()):
#   * affine: the D x D matrix in row-major order, then the translation;
#   * rigid (Euler2DTransform / Euler3DTransform): the angle(s), then the
#     translation.
# The transform center (ITK fixed parameters) is not written.  antsMotionCorr
# sets it to the intensity center of gravity of the fixed image, so it has to
# be recomputed from that image.  An ITK transform with matrix A, translation
# t and center c maps x to A (x - c) + t + c.

# Rotation matrix of an itk::Euler3DTransform (ComputeZYX = false, the
# default): R = Rz %*% Rx %*% Ry.
.euler3DMatrix <- function(angles) {
  cx <- cos(angles[1]); sx <- sin(angles[1])
  cy <- cos(angles[2]); sy <- sin(angles[2])
  cz <- cos(angles[3]); sz <- sin(angles[3])
  rx <- matrix(c(1, 0, 0, 0, cx, -sx, 0, sx, cx), 3, 3, byrow = TRUE)
  ry <- matrix(c(cy, 0, sy, 0, 1, 0, -sy, 0, cy), 3, 3, byrow = TRUE)
  rz <- matrix(c(cz, -sz, 0, sz, cz, 0, 0, 0, 1), 3, 3, byrow = TRUE)
  rz %*% rx %*% ry
}

# Matrix and offset (x' = A x + offset) of one frame's ITK transform.
# `params` are the transform parameters only (no MetricPre/MetricPost).
.mocoTransform <- function(params, center) {
  params <- as.numeric(params)
  center <- as.numeric(center)
  D <- length(center)
  np <- length(params)
  if (np == D * D + D) {
    A <- matrix(params[seq_len(D * D)], D, D, byrow = TRUE)
    trans <- params[D * D + seq_len(D)]
  } else if (D == 3 && np == 6) {
    A <- .euler3DMatrix(params[1:3])
    trans <- params[4:6]
  } else if (D == 2 && np == 3) {
    ca <- cos(params[1]); sa <- sin(params[1])
    A <- matrix(c(ca, -sa, sa, ca), 2, 2, byrow = TRUE)
    trans <- params[2:3]
  } else {
    stop(
      "Unrecognized motion parameters: ", np, " parameters for a ", D,
      "-D transform (expected ", D * D + D, " for affine or ",
      if (D == 3) 6 else 3, " for rigid)"
    )
  }
  list(matrix = A, offset = trans + center - drop(A %*% center))
}

# Physical points used to measure displacement: the voxels of `mask`
# (at most `maxPoints` of them, evenly subsampled).
.mocoSamplePoints <- function(mask, maxPoints = 5000) {
  idx <- which(as.array(mask) > 0.5, arr.ind = TRUE)
  if (length(idx) == 0) {
    stop("The mask used for framewise displacement is empty")
  }
  idx <- matrix(idx, ncol = mask@dimension)
  if (nrow(idx) > maxPoints) {
    idx <- idx[unique(round(seq(1, nrow(idx), length.out = maxPoints))), ,
      drop = FALSE
    ]
  }
  antsTransformIndexToPhysicalPoint(mask, idx)
}

# Framewise displacement between consecutive frames.
#
# mocoParams: MOCOparams matrix (columns MetricPre, MetricPost, transform
#   parameters), one row per time point.
# fixed: the fixed (reference) image used for motion correction; its center
#   of gravity is the transform center.
# mask: brain mask whose voxels are the sample points (default: getMask(fixed)).
#
# Returns a data frame with the mean and max Euclidean displacement (in
# physical units, e.g. mm) of the sample points between frame i - 1 and
# frame i; the first row is 0.  This is not the FD of Power et al. (2012),
# which sums absolute changes in the six rigid parameters.
.mocoFramewiseDisplacement <- function(mocoParams, fixed, mask = NULL,
                                       maxPoints = 5000) {
  mocoParams <- as.matrix(mocoParams)
  params <- mocoParams[, 3:ncol(mocoParams), drop = FALSE]
  fixed <- check_ants(fixed)
  if (fixed@pixeltype != "float") {
    fixed <- antsImageClone(fixed, "float")
  }
  if (is.null(mask)) {
    mask <- getMask(fixed)
  }
  center <- getCenterOfMass(fixed)
  points <- .mocoSamplePoints(check_ants(mask), maxPoints)
  .framewiseDisplacementFromParams(params, center, points)
}

# Core computation, separated so it can be tested without images.
# params: one row of transform parameters per time point.
# points: n x D matrix of physical points.
.framewiseDisplacementFromParams <- function(params, center, points) {
  params <- as.matrix(params)
  points <- as.matrix(points)
  ntimes <- nrow(params)
  mapPoints <- function(i) {
    tx <- .mocoTransform(params[i, ], center)
    sweep(points %*% t(tx$matrix), 2, tx$offset, "+")
  }
  out <- data.frame(
    MeanDisplacement = rep(0, ntimes),
    MaxDisplacement = rep(0, ntimes)
  )
  if (ntimes < 2) {
    return(out)
  }
  previous <- mapPoints(1)
  for (i in 2:ntimes) {
    current <- mapPoints(i)
    d <- sqrt(rowSums((current - previous)^2))
    out$MeanDisplacement[i] <- mean(d)
    out$MaxDisplacement[i] <- max(d)
    previous <- current
  }
  out
}
