#' Define an anatomical coordinate system in a new image based on a template
#'
#' This function will provide a mapping that labels an input image and its
#' blobs.
#'
#' Uses Matthew Brett's mni2tal to get the final Talairach coordinates from MNI
#' space.
#'
#' This is a standard approach but it's not very accurate.
#'
#' @param imagePairToBeLabeled e.g. the template image and the activation map
#' @param templatePairWithLabels e..g the mni image and brodmann label set
#' @param labelnames a list of names for the labels
#' @param outprefix if output to a file, provide file prefix
#' @param convertToTal bool, return talairach coordinates
#' @return The output point is in approximate template space.
#' @author Avants, BB
#' @keywords Talairach, Template, Coordinates
#' @examples
#' \dontrun{
#' #
#' # ch2bet is available in chris rorden's mricron
#' #  but you can do something with any other image
#' #  e.g. a statistical image
#' #
#' tem <- antsImageRead(getANTsRData("ch2"), 3)
#' clust <- antsImageClone(tem)
#' atem <- as.array(tem)
#' clust[atem < 80] <- 0
#' clust[atem > 90] <- 0
#' clust[atem > 80 & atem < 90] <- 1
#' clust <- iMath(clust, "ME", 1) # erosion
#' clust <- labelClusters(clust, minClusterSize = 30, minThresh = 1, maxThresh = 1)
#' if (!exists("mymni")) {
#'   # try getANTsRData if you have www access
#'   mymni <- list(
#'     antsImageRead(getANTsRData("mni"), 3),
#'     antsImageRead(getANTsRData("mnib"), 3),
#'     antsImageRead(getANTsRData("mnia"), 3)
#'   )
#' }
#' template_cluster_pair <- list(tem, clust)
#' gcoords <- getTemplateCoordinates(template_cluster_pair,
#'   mymni,
#'   convertToTal = TRUE
#' )
#' # output will be like
#' # > gcoords$templatepoints
#' #     x   y   z t label Brodmann                 AAL
#' # 1  -12  13  -3 0     1        0           Caudate_R
#' # 2   13  16   5 0     2        0           Caudate_L
#' #
#' # can also use a white matter label set ...
#' #
#' }
#'
#' @export getTemplateCoordinates
getTemplateCoordinates <- function(
    imagePairToBeLabeled,
    templatePairWithLabels,
    labelnames = NULL,
    outprefix = NA,
    convertToTal = FALSE) {
  if (nargs() == 0 | length(imagePairToBeLabeled) < 2 | length(templatePairWithLabels) <
    2) {
    print(args(getTemplateCoordinates))
    print(" imagePairToBeLabeled <-list( myBrain, myBrainBlobs ) ")
    print(" templatePairWithLabels <-list( ch2orMNI_Brain, ch2orMNI_BrodmannLabels ) ")
    print(" labelnames <-c(\"HippocampusL\",\"HippocampusR\") ")
    return(1)
  }


  getValueAtPoint <- function(x, point) {
    if (!inherits(x, "antsImage")) {
      stop("Input must be of class 'antsImage'")
    }
    if (!is.numeric(point)) {
      stop("point must be of class 'numeric'")
    }

    idx <- as.numeric(antsTransformPhysicalPointToIndex(x, point))
    idx <- floor(idx)

    dims <- length(idx)

    value <- NA
    if (dims == 2) {
      value <- getPixels(x, i = idx[1], j = idx[2])
    } else if (dims == 3) {
      value <- getPixels(x, i = idx[1], j = idx[2], k = idx[3])
    } else if (dims == 4) {
      value <- getPixels(x, i = idx[1], j = idx[2], k = idx[3], l = idx[4])
    }

    return(value[[1]])
  }


  fi <- templatePairWithLabels[[1]]
  mi <- imagePairToBeLabeled[[1]]
  if (!inherits(fi, "antsImage")) {
    print("  fi not an antsImage ")
  }
  if (!inherits(mi, "antsImage")) {
    print("  mi not an antsImage ")
  }
  imagedim <- mi@dimension
  if (is.na(outprefix)) {
    outprefix <- paste(tempdir(), "/Z", sep = "")
  }
  txfn <- paste(outprefix, "0GenericAffine.mat", sep = "")
  if (!file.exists(txfn)) {
    mytx <- antsRegistration(
      fixed = fi, moving = mi,
      typeofTransform = c("Affine"),
      outprefix = outprefix
    )
  } else {
    mytx <- list(fwdtransforms = txfn)
  }
  mywarpedimage <- antsApplyTransforms(
    fixed = fi, moving = mi,
    transformlist = mytx$fwdtransforms,
    interpolator = c("Linear")
  )
  milab <- imagePairToBeLabeled[[2]]
  mywarpedLimage <- antsApplyTransforms(
    fixed = fi, moving = milab, transformlist = mytx$fwdtransforms,
    interpolator = c("NearestNeighbor")
  )
  mypoints <- data.frame(getCentroids(mywarpedLimage))
  for (mylab in 2:length(templatePairWithLabels)) {
    filab <- templatePairWithLabels[[mylab]]
    if (!inherits(filab, "antsImage")) {
      print("  filab != antsImage ")
      return(1)
    }
    if (!inherits(milab, "antsImage")) {
      print("  milab != antsImage ")
      return(1)
    }
    # now we know the (e.g. MNI) coordinate of each labeled region in the original
    # image we want, next, to identify the 'Brodmann' label for each of these regions
    # for instance, you might have found 2 blobs, blob1 and blob2 with labels 1 and 2
    # you know want to know if these are at Brodmann area 21 or 22 or whatever so we
    # iterate through the point list and index the filab image ( template labels )
    templateLab <- rep(NA, nrow(mypoints))
    for (i in 1:nrow(mypoints)) {
      if (imagedim == 2) {
        mypoint <- as.numeric(c(mypoints$x[i], mypoints$y[i]))
      }
      if (imagedim == 3) {
        mypoint <- as.numeric(c(mypoints$x[i], mypoints$y[i], mypoints$z[i]))
      }
      templateLab[i] <- getValueAtPoint(filab, mypoint)
    }
    if (mylab == 2) {
      mypoints <- cbind(mypoints, Brodmann = templateLab)
    }
    if (mylab == 3 & max(filab[filab > 0]) == 11) {
      mypoints <- cbind(mypoints, Tracts = templateLab)
    }
    if (mylab == 3 & max(filab[filab > 0]) != 11) {
      mypoints <- cbind(mypoints, AAL = templateLab)
    }
    if (mylab > 3) {
      mypoints <- cbind(mypoints, templateLab = templateLab)
    }
  }
  if (convertToTal & imagedim == 3) {
    for (i in 1:nrow(mypoints)) {
      talpt <- mni2tal(c(mypoints$x[i], mypoints$y[i], mypoints$z[i]))
      mypoints$x[i] <- talpt[1]
      mypoints$y[i] <- talpt[2]
      mypoints$z[i] <- talpt[3]
    }
  }

  if (!convertToTal & imagedim == 3) {
    # assume MNI
    for (i in 1:nrow(mypoints)) {
      mypoints$x[i] <- mypoints$x[i] * (-1) #
      mypoints$y[i] <- mypoints$y[i] * (-1) # due to ITK coordinates being LPS whereas MNI are RAS
      mypoints$z[i] <- mypoints$z[i]
    }
  }
  scl <- 1
  mypoints$x <- round(mypoints$x * scl) / scl
  mypoints$y <- round(mypoints$y * scl) / scl
  mypoints$z <- round(mypoints$z * scl) / scl
  if (max(filab[filab > 0]) == 11) {
    tracts <- NULL
    data("tracts", package = "ANTsR", envir = environment())
    tractnames <- rep("", nrow(mypoints))
    for (i in 1:nrow(mypoints)) {
      tractnum <- as.character(mypoints$Tracts[i])
      tractname <- as.character(tracts$label_name[as.numeric(tractnum)])
      if (length(tractname) > 0) {
        tractnames[i] <- tractname
      }
    }
    mypoints$Tracts <- tractnames
  }
  if (max(filab[filab > 0]) != 11) {
    aal <- NULL
    data("aal", package = "ANTsR", envir = environment())
    aalnames <- rep("", nrow(mypoints))
    for (i in 1:nrow(mypoints)) {
      aalnum <- as.character(mypoints$AAL[i])
      aalname <- as.character(aal$label_name[as.numeric(aalnum)])
      if (length(aalname) > 0) {
        aalnames[i] <- aalname
      }
    }
    mypoints$AAL <- aalnames
  }
  return(list(
    templatepoints = mypoints,
    myLabelsInTemplateSpace = mywarpedLimage,
    myImageInTemplateSpace = mywarpedimage
  ))
}
