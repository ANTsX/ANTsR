#' plotANTsImage
#'
#' Plotting an image slice or multi-slice with optional color overlay.
#'
#' This is a plotting utility for antsImage types with a background and color
#' overlay option.  Useful for displaying statistical results overlaid on a
#' background image.
#'
#'
#' @param x the reference image on which to overlay
#' @param y list of the images to use as overlays
#' @param color=<string> the color for the overlay , e.g c('blue','red') length
#' of this list should match the image list
#' @param axis=<value> character ... the axis to slice (1 , 2 or 3)
#' @param slices=<string> character, the slices to overlay written as 10x20x3
#' where 10x20 is the range and 3 is the increment, for multislice display
#' @param threshold=<string> character, we overlay values above/below this
#' threshold : of form LOxHI
#' @param quality=<value> integer quality magnification factor 1 => large (e.g.
#' 10)
#' @param outname='figx.jpg' output name if you want to write the result to a
#' file
#' @return output is plot to standard R window
#' @author Avants BB
#' @examples
#'
#'   mnit<-getANTsRData('mni')
#'   mnit<-antsImageRead(mnit,3)
#'   mniafn<-getANTsRData('mnia')
#'   mnia<-antsImageRead(mniafn,3)
#'   ThresholdImage(3,mnia,mnia,22,25)
#'   SmoothImage(3,mnia,1.5,mnia)
#'   mnia2<-antsImageRead(mniafn,3)
#'   ThresholdImage(3,mnia2,mnia2,1,4)
#'   SmoothImage(3,mnia2,1.5,mnia2)
#'   plot( mnit, list(mnia,mnia2), slices='50x140x5',
#'    threshold = '0.25x1', axis=0,color=c('red','blue') )
#'   ofn<-paste(tempfile(),'.png',sep='')
#'   # write directly to a file
#'   plot( mnit, list(mnia,mnia2), slices='50x140x5',
#'    threshold = '0.25x1', axis=0,color=c('red','blue'), outname = ofn )
#'
#' @export plotANTsImage
plotANTsImage <- function(x, y, color = c("jet", "red", "blue",
  "green", "yellow"), axis = 1, slices = "1x1x1", threshold = "0.5xInf", quality = NA,
  outname = NA, alpha = 0.5, ... ) {
  spec <- c("verbose", "v", 2, "integer", " verbose output ", "help", "h", 0, "logical",
    " print the help ", "myantsimage", "b", 2, "character", " the reference image on which to overlay ",
    "color", "c", 1, "character", " the color for the overlay ", "functional",
    "f", 1, "character", " the image to use as overlay ", "axis", "a", 1, "character",
    " the axis to slice (1 , 2 or  3)  ", "slices", "s", 1, "character", " the slices to overlay written as 10x20x3 where 10x20 is the range and 3 is the increment etc. ",
    "threshold", "t", 1, "character", " we overlay values above/below this threshold : of form LOxHI  ",
    "quality", "q", 1, "integer", " integer quality magnification factor 1 => large (e.g. 10) ",
    "output", "o", 1, "character", " the output prefix ")
  # ............................................. #
  spec <- matrix(spec, ncol = 5, byrow = TRUE)
  if (missing(x)) {
    # print a friendly message and exit with a non-zero error code
    cat("\n")
    self <- "plotANTsImage"
    cat(paste(self, "\n"))
    for (x in 1:nrow(spec)) {
      cat("\n")
      longopt <- paste("--", spec[x, 1], sep = "")
      shortopt <- paste("-", spec[x, 2], sep = "")
      hlist <- paste(shortopt, "|", longopt, spec[x, 5], "\n \n")
      # print(hlist,quote=F)
      cat(format(hlist, width = 40, justify = c("left")))
    }
    cat(format("Example: in 2D \n", width = 40, justify = c("left")))
    ex <- paste(" plotANTsImage(myantsimage=mask,functional=mask,threshold=\"50x150\",color=\"red\",axis=1)\n \n ")
    cat(format("Example: in 3D \n", width = 40, justify = c("left")))
    ex <- paste(" plotANTsImage(myantsimage=img,functional=img,threshold=\"50x150\",slices=\"10x20x3\",color=\"red\",axis=0)\n \n ")
    ex <- format(ex, width = length(ex), justify = c("left"))
    cat("\n")
    cat(ex)
    return(NULL)
  }
  # get the options
  myantsimage<-x
  if ( missing( y ) ) y<-NA
  functional<-y
  imagedim <- length(dim(myantsimage))
  usePkg("pixmap")
  usePkg("misc3d")
  usePkg("rgl")
  usePkg("utils")
  read.img <- function(x, dim = 2) {
    img <- antsImageRead(x, dim)
    img <- as.array(img)
  }
  # define a bunch of functions for rotating matrices Flip matrix (upside-down)
  flip.matrix <- function(x) {
    mirror.matrix(rotate180.matrix(x))
  }

  # Mirror matrix (left-right)
  mirror.matrix <- function(x) {
    xx <- as.data.frame(x)
    xx <- rev(xx)
    xx <- as.matrix(xx)
    xx
  }

  # Rotate matrix 90 clockworks
  rotate90.matrix <- function(x) {
    t(mirror.matrix(x))
  }

  # Rotate matrix 180 clockworks
  rotate180.matrix <- function(x) {
    xx <- rev(x)
    dim(xx) <- dim(x)
    xx
  }

  # Rotate matrix 270 clockworks
  rotate270.matrix <- function(x) {
    mirror.matrix(t(x))
  }

  ############################################################################ Color methods The inverse function to col2rgb()
  rgb2col <- function(rgb) {
    hexDigit <- c(0:9, "A", "B", "C", "D", "E", "F")
    rgb <- rgb%%256
    hi <- rgb%/%16
    # lo <- rgb %% 16
    lo <- rgb - 16 * hi  # Faster?
    x <- t(matrix(c(hi, lo), ncol = 2)) + 1
    s <- matrix(hexDigit[x], nrow = 6)
    s <- apply(s, MARGIN = 2, FUN = paste, collapse = "")
    paste("#", s, sep = "")
  }

  ############################################################################ Draw methods
  image180 <- function(z, ...) {
    image(rotate180.matrix(z), ...)
  }

  image270 <- function(z, ...) {
    image(rotate270.matrix(z), ...)
  }

  if (is.na(quality)) {
    quality <- 4
  }
  if (is.na(slices) & imagedim > 2) {
    print(paste(" No slices set."))
    return(NULL)
  }
  if (is.na(myantsimage)) {
    print(paste("image", myantsimage, "does not exist. Exiting."))
    return(NULL)
  }
  if (is.na(functional)) {
    # print(paste('functional image file', functional, 'does not exist. no overlay
    # will be produced.'))
    thresh <- "1.e9x1.e9"
  }
  # .................................................
  img <- as.array(myantsimage)  # the background/template image
  if (imagedim == 2) {
    img <- rotate270.matrix(img)
  }
  perms <- c(1, 2, 3)
  axis <- as.numeric(axis)
  if (axis == 1) {
    perms <- c(2, 3, 1)
  }
  if (axis == 2) {
    perms <- c(3, 1, 2)
  }
  if (axis == 3) {
    perms <- c(1, 2, 3)
  }
  # now label the results
  if (imagedim == 3)
    img <- aperm(img, c(perms), resize = T)
  slicesin <- c(as.numeric(unlist(strsplit(slices, "x"))))
  threshold <- c(as.numeric(unlist(strsplit(threshold, "x"))))
  # print(paste('threshold at ', threshold[1], ' and ', threshold[2], 'you chose
  # these slices :')) print(slicesin)
  if (slicesin[1] > dim(img)[imagedim] | slicesin[2] > dim(img)[imagedim]) {
    print("slices do not fit in image dimensions ---exiting")
    print(paste("slices1 ", slicesin[1], "slices2", slicesin[2], "dim-to-slice",
      dim(img)[imagedim]))
    return(NULL)
  }
  nslices <- round((slicesin[2] - slicesin[1])/slicesin[imagedim]) + 1
  slices <- rep(NA, nslices)
  ct <- 1
  curslice <- slicesin[1]
  while (ct < nslices) {
    slices[ct] <- curslice
    curslice <- curslice + slicesin[imagedim]
    ct <- ct + 1
  }
  if (is.na(slices[nslices]))
    slices[nslices] <- slicesin[2]
  winrows <- round(length(slices)/10 + 0.5)
  if (winrows < 1)
    winrows <- 1
  wincols <- 10
  if (length(slices) < 10)
    wincols <- length(slices)
  if (axis != 2 & imagedim > 2)
    slice <- rotate90.matrix(img[, , slices[1]])
  if (axis == 2 & imagedim > 2)
    slice <- flip.matrix(img[, , slices[1]])
  if (imagedim > 2)
    slice <- mirror.matrix(slice) else slice <- img
  slicerow <- nrow(slice)
  slicecol <- ncol(slice)
  bigslice <- matrix(0, nrow = slicerow * winrows, ncol = (slicecol * wincols))
  rowsl <- 0
  # convert to 0 255
  nlevels <- 2^8
  for (sl in c(0:(length(slices) - 1))) {
    if (sl < dim(img)[imagedim]) {
      if (axis != 2 & imagedim > 2)
        slice <- rotate90.matrix(img[, , slices[sl + 1]])
      if (axis == 2 & imagedim > 2)
        slice <- flip.matrix(img[, , slices[sl + 1]])
      if (imagedim > 2) {
        slice <- mirror.matrix(slice)
      } else {
        slice <- img
      }
      locsl <- (sl%%(wincols)) + 1
      if (locsl == 1)
        rowsl <- rowsl + 1
      xl <- ((locsl - 1) * slicecol + 1)
      xs <- c(xl:(xl + slicecol - 1))
      yl <- (rowsl - 1) * slicerow + 1
      ys <- c(yl:(yl + slicerow - 1))
      bigslice[ys, xs] <- slice
    }
  }
  # pdf(paste(output,'.pdf',sep='')) onm<-paste(output,'.jpg',sep='')
  mag <- quality
  pixperinch <- 96
  if (!is.na(outname))
    suppressMessages(jpeg(outname, width = ncol(bigslice) * mag, height = nrow(bigslice) *
      mag, units = "px", quality = 75, bg = "white")) else dev.new(height = nrow(bigslice)/pixperinch, width = ncol(bigslice)/pixperinch)
  x <- pixmapGrey(bigslice, nrow = nrow(bigslice), ncol = ncol(bigslice))
  # dd<-pixmapRGB(c(bigslice,bigslice,bigslice),nrow=nrow(bigslice),ncol=ncol(bigslice),bbox=c(0,0,wincols,winrows))
  # plot(dd)
  par(mar = c(0, 0, 0, 0) + 0)  # set margins to zero ! less wasted space
  plot(x, bg = "white")
  if (threshold[1] > threshold[2] | is.na(functional)) {
    if (!is.na(outname))
      dev.off()
    return(NULL)
  }
  for (ind in 1:length(functional)) {
    biglab <- matrix(0, nrow = slicerow * winrows, ncol = (slicecol * wincols))
    labimg <- as.array(functional[[ind]])  # the label image
    if (imagedim == 2) {
      labimg <- rotate270.matrix(labimg)
    }
    if (imagedim == 3)
      labimg <- aperm(labimg, c(perms), resize = T)
    # check sizes
    for (x in c(1:imagedim)) if (dim(img)[x] != dim(labimg)[x]) {
      print(paste("img", dim(img)))
      print(paste("lab", dim(labimg)))
      print("mask and label image do not match---exiting")
      return(NULL)
    }
    mncl <- min(labimg)
    mxcl <- max(labimg)
    # print('threshold') print(threshold) print(paste('min/max of image', mncl,
    # mxcl))
    temp <- labimg
    temp <- (temp - mncl)/(mxcl - mncl) * (nlevels - 1)
    labimg <- temp
    locthresh <- round((threshold[1:2] - mncl)/(mxcl - mncl) * (nlevels - 1))
    if (axis != 2 & imagedim > 2)
      labslice <- rotate90.matrix(labimg[, , slices[1]])
    if (axis == 2 & imagedim > 2)
      labslice <- flip.matrix(labimg[, , slices[1]])
    if (imagedim > 2)
      labslice <- mirror.matrix(labslice) else slice <- img
    slicerow <- nrow(slice)
    slicecol <- ncol(slice)
    bigslice <- matrix(0, nrow = slicerow * winrows, ncol = (slicecol * wincols))
    rowsl <- 0
    for (sl in c(0:(length(slices) - 1))) {
      if (sl < dim(img)[imagedim]) {
        if (axis != 2 & imagedim > 2)
          labslice <- rotate90.matrix(labimg[, , slices[sl + 1]])
        if (axis == 2 & imagedim > 2)
          labslice <- flip.matrix(labimg[, , slices[sl + 1]])
        if (imagedim > 2) {
          labslice <- mirror.matrix(labslice)
        } else {
          labslice <- labimg
        }
        locsl <- (sl%%(wincols)) + 1
        if (locsl == 1)
          rowsl <- rowsl + 1
        xl <- ((locsl - 1) * slicecol + 1)
        xs <- c(xl:(xl + slicecol - 1))
        yl <- (rowsl - 1) * slicerow + 1
        ys <- c(yl:(yl + slicerow - 1))
        biglab[ys, xs] <- labslice
      }
    }
    overlaycolors <- sort(c((unique(c(labimg)))))
    overlaycolors <- c(0:nlevels)
    minind <- 0
    mindiff <- 1e+09
    maxind <- 0
    maxdiff <- 1e+09
    for (i in c(1:length(overlaycolors))) {
      diff <- abs(overlaycolors[i] - locthresh[1])
      if (diff < mindiff) {
        minind <- i
        mindiff <- diff
      }
      diff <- abs(overlaycolors[i] - locthresh[2])
      if (diff < maxdiff) {
        maxind <- i
        maxdiff <- diff
      }
    }
    if (minind > 1)
      minind <- minind - 1
    heatvals <- heat.colors(nlevels, alpha = alpha)
    heatvals <- rainbow(nlevels, alpha = alpha)
    if (color[ind] != "jet")
      colorfun <- colorRampPalette(c("white", color[ind]), interpolate = c("spline"),
        space = "Lab")
    if (color[ind] == "jet") {
      # print('use jet')
      colorfun <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
        "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), interpolate = c("spline"),
        space = "Lab")
    }
    heatvals <- colorfun(nlevels)
    # print(heatvals)
    if (locthresh[1] > 1)
      heatvals[1:(locthresh[1] - 1)] <- NA
    if (locthresh[2] < (nlevels - 1)) {
      upper <- c((locthresh[2] + 1):nlevels)
      heatvals[upper] <- NA
    }
    # heatvals[1:(length(heatvals)-50 ) ]<-NA
    if (min(biglab) != max(biglab))
      plot(pixmapIndexed(biglab, col = heatvals), add = TRUE)
  }
  # g<-biglab ; g[]<-0 ; b<-biglab ; b[]<-0 print('try rgb')
  # dd<-pixmapRGB(c(biglab,g,b),nrow=nrow(bigslice),ncol=ncol(bigslice),bbox=c(0,0,wincols,winrows))
  if (!is.na(outname))
    dev.off()
  return(NULL)
}
plot.antsImage <- plotANTsImage
