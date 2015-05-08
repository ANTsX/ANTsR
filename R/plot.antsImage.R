#' Plotting an image slice or multi-slice with optional color overlay.
#'
#' This is a plotting utility for antsImage types with a background and color
#' overlay option.  Useful for displaying statistical results overlaid on a
#' background image.
#'
#' @param x the reference image on which to overlay.
#' @param y image or list of images to use as overlays.
#' @param color.img color for main image.
#' @param color.overlay the color for the overlay , e.g c('blue','red') length
#' of this list should match the image list.
#' @param axis the axis to slice (1 , 2 or 3)
#' @param slices vector of slices to plot (e.g., c(10, 15, 20))
#' @param colorbar make colorbar?
#' @param title.colorbar title for colorbar
#' @param title.img title for main image
#' @param color.colorbar color scale to use for colorbar
#' @param window.img lower and upper thresholds for display of main image
#' @param window.overlay lower and upper thresholds for display of overlay
#' @param quality  integer quality magnification factor 1 => large (e.g.
#' 10)
#' @param outname name of output file if you want to write result to file, e.g.
#' \code{plot.jpg}.
#' @param alpha  opacity
#' @param newwindow  boolean controlling if we open a new device for this plot
#' @param nslices  number of slices to view
#' @param dorot  do a rotation of the slice before viewing
#' @param ...  other parameters
#' @return output is plot to standard R window
#' @author Avants BB
#' @examples
#'
#' img <- makeImage(c(4,4), rnorm(4*4))
#' mask <- makeImage(c(4,4),
#'    as.matrix(c(0,0,0,0,
#'             0,1,1,0,
#'             0,1,1,0,
#'             0,0,0,0), nrow=4))
#' plot(img, list(mask))
#' \dontrun{
#'   mnit<-getANTsRData('mni')
#'   mnit<-antsImageRead(mnit)
#'   mniafn<-getANTsRData('mnia')
#'   mnia<-antsImageRead(mniafn)
#'   mnia<-thresholdImage(mnia,22,25)
#'   mnia<-smoothImage(mnia,1.5)
#'   mnia2<-antsImageRead(mniafn)
#'   mnia2<-thresholdImage(mnia2,1,4)
#'   mnia2<-smoothImage(mnia2,1.5)
#'   ofn<-paste(tempfile(),'.png',sep='')
#'   # write directly to a file
#'   plot( mnit, list(mnia,mnia2), slices=seq(50, 140, by=5),
#'     window.overlay = c(0.25,1), axis=2,
#'     overlay.color=c('red','blue'), outname = ofn )
#' }
#'
#' @method plot antsImage
#' @export
plot.antsImage <- function(x, y,
  color.img = "white",
  color.overlay = c("jet", "red", "blue",  "green", "yellow"),
  axis = 2,
  slices,
  colorbar = missing(y),
  title.colorbar,
  title.img,
  color.colorbar,
  window.img,
  window.overlay,
  quality = 4,
  outname = NA,
  alpha = 0.5,
  newwindow = FALSE,
  nslices = 10,
  dorot = 0,
  ... ) {
  if(missing(slices)){
    plotimask<-getMask(x, cleanup=0)
    x <- cropImage(x, plotimask )
    slices <- round(seq(1, dim(x)[axis], length.out=nslices))
  }
  nonzeros <- x[x != 0]
  if(missing(window.img)){
    if (length(nonzeros) > 0 ){
      window.img <- quantile(nonzeros, c(0.05, 0.95))
    } else {
      window.img <- c(0, 0)
    }
  }
  color.colorbar <- ifelse(missing(y), "white", color.overlay[1])
  myantsimage <- x
  if (missing(y))
    y <- NA
  if (is.antsImage(y))
    y <- list(y)
  functional <- y
  imagedim <- length(dim(myantsimage))
  hvpx <- usePkg("pixmap")
  hvmsc <- usePkg("misc3d")
  hvrgl <- usePkg("rgl")
  if ( !hvrgl | !hvmsc | ! hvpx )
    {
    print(paste("You need rgl, misc3d and pixmap libraries to use this."))
    invisible(return())
    }
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
  makePalette <- function( mpcolor, nlevels=15){
    if (mpcolor == "white"){
      colorfun <- colorRampPalette(c("black", "white"), interpolate = c("spline"),
        space = "Lab")
    } else if (mpcolor != "jet"){
      colorfun <- colorRampPalette(c("white", mpcolor), interpolate = c("spline"),
        space = "Lab")
    } else {
      colorfun <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
        "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), interpolate = c("spline"),
        space = "Lab")
    }
    colorfun(nlevels)
  }

  if (all(is.na(functional))) {
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
  if(class(slices) == "character"){
    slices <- c(as.numeric(unlist(strsplit(slices, "x"))))
    slices <- round(seq(slices[1], slices[2], by=slices[3]))
  }
  if (max(slices) > dim(myantsimage)[axis]) {
    stop("Slices do not fit in image dimensions.")
  }
  if ( imagedim == 2 )
    slices <- 1
  nslices <- length(slices)
  winrows <- round(length(slices) / 4 ) # controls number of rows
  if (winrows < 1)
    winrows <- 1
  wincols <- round(nslices/winrows) # controls number of rows
  if (length(slices) < wincols )
    wincols <- length(slices)
  if (axis != 2 & imagedim > 2)
    slice <- rotate90.matrix(img[, , slices[1]])
  if (axis == 2 & imagedim > 2)
    slice <- flip.matrix(img[, , slices[1]])
  if (imagedim > 2)
    slice <- mirror.matrix(slice) else slice <- img
  slicerow <- nrow(slice)
  slicecol <- ncol(slice)
  if ( dorot == 1 )
    {
    slicerow <- ncol(slice)
    slicecol <- nrow(slice)
    }
  bigslice <- matrix(0, nrow = slicerow * winrows, ncol = (slicecol * wincols))
  rowsl <- 0
  # convert to 0 255
  nlevels <- 2^8
  for (sl in c(0:(length(slices) - 1))) {
    if (sl < dim(img)[imagedim]) {
      if (axis != 2 & imagedim > 2)
        slice <- rotate90.matrix(img[, , slices[sl + 1]])
      if (axis == 2 & imagedim > 2 & dorot==0 )
        slice <- flip.matrix(img[, , slices[sl + 1]])
      if (axis == 2 & imagedim > 2 & dorot==1 )
        slice <- rotate270.matrix(img[, , slices[sl + 1]])
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
  bbox <- c(1, 1, c(1+antsGetSpacing(myantsimage)[-axis]*dim(myantsimage)[-axis]) *
              c(wincols, winrows))
  mag <- quality
  pixperinch <- 96
  if (!is.na(outname)){
    suppressMessages(jpeg(outname, width = ncol(bigslice) * mag,
       height = nrow(bigslice) * mag, units = "px", quality = 75, bg = "white"))
  } else {
    if (newwindow) dev.new(height = nrow(bigslice)/pixperinch, width = ncol(bigslice)/pixperinch)
  }
  if ( window.img[ 1 ] == window.img[ 2 ] )
    window.img[ 1 ]<-min( x )
  bigslice[bigslice<window.img[1]] <- window.img[1]
  bigslice[bigslice>window.img[2]] <- window.img[2]
  if(colorbar){
    levels <- seq(window.img[1], window.img[2], length.out=15)
    # code taken from filled.contour
    mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
    on.exit(par(par.orig))
    w <- (3 + mar.orig[2L]) * par("csi") * 2.54
    layout(matrix(c(2, 1), ncol = 2L), widths = c(1, lcm(w)))
    par(las = 1)
    mar <- mar.orig
    mar[4L] <- mar[2L]
    mar[2L] <- 1
    par(mar = mar)
    plot.new()
    plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i",
        yaxs = "i")
    rect(0, levels[-length(levels)], 1, levels[-1L],
         col = makePalette(color.colorbar, 15))
    axis(4)
    box()
    if (!missing(title.colorbar))
        title(title.colorbar)
    mar <- mar.orig
    mar[4L] <- 1
    par(mar = mar)
  }
  img.plot <- suppressWarnings(pixmap::pixmapGrey(
    bigslice, bbox=bbox ) )
  if (!missing(title.img))
    title(title.img)
  # dd<-pixmapRGB(c(bigslice,bigslice,bigslice),nrow=nrow(bigslice),ncol=ncol(bigslice),bbox=c(0,0,wincols,winrows))
  # plot(dd)
  par(mar = c(0, 0, 0, 0) + 0)  # set margins to zero ! less wasted space
  pixmap::plot(img.plot, bg = "white")

  if (missing(window.overlay) & !all(is.na(y))) {
    window.overlay <- quantile(y[[1]][y[[1]] != 0], c(0.05, 0.95))
    if(window.overlay[1] == window.overlay[2]){
      window.overlay[1] <- min(y[[1]])
      window.overlay[2] <- max(y[[1]])
    }
  } else {
#    window.overlay <- NA
  }
  if ( ! missing( window.overlay ) )
  if ( typeof(window.overlay) == "character" ){
    window.overlay <- c(as.numeric(unlist(strsplit(window.overlay, "x"))))
  }
  if ( ! missing( window.overlay ) )
  if ( ( window.overlay[1] > window.overlay[2] ) |
         all( is.na( functional ) ) ) {
    if (!is.na(outname))
      dev.off()
    invisible(return())
  }
  if ( ! all( is.na(functional) ) )
  {
  for (ind in 1:length(functional)) {

    biglab <- matrix(0, nrow = slicerow * winrows, ncol = (slicecol * wincols))
    if ( exists("plotimask") ) # the label image
      labimg <- as.array(  cropImage(functional[[ind]], plotimask ) )
    else
      labimg <- as.array(functional[[ind]])  # the label image
    if (imagedim == 2) {
      labimg <- rotate270.matrix(labimg)
    }
    if (imagedim == 3)
      labimg <- aperm(labimg, c(perms), resize = T)
    # check sizes
#    if ( ! antsImagePhysicalSpaceConsistency( img, labimg ) )
#      stop("Image and overlay image sizes do not match.")
    mncl <- min(labimg)
    mxcl <- max(labimg)
    temp <- labimg
    temp <- (temp - mncl)/(mxcl - mncl) * (nlevels - 1)
    labimg <- temp
    locthresh <- round((window.overlay[1:2] - mncl) /
                      (mxcl - mncl) * (nlevels - 1))
    if (axis != 2 & imagedim > 2)
      labslice <- rotate90.matrix(labimg[, , slices[1]])
    if (axis == 2 & imagedim > 2 & dorot==0 )
      labslice <- flip.matrix(labimg[, , slices[1]])
    if (axis == 2 & imagedim > 2 & dorot==1 )
      labslice <- rotate270.matrix(labimg[, , slices[sl + 1]])
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
        if (axis == 2 & imagedim > 2 & dorot==0 )
          labslice <- flip.matrix(labimg[, , slices[1]])
        if (axis == 2 & imagedim > 2 & dorot==1 )
          labslice <- rotate270.matrix(labimg[, , slices[sl + 1]])
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
    if (color.overlay[ind] != "jet")
      colorfun <- colorRampPalette(c("white", color.overlay[ind]), interpolate = c("spline"),
        space = "Lab")
    if (color.overlay[ind] == "jet") {
      # print('use jet')
      colorfun <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
        "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), interpolate = c("spline"),
        space = "Lab")
    }
    heatvals <- colorfun(nlevels)
    if (locthresh[1] > 1)
      heatvals[1:(locthresh[1] - 1)] <- NA
    if (locthresh[2] < (nlevels - 1)) {
      upper <- c((locthresh[2] + 1):nlevels)
      heatvals[upper] <- NA
    }
    heatvals[1]<-NA # dont overlay the background
    if (min(biglab) != max(biglab))
      {
      invisible( suppressWarnings(
        plot(
          pixmap::pixmapIndexed(biglab,
            col = heatvals, bbox = bbox), add = TRUE) ) )
      }
  } # for loop
  } # if not all na functional
  # g<-biglab ; g[]<-0 ; b<-biglab ; b[]<-0 print('try rgb')
  # dd<-pixmapRGB(c(biglab,g,b),nrow=nrow(bigslice),ncol=ncol(bigslice),bbox=c(0,0,wincols,winrows))
  if (!is.na(outname))
    dev.off()
  invisible(return())
}
