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
#' @param title.img title for main image (not displayed when file is saved with outname)
#' @param title.line title vertical displacement (irrelevant when a file is saved with outname)
#' @param color.colorbar color scale to use for colorbar
#' @param window.img lower and upper thresholds for display of main image
#' @param window.overlay lower and upper thresholds for display of overlay.
#' if you set \code{window.overlay = c( 0, 100 )} and \code{useAbsoluteScale=T}
#' then the image overlay will map into that range.  otherswise, the min and
#' max of the image will be used.
#' @param quality  integer quality magnification factor 1 => large (e.g.
#' 10)
#' @param outname name of output file if you want to write result to file, e.g.
#' \code{plot.jpg}.
#' @param alpha  opacity
#' @param newwindow  boolean controlling if we open a new device for this plot
#' @param nslices  number of slices to view
#' @param domainImageMap this input antsImage or list contains a reference image
#' (\code{domainImage}) and optional reference mapping named \code{domainMap}.
#' these will be used to map the input image(s) to this antsImage space before
#' plotting. this is useful for non-standard image orientations.
#' @param ncolumns number of columns in plot
#' @param useAbsoluteScale boolean determines whether dynamic range is maximized
#' when visualizing overlays
#' @param doCropping  apply cropping, defaults to \code{TRUE}
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
#'     color.overlay=c('red','blue'), outname = ofn )
#' }
#'
#' @method plot antsImage
#' @export
plot.antsImage <- function(x, y,
  color.img = "white",
  color.overlay = c("jet", "red", "blue",  "green", "yellow", "viridis", "magma", "plasma", "inferno"),
  axis = 2,
  slices,
  colorbar = missing(y),
  title.colorbar,
  title.img,
  title.line=NA,
  color.colorbar,
  window.img,
  window.overlay,
  quality = 2,
  outname = NA,
  alpha = 1.0,
  newwindow = FALSE,
  nslices = 10,
  domainImageMap = NA,
  ncolumns = 4,
  useAbsoluteScale = FALSE,
  doCropping = TRUE,
  ... ) {
if ( x@components > 1 )
  {
  stop("Cannot plot multichannel image: splitChannels, then plot as overlays.")
  }
if ( ! is.antsImage( x ) ) stop("input x should be an antsImage.")
interpNN = 'NearestNeighbor'
interpStyle = 'Linear'
if ( ! missing( "y" ) )
  {
  if ( is.antsImage( y ) ) y <- list(y)
  for ( i in 1:length( y ) ) {
    if ( antsImagePhysicalSpaceConsistency( x, y[[i]] ) == FALSE ) {
      y[[ i ]] = resampleImageToTarget( y[[i]], x, interpType = interpStyle )
      }
    }
  }
if ( ! any( is.na( domainImageMap ) ) )
  {
  if ( is.antsImage( domainImageMap ) )
    {
    tx = new("antsrTransform", precision="float",
      type="AffineTransform", dimension = x@dimension )
    x = applyAntsrTransformToImage(tx, x, domainImageMap )
    if ( ! missing( "y" ) )
      {
      if ( is.antsImage( y ) ) y <- list(y)
      for ( i in 1:length( y ) )
        y[[ i ]] =
          applyAntsrTransformToImage(tx, y[[ i ]], domainImageMap,
            interpolation = interpStyle )
      }
    }
  if ( is.list( domainImageMap ) ) # expect an image and transformation
    {
    if ( length( domainImageMap ) != 2 )  stop("domainImageMap list Should be length 2.")
    dimg = domainImageMap[[1]]
    if ( ! is.antsImage( dimg ) )
      stop("domainImageMap list first entry list should be antsImage.")
    tx = domainImageMap[[2]]
    x = antsApplyTransforms( dimg, x, transformlist = tx )
    if ( ! missing( "y" ) )
      {
      if ( is.antsImage( y ) ) y <- list(y)
      for ( i in 1:length( y ) )
        y[[ i ]] = antsApplyTransforms( dimg, y[[ i ]], transformlist = tx,
          interpolator = interpStyle )
      }
    }
  }

  if ( length( dim( x ) ) < axis ) axis = length( dim( x ) )
  if(missing(slices)){
    plotimask<-getMask(x, cleanup=0)
    if ( max( plotimask ) == 0 ) plotimask = plotimask + 1
    if ( doCropping ) x <- cropImage(x, plotimask )
    slices <- round(seq(1, dim(x)[axis], length.out=nslices))
  }
  startpar <- par(c("mar", "las", "mfrow"))$mar

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
  imagedim <- length(dim(myantsimage))
  hvpx <- usePkg("pixmap")
  hvmsc <- usePkg("misc3d")
  hvrgl <- usePkg("rgl")
  if ( !hvmsc | ! hvpx )
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
    if ( mpcolor == "viridis") return( viridis::viridis( nlevels ) )
    if ( mpcolor == "magma") return( viridis::magma( nlevels ) )
    if ( mpcolor == "plasma") return( viridis::plasma( nlevels ) )
    if ( mpcolor == "inferno") return( viridis::inferno( nlevels ) )
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

  if (all(is.na(y))) {
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
  winrows <- ceiling(length(slices) / ncolumns ) # controls number of rows
  if (winrows < 1)
    winrows <- 1
  wincols <- ncolumns # controls number of columns
  if (length(slices) < wincols )
    wincols <- length(slices)
  reoSlice <- function( inimg )
    {
    if (axis != 2 & imagedim > 2)
      slice <- rotate90.matrix(inimg[, , slices[1]])
    if (axis == 2 & imagedim > 2)
      slice <- flip.matrix(inimg[, , slices[1]])
    if (imagedim > 2)
      slice <- mirror.matrix(slice) else slice <- img
    return( slice )
    }
  slice = reoSlice( img )
  slicerow <- nrow(slice)
  slicecol <- ncol(slice)
  bigslice <- matrix(0, nrow = slicerow * winrows, ncol = (slicecol * wincols))
  rowsl <- 0
  # convert to 0 255
  nlevels <- 2^8
  reoSlice2 <- function( inimg, insl )
    {
      if (axis != 2 & imagedim > 2)
        slice <- rotate90.matrix(inimg[, , slices[insl + 1]])
      if (axis == 2 & imagedim > 2 )
        slice <- flip.matrix(inimg[, , slices[insl + 1]])
      if (imagedim > 2) {
        slice <- mirror.matrix(slice)
      } else {
        slice <- inimg
      }
      return( slice )
    }
  for (sl in c(0:(length(slices) - 1))) {
    if (sl < dim(img)[imagedim]) {
      slice = reoSlice2( img, sl )
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
  if ( colorbar & missing( y ) ){
    nlev = 50
    levels <- seq(window.img[1], window.img[2], length.out=nlev)
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
         col = makePalette(color.colorbar, nlev))
    axis(4)
    box()
    if (!missing(title.colorbar))
        title(title.colorbar)
    mar <- mar.orig
    mar[4L] <- 1
    par(mar = mar)
  }
  if ( ! missing( window.overlay ) ) {
    eps = 1.e-8
    window.overlay[1] = window.overlay[1] - eps
    window.overlay[2] = window.overlay[2] + eps
    }
  if(colorbar & !missing(y)){
    nlev = 50
    levels <- seq(window.overlay[1], window.overlay[2], length.out=nlev )
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
         col = makePalette(color.colorbar, nlev))
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
  # dd<-pixmapRGB(c(bigslice,bigslice,bigslice),nrow=nrow(bigslice),ncol=ncol(bigslice),bbox=c(0,0,wincols,winrows))
  # plot(dd)
  par(mar = c(0, 0, 0, 0) + 0)  # set margins to zero ! less wasted space
  pixmap::plot(img.plot, bg = "white")

  if (!missing(title.img))
    title(title.img, line=title.line)

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
         all( is.na( y ) ) ) {
    if (!is.na(outname))
      dev.off()
    invisible(return())
  }
  if ( ! all( is.na( y ) ) )
  {
  for (ind in 1:length( y )) {

    biglab <- matrix(0, nrow = slicerow * winrows, ncol = (slicecol * wincols))
    if ( exists("plotimask") ) { # the label image
      if ( doCropping )
        {
        fimg = cropImage(y[[ind]], plotimask )
        labimg <- as.array(  fimg )
        } else labimg <- as.array( y[[ind]] )
    } else labimg <- as.array(y[[ind]])  # the label image
    labimg[ labimg < window.overlay[1] ] = 0
    labimg[ labimg > window.overlay[2] ] = 0
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
    if ( useAbsoluteScale )
      {
      mncl = window.overlay[1]
      mxcl = window.overlay[2]
      }
    temp <- labimg
    temp <- (temp - mncl)/(mxcl - mncl) * (nlevels - 1)
    temp[ temp < 0 ] = 0
    temp[ temp > 255 ] = 0
    labimg <- temp
    locthresh <- round((window.overlay[1:2] - mncl) /
                      (mxcl - mncl) * (nlevels - 1))
    if ( ( min(locthresh) == max(locthresh) ) | any(is.na(locthresh) ) )
      locthresh=c(0,1)
    labslice = reoSlice( labimg )
    slicerow <- nrow(slice)
    slicecol <- ncol(slice)
    bigslice <- matrix(0, nrow = slicerow * winrows, ncol = (slicecol * wincols))
    rowsl <- 0
    for (sl in c(0:(length(slices) - 1))) {
      if (sl < dim(img)[imagedim]) {
        labslice = reoSlice2( labimg, sl )
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
    colorfun = rainbow
    heatvals <- heat.colors(nlevels, alpha = alpha)
    heatvals <- rainbow(nlevels, alpha = alpha)
    if ( color.overlay[ind] != "jet" & color.overlay[ind] != "viridis" & color.overlay[ind] != "magma" & color.overlay[ind] != "plasma" & color.overlay[ind] != "inferno"   )
      colorfun <- colorRampPalette(c("white", color.overlay[ind]), interpolate = c("spline"),
        space = "Lab")
    if (color.overlay[ind] == "jet") {
      # print('use jet')
      colorfun <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
        "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), interpolate = c("spline"),
        space = "Lab")
    }
    if (color.overlay[ind] == "viridis") {
      colorfun <- colorRampPalette( viridis::viridis( nlevels ) , interpolate = c("spline"),
        space = "Lab")
    }
    heatvals <- colorfun(nlevels)
    if ( color.overlay[ind] == "viridis" ) heatvals <- viridis::viridis( nlevels )
    if ( color.overlay[ind] == "magma" ) heatvals <- viridis::magma( nlevels )
    if ( color.overlay[ind] == "plasma" ) heatvals <- viridis::plasma( nlevels )
    if ( color.overlay[ind] == "inferno" ) heatvals <- viridis::inferno( nlevels )
    # fix to get alpha transparency correct
    if (nchar(heatvals[1]) == 7 & alpha != 1) heatvals = paste0(heatvals,round(alpha*100,0))
    if (locthresh[1] > 1)
      heatvals[1:(locthresh[1] - 1)] <- NA
    if (locthresh[2] < (nlevels - 1)) {
      upper <- c((locthresh[2] + 1):nlevels)
      heatvals[upper] <- NA
    }
    heatvals[1]<-NA # dont overlay the background
    if ( useAbsoluteScale )
      {
      biglab[1]=255
      biglab[2]=0
      }
    if (min(biglab,na.rm=T) != max(biglab,na.rm=T))
      {
      invisible( suppressWarnings(
        pixmap::plot(
          pixmap::pixmapIndexed(biglab,
            col = heatvals, bbox = bbox), add = TRUE) ) )
      }
  } # for loop
  } # if not all na y
  # g<-biglab ; g[]<-0 ; b<-biglab ; b[]<-0 print('try rgb')
  # dd<-pixmapRGB(c(biglab,g,b),nrow=nrow(bigslice),ncol=ncol(bigslice),bbox=c(0,0,wincols,winrows))
  par( mar = startpar )  # set margins to zero ! less wasted space
  if (!is.na(outname))
    dev.off()
  invisible(return())
}
