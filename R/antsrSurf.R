#' Render a function onto a surface.
#'
#' Use a system call to \code{antsSurf} in order to render a function onto
#' a surface.  This requires \code{antsSurf} to be in the environment as well
#' as \code{ConvertScalarImageToRGB}, both available in Advanced Normalization
#' Tools software on github.
#'
#' @param x input antsImage defining the surface on which to render
#' @param y input antsImage list defining the function to render
#' on the surface. these image(s) should be in the same space as \code{x}.
#' @param z input antsImage list mask for each \code{y} function to render
#' on the surface. these image(s) should be in the same space as \code{y}.
#' @param quantlimits lower and upper quantile limits for overlay
#' @param colormap character, one of: grey, red, green, blue, copper, jet, hsv,
#' spring, summer, autumn, winter, hot, cool, overunder, custom
#' @param alpha transparency vector for underlay and each overlay, default zero
#' @param inflationFactor number of inflation iterations to run
#' @param smoothingSigma gaussian smooth the overlay by this sigma
#' @param rotationParams 3 Rotation angles expressed in degrees or a matrix of
#' rotation parameters that will be applied in sequence.
#' @param overlayLimits absolute lower and upper limits for functional overlay.
#' this parameter will override \code{quantlimits}.  Currently, this will set
#' levels above \code{overlayLimits[2]} to \code{overlayLimits[2]}. Can be a
#' list of length of y.
#' @param filename prefix filename for output pngs
#' @param antspath pass the ANTSPATH here otherwise we try to detect it from environment
#' @param verbose prints the command used to call \code{antsSurf}
#' @return no output
#' @author Avants BB, Tustison NJ
#' @examples
#'
#' \dontrun{
#'
#' ch2i = antsImageRead( getANTsRData("ch2") )
#' ch2seg = thresholdImage( ch2i, "Otsu", 3 )
#' wm   = thresholdImage( ch2seg, 3, 3 )
#' wm2 = smoothImage( wm, 1 ) %>% thresholdImage( 0.5, Inf )
#' kimg = weingartenImageCurvature( ch2i, 1.5  ) %>% smoothImage( 1 )
#' rp1 = matrix( c(90,180,90), ncol = 3 )
#' rp2 = matrix( c(90,180,270), ncol = 3 )
#' rp3 = matrix( c(90,180,180), ncol = 3 )
#' rp  = rbind( rp1, rp3, rp2 )
#' antsrSurf( x=wm2, y=list( kimg ), z=list( wm2 %>% iMath("MD",3) ),
#'   inflationFactor=255, overlayLimits=c(-0.3,0.3), verbose = TRUE,
#'   rotationParams = rp, filename=tempfile() )
#'
#' fn = getANTsRData( "surf" )
#' img = antsImageRead( fn ) # see antsSurf on github for data
#' wm   = thresholdImage( img, 3, 4 )
#' wm = thresholdImage(img, 3, 4) %>% iMath("FillHoles")
#' wms = smoothImage( wm, 1.0 )
#' wmk = weingartenImageCurvature( wms, 1.5, 0 )
#' # will display to screen
#' antsrSurf( x=wm, y = list( wmk %>% smoothImage(1)), z=list( wm %>% iMath("MD",1)),
#'  rotationParams = c(270, 0, 90), overlayLimits=c(-0.4,0.4) )
# surface and two overlays
#' blob = antsImageRead( getANTsRData( "blob") )
#' blob[1:266,1:266,1:100] = 0
#' z = list(  wm %>% iMath("MD",1) ,  blob %>% smoothImage( 1 ) )
#' antsrSurf( x=wm, y = list( wmk%>% smoothImage(1), blob ), z = z,
#'  colormap=c("jet","blue"), alpha=c(1,0.5,1),
#'  rotationParams = c(270, 0, 90),
#'  overlayLimits = list( c(-0.4,0.4) , c(0.9,1.001)) )
#'# separate pos and neg curvature
#' y = list( thresholdImage( wmk, 0.00, Inf ) ,
#'           thresholdImage( wmk, -100, -0.00 ) )
#' z = list( y[[1]] %>% iMath("MD",1) ,
#'           y[[2]] %>% iMath("MD",1) )
#' antsrSurf( x=wm, y=y, z=z, smoothingSigma=0.5, alpha=c( 1, 1, 1),
#'     colormap=c("red","blue"),
#'     inflationFactor=155, overlayLimits=list( c(0.5,1.0001), c(0.5,1.0001) ),
#'     verbose = TRUE, rotationParams = rp[1,] )
#'
#' }
#'
#' @export antsrSurf
antsrSurf <- function( x, y, z,
  quantlimits = c(0.1,0.9),
  colormap = 'jet',
  alpha = NA,
  inflationFactor = 25,
  smoothingSigma = 0.0,
  rotationParams = c(270,0,270),
  overlayLimits = NA,
  filename = NA,
  antspath = NA,
  verbose = FALSE )
{
domainImageMap = NA
if ( any( is.na( alpha ) ) ) {
  alpha = rep( 1, length(x)+length(y) )
}
if ( length( z ) != length( y ) ) stop("each y must have a mask in z")
if ( class( overlayLimits ) == 'numeric' )
  overlayLimits = list( overlayLimits )
# #' @param domainImageMap resamples surf and func to this domain FIXME
# check for needed programs
# first get antspath
if ( is.na( antspath) ) myantspath = Sys.getenv(c("ANTSPATH")) else myantspath = antspath
if ( nchar( myantspath ) == 0 & is.na( antspath ) )
  stop("Please set ANTSPATH in your environment")
if ( ! is.na( antspath ) ) myantspath = antspath
ass = file.exists( paste(myantspath,"antsSurf",sep='/') )
cvt = file.exists( paste(myantspath,"ConvertScalarImageToRGB",sep='/') )
if ( length( ass ) == 0 ) stop("This function needs antsSurf in the environment.")
if ( length( cvt ) == 0  ) stop("This function needs ConvertScalarImageToRGB in the environment.")
ass = paste(myantspath,"antsSurf",sep='/')
cvt = paste(myantspath,"ConvertScalarImageToRGB",sep='/')
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
            interpolation = 'Linear' )
      }
    }
  }
xfn = tempfile( fileext = ".nii.gz" )
antsImageWrite( x, xfn )
if ( is.na( filename ) ) filename = tempfile( )
if ( ! is.matrix( rotationParams ) )
  rotationParams = matrix( rotationParams, ncol=3 )
if ( nrow( rotationParams ) > 1 )
  {
  if ( !  usePkg("abind") | !  usePkg("png") | !  usePkg("grid"))
    {
    print("Need abind, grid and png")
    return(NULL)
    }
  }
pngs = rep( NA, nrow( rotationParams ) )
backgroundColor = paste("255x255x255x",alpha[1],sep='')
for( myrot in 1:nrow( rotationParams ) )
{
asscmd = paste( ass," -s [ ",xfn,",",backgroundColor,"] ")
if ( ! missing( y ) )
{
ct = 0
if ( length( colormap ) != length( y ) )
  colormap = rep( colormap, length.out = length( y ) )
for ( overlay in y )
  {
  ct = ct + 1
  wms = smoothImage( overlay, smoothingSigma  )
  myquants = quantile( wms[ abs(wms) > 0 ], quantlimits )
  if ( ! all( is.na( overlayLimits ) ) )
    {
    myquants = overlayLimits[[ ct ]]
    if ( all( myquants < 0 ) ) {
#      wms = wms * -1.0
#      myquants = rev( myquants ) * ( -1.0 )
#      wms = wms * thresholdImage( wms, myquants[1] , myquants[2] )
      }
    if ( all( myquants > 0 ) ) {
#      wms = wms * thresholdImage( wms, myquants[1] , myquants[2] )
      }
    } else {
#      wms[ wms <  quantlimits[1] ] = quantlimits[1]
#      wms[ wms >  quantlimits[2] ] = quantlimits[2]
    }
  if ( verbose ) {
    print( paste( "overlay quantiles for overlay" , ct  ) )
    print( myquants )
    print( 'range')
    print( range( wms ) )
    }
  kblobfn = tempfile( fileext = ".nii.gz" )
  antsImageWrite( z[[ ct ]], kblobfn )
  overlayfn = tempfile(fileext = ".nii.gz" )
  antsImageWrite( wms, overlayfn )
  csvlutfn = tempfile(fileext = ".csv" )
  overlayrgbfn = tempfile(fileext = ".nii.gz" )
  if ( verbose ) print( colormap[ct] )
  cvtcmd = paste( cvt," 3 ",overlayfn, overlayrgbfn,
    kblobfn, colormap[ct]," none ", myquants[1],myquants[2]," 0 255", csvlutfn )
  sss = system( cvtcmd )
  if ( verbose ) print( cvtcmd )
  if ( verbose ) cat( "\n" )
  alphaloc = alpha[ min( c( ct + 1, length(alpha) ) ) ]
  if ( verbose ) print( paste( "alpha", alphaloc ) )
  asscmd = paste( asscmd , "-f [ ",overlayrgbfn,", ",kblobfn,", ", alphaloc, " ] ")
  }
}
if ( nrow( rotationParams ) == 1 )
{
asscmd = paste( asscmd , " -i ", inflationFactor,
  " -d [",paste( rotationParams[myrot,], collapse='x' ),",255x255x255] " )
} else {
  pngext = myrot
  if ( myrot < 10 ) pngext = paste( "0",pngext,sep='' )
  if ( myrot < 100 ) pngext = paste( "0",pngext,sep='' )
  pngfnloc = paste( filename, pngext, ".png", sep='' )
  system( paste( "rm", pngfnloc ) )
  asscmd = paste( asscmd , " -i ", inflationFactor, " -d ", pngfnloc,
    "[",paste( rotationParams[myrot,], collapse='x' ),",255x255x255] ",sep='' )
}
if ( verbose ) print( asscmd )
sss = system( asscmd )
Sys.sleep( 3 )
if ( nrow( rotationParams ) > 1 ) pngs[ myrot ] = pngfnloc
}
Sys.sleep( 3 )
if ( nrow( rotationParams ) > 1 )
  {
  mypng = png::readPNG( pngs[ 1 ] )
  for ( i in 2:length( pngs ) )
    {
    mypng = abind::abind(  mypng, png::readPNG( pngs[ i ] ) , along = 2 )
    }
  png(paste(filename, ".png", sep = ""), width = dim(mypng)[2], height = dim(mypng)[1])
  grid::grid.raster(mypng)
  dev.off()
  }
}









#' Render an image volume with or without overlay.
#'
#' Use a system call to \code{antsVol} in order to render an image.  This
#' requires \code{antsVol} to be in the environment as well as
#' \code{ConvertScalarImageToRGB}, both available in Advanced Normalization
#' Tools software on github.
#'
#' @param x input antsImage defining the image to render
#' @param y input antsImage list defining the function to render
#' on the image. these image(s) should be in the same space.
#' @param quantlimits lower and upper quantile limits for overlay
#' @param colormap character, one of: grey, red, green, blue, copper, jet, hsv,
#' spring, summer, autumn, winter, hot, cool, overunder, custom
#' @param rotationParams 3 Rotation angles expressed in degrees or a matrix of
#' rotation parameters that will be applied in sequence.
#' @param overlayLimits absolute lower and upper limits for functional overlay.
#' this parameter will override \code{quantlimits}.  Currently, this will set
#' levels above \code{overlayLimits[2]} to \code{overlayLimits[2]}.
#' @param magnificationFactor zooms in on image during rendering
#' @param intensityTruncation lower and upper quantiles at which to truncate intensity
#' @param filename prefix filename for output pngs
#' @param antspath pass the ANTSPATH here otherwise we try to detect it from environment
#' @param verbose prints the command used to call \code{antsVol}
#' @return no output
#' @author Avants BB, Tustison NJ
#' @examples
#'
#' \dontrun{
#' ch2i = antsImageRead( getANTsRData("mni") )
#' ch2seg = thresholdImage( ch2i, "Otsu", 3 )
#' wm   = thresholdImage( ch2seg, 3, 3 )
#' kimg = weingartenImageCurvature( ch2i, 1.5  ) %>% smoothImage( 1 )
#' ap = path.expand( "~/code/ants-src/bin/" )
#' rp0 = matrix( c(90,180,90), ncol = 3 )
#' rp2 = matrix( c(0,0,0), ncol = 3 )
#' rp3 = matrix( c(270,90,90), ncol = 3 )
#' rp  = rbind( rp0, rp2, rp3 ) # pass these as rotationParams
#' antsrVol( wm, list( kimg ), quantlimits=c(0.01,0.99) )
#' }
#'
#' @export antsrVol
antsrVol <- function( x, y,
  quantlimits = c(0.1,0.9),
  colormap = 'jet',
  rotationParams = c(270,0,270),
  overlayLimits = NA,
  magnificationFactor = 1.0,
  intensityTruncation = c( 0.0, 1.0 ),
  filename = NA,
  antspath = NA,
  verbose = FALSE )
{
# check for needed programs
# first get antspath
myantspath = Sys.getenv(c("ANTSPATH"))
if ( nchar( myantspath ) == 0 & is.na( antspath ) )
  stop("Please set ANTSPATH in your environment")
if ( ! is.na( antspath ) ) myantspath = antspath
ass = file.exists( paste(myantspath,"antsVol",sep='/') )
cvt = file.exists( paste(myantspath,"ConvertScalarImageToRGB",sep='/') )
if ( length( ass ) == 0 ) stop("This function needs antsSurf in the environment.")
if ( length( cvt ) == 0  ) stop("This function needs ConvertScalarImageToRGB in the environment.")
ass = paste(myantspath,"antsVol",sep='/')
cvt = paste(myantspath,"ConvertScalarImageToRGB",sep='/')
xfn = tempfile( fileext = ".nii.gz" )
xmod = antsImageClone( x )
if ( intensityTruncation[1] > 0 | intensityTruncation[2] < 1 )
  xmod = iMath( x, "TruncateIntensity", intensityTruncation[1], intensityTruncation[2] )
antsImageWrite( xmod, xfn )
if ( is.na( filename ) ) filename = tempfile( )
if ( ! is.matrix( rotationParams ) )
  rotationParams = matrix( rotationParams, ncol=3 )
if ( nrow( rotationParams ) > 1 )
  {
  if ( !  usePkg("abind") | !  usePkg("png") | !  usePkg("grid"))
    {
    print("Need abind, grid and png")
    return(NULL)
    }
  }
pngs = rep( NA, nrow( rotationParams ) )
for( myrot in 1:nrow( rotationParams ) )
{
assvcmd = paste( ass," -i  ",xfn," ",sep='')
if ( ! missing( y ) )
{
ct = 0
if ( length( colormap ) != length( y ) )
  colormap = rep( colormap, length.out = length( y ) )
for ( overlay in y )
  {
  ct = ct + 1
  wms = smoothImage( overlay, 1.0 )
  myquants = quantile( overlay[ abs(overlay) > 0 ], quantlimits )
  if ( ! all( is.na( overlayLimits ) ) )
    {
    myquants = overlayLimits
    overlay[ overlay < myquants[1] ] = 0
    overlay[ overlay > myquants[2] ] = myquants[2]
    if ( verbose ) print( myquants )
    }
  kblob = thresholdImage( wms, myquants[1], Inf )
  kblobfn = tempfile( fileext = ".nii.gz" )
  antsImageWrite( kblob, kblobfn )
  overlayfn = tempfile(fileext = ".nii.gz" )
  antsImageWrite( overlay, overlayfn )
  csvlutfn = tempfile(fileext = ".csv" )
  overlayrgbfn = tempfile(fileext = ".nii.gz" )
  if ( verbose ) print( colormap[ct] )
  cvtcmd = paste( cvt," 3 ",overlayfn, overlayrgbfn,
    kblobfn, colormap[ct]," none ", myquants[1],myquants[2]," 0 255", csvlutfn )
  sss = system( cvtcmd )
  if ( verbose ) print( cvtcmd )
  if ( verbose ) cat( "\n" )
  assvcmd = paste( assvcmd , "-f [ ",overlayrgbfn,", ",kblobfn," ] ")
  }
}
if ( is.na( filename ) )
{
assvcmd = paste( assvcmd ,
  " -d [",magnificationFactor,",",paste( rotationParams[myrot,], collapse='x' ),",255x255x255] " )
} else {
  pngext = myrot
  if ( myrot < 10 ) pngext = paste( "0",pngext,sep='' )
  if ( myrot < 100 ) pngext = paste( "0",pngext,sep='' )
  pngfnloc = paste( filename, pngext, ".png", sep='' )
  system( paste( "rm", pngfnloc ) )
  assvcmd = paste( assvcmd ,  " -d ", pngfnloc,
    "[",magnificationFactor,",",
    paste( rotationParams[myrot,], collapse='x' ),",255x255x255] ",sep='' )
}
if ( verbose ) print( assvcmd )
sss = system( assvcmd )
if ( nrow( rotationParams ) > 1 ) pngs[ myrot ] = pngfnloc
}
if ( nrow( rotationParams ) > 1 )
  {
  mypng = png::readPNG( pngs[ 1 ] )
  for ( i in 2:length( pngs ) )
    {
    mypng = abind::abind(  mypng, png::readPNG( pngs[ i ] ) , along = 2 )
    }
  png(paste(filename, ".png", sep = ""), width = dim(mypng)[2], height = dim(mypng)[1])
  grid::grid.raster(mypng)
  dev.off()
  }
}
