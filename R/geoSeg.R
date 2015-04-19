#' brain segmentation based on geometric priors
#'
#' uses topological constraints to enhance accuracy of brain segmentation
#'
#' @param img input image
#' @param brainmask binary image
#' @param priors spatial priors
#' @param vesselopt one of bright, dark or none
#' @return list of segmentation result images
#' @author Brian B. Avants
#' @examples
#'
#' img = antsImageRead( getANTsRData("simple") ,2)
#' img = n3BiasFieldCorrection( img , 4 )
#' img = n3BiasFieldCorrection( img , 2 )
#' bmk = getMask( img )
#' segs <- kmeansSegmentation( img, 3, bmk )
#' priors = segs$probabilityimages
#' seg = geoSeg( img, bmk, priors, 'none' )
#'
#' @export geoSeg
geoSeg <- function( img, brainmask, priors, vesselopt="none" )
  {
  if ( ! exists("vesselopt") ) vesselopt="none"
  dim = img@dimension

  # 1 vessels via bright / dark
  if ( vesselopt != 'none' )
  {
  vseg <- atropos( d = dim, a = img, m = '[0.1,1x1]',
     c = '[5,0]',  i = 'kmeans[2]', x = brainmask )
  if ( vesselopt == 'bright' )
    mask<-thresholdImage( vseg$segmentation , 1, 1 )
  if ( vesselopt == 'dark' )
    mask<-thresholdImage( vseg$segmentation , 2, 2 )
  } else mask = antsImageClone( brainmask )

  # 2 wm / gm use topology to modify wm
  s2 <- atropos( d = dim, a = img, m = '[0.1,1x1]',
     c = '[50,0]',  i = priors, x = mask )
  wm   = thresholdImage( s2$segmentation, 3, 3 )
  wm   = wm %>% iMath( "GetLargestComponent" ) %>% iMath("MD",1)
  wmp  = wm * s2$probabilityimages[[3]]
  cort = thresholdImage( s2$segmentation, 2, 2 )
  gm   = s2$probabilityimages[[2]]
  gmp  = gm + wmp

  # 3 wm / gm use diffeo to estimate gm
  tvreg = antsRegistration( gmp, wmp, typeofTransform = "TVMSQ",
    gradStep=1.0, mask=cort )

  # 4 wm / gm / csf priors from jacobian
  jac    = createJacobianDeterminantImage( wmp, tvreg$fwdtransforms[[1]], 0)
  jacinv = createJacobianDeterminantImage( wmp, tvreg$invtransforms[[1]], 0)
  thkj   = antsApplyTransforms( fixed=wmp, moving=jacinv,
       transformlist=tvreg$fwdtransforms ) * gm

  # 5 resegment with new priors
  thksig = antsImageClone( thkj )
  a      = 0.01
  beta   = 0.6
  thksig[ mask == 1 ] = 1.0 / ( 1 + exp( -1.0 * ( thkj[mask==1] - beta ) / a ) )
  thkcsf = thresholdImage( thkj, 0.05, 0.5  ) * iMath( wm, "Neg" )
  thkcsf = smoothImage( thkcsf, 0.5 )
  s2$probabilityimages[[3]] = s2$probabilityimages[[3]] * wm
  s2$probabilityimages[[3]] = s2$probabilityimages[[3]] * iMath( thksig, "Neg")
  s2$probabilityimages[[2]] = s2$probabilityimages[[2]] * thksig
  s2$probabilityimages[[1]] = s2$probabilityimages[[1]] * iMath( thksig, "Neg")
  s2$probabilityimages[[1]] = s2$probabilityimages[[1]] + thkcsf
  s3 <- atropos( d = dim, a = img, m = '[0.1,1x1]',
    c = '[50,0]',  i = s2$probabilityimages, x = mask )

  return( list( segobj=s3, seginit=s2, thkcsf=thkcsf, thkj=thkj ) )
}

# 5 curvature seg - use results of 4 to segment based on
# 5.1 gm/wm  image
# 5.2 gm/csf image
# done!
