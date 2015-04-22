#' brain segmentation based on geometric priors
#'
#' uses topological constraints to enhance accuracy of brain segmentation
#'
#' @param img input image or list of images (multiple features) where 1st image
#' would typically be the primary constrast
#' @param brainmask binary image
#' @param priors spatial priors, assume first is csf, second is gm, third is wm
#' @param seginit a previously computed segmentation which should have the structure of \code{atropos} or \code{kmeansSegmentation} output
#' @param vesselopt one of bright, dark or none
#' @param vesselk integer for kmeans vessel-based processing
#' @param gradStep scalar for registration
#' @param mrfval e.g. 0.05 or 0.1
#' @param atroposits e.g. 5 iterations
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
#' seg = geoSeg( img, bmk, priors )
#'
#' @export geoSeg
geoSeg <- function( img, brainmask, priors, seginit,
  vesselopt="none", vesselk=2,
  gradStep=1, mrfval=0.1, atroposits=5 )
  {
  if ( typeof( img ) == "S4" ) img=list( img )
  if ( ! exists("vesselopt") ) vesselopt="none"
  idim = img[[1]]@dimension

  mrfterm=paste("[",mrfval,",",paste(rep(1,idim),collapse='x'),"]")
  atroposits=paste('[',atroposits,',0]')

  # 1 vessels via bright / dark
  if ( vesselopt == 'bright' | vesselopt == 'dark' )
  {
  vseg <- kmeansSegmentation( img[[1]], vesselk, brainmask )
  if ( vesselopt == 'bright' )
    mask = thresholdImage( vseg$segmentation , 1, (vesselk-1) )
  if ( vesselopt == 'dark' )
    mask = thresholdImage( vseg$segmentation , 2, vesselk )
  } else mask = antsImageClone( brainmask )

  # 2 wm / gm use topology to modify wm
  if ( missing(seginit) ) {
    seginit <- atropos( d = idim, a = img, m = mrfterm, priorweight=0.25,
      c = atroposits,  i = priors, x = mask )
    }
  wm   = thresholdImage( seginit$segmentation, 3, 3 )
  wm   = wm %>% iMath( "GetLargestComponent" ) %>% iMath("MD",1)
  wmp  = wm * seginit$probabilityimages[[3]]
  cort = thresholdImage( seginit$segmentation, 2, 2 )
  gm   = seginit$probabilityimages[[2]]
  gmp  = gm + wmp

  # 3 wm / gm use diffeo to estimate gm
  tvreg = antsRegistration( gmp, wmp, typeofTransform = "TVMSQ",
    gradStep=gradStep, mask=cort )

  # 4 wm / gm / csf priors from jacobian
  jac    = createJacobianDeterminantImage( wmp, tvreg$fwdtransforms[[1]], 0)
  jacinv = createJacobianDeterminantImage( wmp, tvreg$invtransforms[[1]], 0)
  thkj   = antsApplyTransforms( fixed=wmp, moving=jacinv,
       transformlist=tvreg$fwdtransforms ) * gm

  #####################################
  # 5 resegment with new priors begin #
  #####################################
  #
  # gm topology constraint based on gm/wm jacobian
  thksig = antsImageClone( thkj )
  a      = 0.05
  beta   = 0.5
  thksig[ mask == 1 ] = 1.0 / ( 1 + exp( -1.0 * ( thkj[mask==1] - beta ) / a ) )
  seginit$probabilityimages[[2]] = priors[[2]] * thksig
  #
  # csf topology constraint based on gm/wm jacobian
  thkcsf = thresholdImage( thkj, 0.05, 0.5  ) * iMath( wm, "Neg" )
  thkcsf = smoothImage( thkcsf, 0.5 )
  seginit$probabilityimages[[1]] = priors[[1]] * iMath( thksig, "Neg")
  seginit$probabilityimages[[1]] = priors[[1]] + thkcsf
  #
  # wm topology constraint based on largest connected component
  # and excluding high gm-prob voxels
  seginit$probabilityimages[[3]] = priors[[3]] * wm
  seginit$probabilityimages[[3]] = priors[[3]] * iMath( thksig, "Neg")
  #
  # now resegment with topology-modified priors
  s3 <- atropos( d = idim, a = img, m = mrfterm, priorweight=0.25,
    c = atroposits,  i = seginit$probabilityimages, x = mask )
  ###################################
  # 5 resegment with new priors end #
  ###################################
  return( list( segobj=s3, seginit=seginit, thkcsf=thkcsf, thkj=thkj ) )
}

# 5 curvature seg - use results of 4 to segment based on
# 5.1 gm/wm  image
# 5.2 gm/csf image
# done!
