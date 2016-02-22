#' Multi-run normalization, filtering and nuisance estimation for fMRI.
#'
#' This function leverages structural image processing based on ANTs
#' cortical thickness to implement standard functional image processing
#' recommendations.  The function will crop out the first k time frames,
#' do motion correction and produce a variety of nuisance regressors.  It will
#' also do spatial and temporal filtering as well as interpolation between
#' time frames that exceed a given framewise displacement.  Finally, we return
#' maps to the common coordinate system.  Output may be trimmed in the future
#' but currently provides access at different stages: merging versus filtering
#' and normalized, fused BOLD images in both subject and template space.
#'
#' @param img input time series antsImage.
#' @param fdthresh threshold for framewise displacement.  determines what time
#' frames should be interpolated. Set typically between 0.1 and 0.5 or Inf.
#' @param repeatMotionEst number of times to repeat motion estimation. We
#' recommend the value 2, in general.  The first run improves the template
#' estimate such that the 2nd run gives a more accurate correction.
#' @param freqLimits pair defining bandwidth of interest from low to high.
#' @param nCompCor number of compcor components to use in CSF plus WM mask.
#' @param polydegree eg 4 for polynomial nuisance variables.
#' @param structuralImage the structural antsImage of the brain.
#' @param structuralSeg a 3 or greater class tissue segmentation of the structural image.
#' @param structuralNodes regions of interest for network analysis, in the structural image space.
#' @param templateMap antsRegistration output mapping template space (as moving) to struturalImage (fixed).
#' @param templateImage template reference space to which we map the BOLD image.
#' @param smoothingSigmas 4-vector defining amount of smoothing in FWHM units.
#' @param extraRuns a list containing additional BOLD images (runs) to be merged with the first image.
#' @param verbose enables visualization as well as commentary.
#' @return outputs a list containing:
#' \itemize{
#'   \item{fusedImg: }{runs fused into one image and corrected if polydegree set}
#'   \item{fusedImgFilt: }{runs fused into one image and filtered}
#'   \item{seg2bold: }{strutural segmentation in BOLD space.}
#'   \item{nodes2bold: }{strutural nodes in BOLD space.}
#'   \item{boldToTemplate: }{BOLD fusedImg mapped to template space.}
#'   \item{mapsToTemplate: }{invertible maps from BOLD to template space.}
#'   \item{runID: }{Identifies which run over time series.}
#'   \item{boldMat: }{Matrix of filtered BOLD data.}
#'   \item{boldMask: }{BOLD mask.}
#'   \item{motionCorr: }{Motion corrected data.}
#'   \item{polyNuis: }{Polynomial nuisance variables.}
#'   \item{timevals: }{Temporal variables.}
#'   \item{nuisance: }{Nuisance variables.}
#'   \item{FD: }{mean framewise displacement.}
#'   \item{badtimes: }{time frames that are above FD threshold.}
#'   \item{dmnAtBOLDres: }{Default mode network at BOLD resolution in MNI space.}
#'   \item{seg2template: }{Segmentation in BOLD resolution MNI space.}
#'   \item{networkPriors2Bold: }{WIP: standard network priors in BOLD space.}
#'   \item{powersLabels: }{Powers nodes in BOLD space i.e. fusedImg.}
#'   \item{powersPoints: }{Powers points in BOLD space i.e. fusedImg.}
#'   \item{connMatNodes: }{User provided nodal system connectivity matrix.}
#'   \item{connMatNodesPartialCorr: }{TUser provided nodal system partial correlation matrix.}
#' }
#' @author Avants BB, Duda JT
#' @examples
#' # this example is long-running ( perhaps 10 minutes on an OSX laptop 2016 )
#' \dontrun{
#' exrun = fMRINormalization( verbose = TRUE ) # will download ex data
#' myid = "BBAvants" # some MRI data
#' pre = paste("~/rsfTest/",sep='')
#' fn = list.files( pre, full.names = TRUE, recursive = TRUE,
#'   pattern = glob2rx( paste( myid, "*rsfMRI0.nii.gz", sep='')  ) )
#' img  = antsImageRead( fn  )
#' sfn = list.files( pre, full.names = TRUE, recursive = TRUE,
#'   pattern = glob2rx( paste( myid, "*BrainSegmentation.nii.gz", sep='')  ) )
#' seg = antsImageRead( sfn )
#' t1fn = list.files( pre, full.names = TRUE, recursive = TRUE,
#'   pattern = glob2rx( paste( myid, "*BrainSegmentation0N4.nii.gz", sep='')  ) )
#' t1 = antsImageRead( t1fn   )
#' tt = fMRINormalization( img, repeatMotionEst=1,
#'   structuralImage=t1, structuralSeg=seg, verbose= TRUE )
#' # bold to template
#' antsApplyTransforms( mni, getAverageOfTimeSeries( img ),
#'    transformlist=tt$mapsToTemplate$toTemplate,
#'    whichtoinvert=tt$mapsToTemplate$toTemplateInversion )
#' # template to bold
#' antsApplyTransforms( getAverageOfTimeSeries( img ), mni,
#'    transformlist=tt$mapsToTemplate$toBold,
#'    whichtoinvert=tt$mapsToTemplate$toBoldInversion )
#' }
#'
#' @export fMRINormalization
fMRINormalization <- function(
  img,
  fdthresh=Inf,
  repeatMotionEst = 2,
  freqLimits = c( 0.01, 0.1 ),
  nCompCor = 0,
  polydegree = NA,
  structuralImage = NA,
  structuralSeg = NA,
  structuralNodes = NA,
  templateMap  = NA,
  templateImage = NA,
  smoothingSigmas = NA,
  extraRuns = NA,
  verbose = FALSE )
{
powers_areal_mni_itk <- NULL
if ( ! usePkg( "ggplot2" ) ) stop("need ggplot2")
if ( ! usePkg( "igraph"  ) ) stop("need igraph")
if ( ! usePkg( "pracma"  ) ) stop("need pracma")
if ( ! usePkg( "mFilter" ) ) stop("need mFilter")
if ( missing( img ) ) # for stand-alone testing
  {
  img = antsImageRead(  getANTsRData("rsbold")     )
  mask = antsImageRead( getANTsRData("rsboldmask") )
  structuralSeg = antsImageRead(  getANTsRData("rsboldseg")  )
  structuralImage = antsImageClone( structuralSeg )
  structuralImage[ structuralImage > 3 ] = 2
  }
# start with basic mean bold image
meanbold = getAverageOfTimeSeries( img )
mask = getMask( meanbold )
# Find first steady state timepoint
tr = antsGetSpacing(img)[4]
steady = floor( 20.0 / tr) + 1

# Global signal before cropping (save for visualization)
origmean = apply.antsImage(img, c(1,2,3), mean)
fullmean = rowMeans(timeseries2matrix(img, mask))
allTimes = dim(img)[4]

# Eliminate non steady-state timepoints
img = cropIndices(img, c(1,1,1,steady), dim(img) )
runNuis = rep(1, dim(img)[4] )
if ( ! all( is.na( extraRuns ) ) )
  {
  if ( class( extraRuns )[[1]] != "list"  )
    stop("extraRuns must be a list of antsImages.")
  for ( i in 1:length( extraRuns ) )
    {
    timg = extraRuns[[i]]
    allTimes = allTimes + dim( timg )[4]
    timg = cropIndices( timg, c(1,1,1,steady), dim(timg) )
    extraRuns[[i]] = timg
    runNuis = c( runNuis, rep(i+1, dim(timg)[4] ) )
    }
  }
runNuis = factor( runNuis )
timevals = 0
for ( runlev in levels( runNuis ) )
  {
  if ( length( timevals ) == 1 )
    {
    timevals = as.numeric( 1:sum( runNuis == runlev ) ) * tr
    } else {
    timevals = c( timevals, as.numeric( 1:sum( runNuis == runlev ) ) * tr )
    }
  }

## ----moco,message=FALSE,warnings=FALSE, fig.width=7, fig.height=3--------
mocoTxType = "BOLDRigid"
for ( i in 1:repeatMotionEst )
  {
  moco <- antsrMotionCalculation( img, fixed=meanbold, typeofTransform=mocoTxType )
  }
if ( repeatMotionEst < 1 )
  {
  mocoTxType = "QuickRigid"
  moco = antsrMotionCalculation( img, fixed=meanbold, typeofTransform=mocoTxType )
  }
meanbold = apply.antsImage( moco$moco_img, c(1,2,3), mean)

# at this point, we might map meanbold to the structural image
# and then motion correct all runs to that. this approach would be less
# biased.  however, let us hold that thought for later. FIXME

# now add any additional runs and merge moco results
if ( ! all( is.na( extraRuns ) ) )
  {
  for ( i in 1:length( extraRuns ) )
    {
    timg = extraRuns[[i]]
    # do a more accurate registration for this stage b/c it's a different run
    if ( verbose ) print( paste( "motion correction ", i ) )
    mocoTemp <- antsrMotionCalculation( timg, fixed=meanbold, typeofTransform=mocoTxType )
    if ( verbose ) print( "merge corrected image ( and tsDisplacement? )" )
    if ( usePkg("abind") )
      {
      ttmo = as.array( moco$moco_img )
      ttmo = abind::abind( ttmo, as.array( mocoTemp$moco_img ) )
      moco$moco_img = antsCopyImageInfo( moco$moco_img, as.antsImage(ttmo) )
      rm( ttmo )
      } else stop( "need abind package for the extraRuns feature")
    if ( verbose ) print("merge parameters, fd and dvars")
    moco$moco_params = rbind( moco$moco_params, mocoTemp$moco_params )
    moco$fd = rbind( moco$fd, mocoTemp$fd )
    moco$dvars = c( moco$dvars, mocoTemp$dvars )
    rm( mocoTemp )
    }
    if ( verbose ) print( "fusion done" )
  }

#############################################################
# now use polynomial regressors to match images across runs #
#############################################################
polyNuis = NA
if ( !is.na( polydegree ) ) # polynomial regressors
  {
  boldMat = timeseries2matrix( moco$moco_img, mask )
  meanmat = boldMat * 0
  rboldMat = boldMat * 0
  mycolmean = colMeans( boldMat )
  mycolmean = mycolmean * 1000 / mean( mycolmean ) # map to 1000
  for ( i in 1:nrow( meanmat ) ) meanmat[i,]=mycolmean
  for ( runlev in levels( runNuis ) )
    {
    polyNuis <- stats::poly( timevals[ runNuis == runlev ], degree = polydegree )
    rboldMat[ runNuis == runlev, ] <- residuals( lm( boldMat[ runNuis == runlev, ] ~ polyNuis ) )
    }
  # residualize but also keep the scale mean
  boldMat = rboldMat + meanmat
  rm( meanmat )
  rm( rboldMat )
  if ( verbose ) print("residuals done")
  polyNuis <- stats::poly( timevals, degree = polydegree )
  }
if ( verbose ) print("polyNuis done")
fusedImg = matrix2timeseries( moco$moco_img, mask, boldMat )

## ----mocoimg,message=FALSE,warnings=FALSE, fig.width=7, fig.height=3, echo=FALSE----
if ( verbose )
  invisible( plot( moco$moco_avg_img, axis=3, slices=1:30, ncolumns=10 ) )

if ( is.na( structuralImage ) ) # here do a quick hack so we can process bold alone
  {
  structuralImage = antsImageClone( meanbold )
  structuralSeg = antsImageClone( mask )
  mask1 = iMath(mask,"ME",1) # gm
  mask2 = iMath(mask,"ME",2) # wm
  structuralSeg = structuralSeg + mask1 + mask2
  t1brain = meanbold * mask
  }
  else t1brain = structuralImage * thresholdImage( structuralSeg, 1, Inf )

if ( ! exists("boldmap") )
  {
  if ( verbose ) print("boldmap to structure (no boldmap passed in)")
#  boldmap = antsRegistration( meanbold * mask, t1brain,
#    typeofTransform='QuickRigid', verbose=FALSE )
  if ( verbose ) print( meanbold )
  if ( verbose ) print( mask )
  maskmean = meanbold * mask
  if ( verbose ) print("mask mean done")
  boldmap = antsRegistration( maskmean, t1brain,
    typeofTransform='SyNBoldAff', verbose=FALSE )
  if ( verbose ) print("boldmap to structure done")
  }

notemplateMap = FALSE
if ( any( is.na( templateMap ) ) )
  {
  notemplateMap = TRUE
  mni = antsImageRead( getANTsRData( "mni" ) )
  if ( verbose ) print("boldmap to template")
  templateMap = antsRegistration( t1brain, mni, typeofTransform='SyN',
    verbose = FALSE )
  }

mni2boldmaps = c( boldmap$fwdtransforms, templateMap$fwdtransforms )
mni2boldmapsInv = c(  templateMap$invtransforms , boldmap$invtransforms )
seg2bold = antsApplyTransforms( meanbold, structuralSeg, boldmap$fwdtransforms,
  interpolator = "NearestNeighbor" )
if ( verbose )
  {
  plot( meanbold , boldmap$warpedmovout %>% iMath("Canny", 10, 1, 1) )
  plot( meanbold , maskImage( seg2bold, seg2bold, 2 ) )
  }

## ----mocomatrix,message=FALSE,warnings=FALSE, fig.width=7, fig.height=3----
nVox = length(which(as.array(mask)==1))
vox = sample(1:nVox, 1000)
if ( verbose )
  {
  invisible(plot(as.antsImage( t(timeseries2matrix(moco$moco_img,mask)[,vox]))))
  }
#########################################
# extract just the transform parameters #
#########################################
reg_params <- as.matrix( moco$moco_params )
dvars <- computeDVARS( timeseries2matrix( fusedImg, mask ) )

## ----badtimes,message=FALSE,warnings=FALSE, fig.width=7, fig.height=3----
goodtimes = (1:nrow( moco$moco_img ))
badtimes = which(moco$fd$MeanDisplacement > fdthresh )
haveBadTimes = FALSE
if ( length( badtimes ) > 0 )
  {
  goodtimes = goodtimes[-badtimes]
  haveBadTimes = TRUE
  } else badtimes = NA

boldMat = timeseries2matrix( fusedImg, mask )
nTimes = nrow( boldMat )
if ( haveBadTimes )
  {
  if ( verbose  )
    {
    print( "badtimes" )
    print( badtimes )
    }
  for ( v in c(1:nVox) )
    {
    boldMat[badtimes,v]=spline( c(1:nTimes)[goodtimes], boldMat[goodtimes,v],
      method='natural', xout=badtimes )$y
    }
  # FIXME - may not want to do this ie may want to avoid using badtimes
  haveBadTimes = FALSE
  goodtimes = ( 1:nTimes )
  }

## ----nuisance,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5----
# white matter is labeled as 3
wmMask = seg2bold*1*mask
wmMask[ wmMask != 3] = 0
wmMask[ wmMask == 3 ] = 1
wmMask = iMath( wmMask, "ME", 1)
wmVox = which(subset(wmMask, mask > 0 )==1)
wmMean = rowMeans(boldMat[,wmVox])

# CSF is labeled as 1
csfMask = seg2bold*1
csfMask[ csfMask != 1] = 0
csfVox = which(subset(csfMask, mask > 0)==1)
csfMean= rowMeans(boldMat[,csfVox])

globalMean = rowMeans(boldMat)
tissueNuis = cbind( globalMean, wmMean, csfMean )
if ( haveBadTimes ) {
  for ( v in c(1:dim(tissueNuis)[2]) ) {
    tissueInterp = spline( c(1:nTimes)[goodtimes], tissueNuis[goodtimes,v],
      method='natural', xout=badtimes )$y
    tissueNuis[badtimes,v]=tissueInterp
    }
  }
tissueDeriv = rbind( rep(0,dim(tissueNuis)[2]), diff(tissueNuis,1) )

# Save mean cortex signal for later plotting
ctxMask = seg2bold*1
ctxMask[ ctxMask != 2] = 0
ctxMask[ ctxMask == 2 ] = 1
ctxVox = which(subset(ctxMask, mask > 0)==1)
ctxMean = rowMeans(boldMat[,ctxVox])

## ----regression,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5----
mocoNuis              = reg_params
mocoNuis2             = reg_params * reg_params
colnames( mocoNuis2 ) = paste( "MocoSqr", 1:ncol( mocoNuis2 ) , sep='' )
mocoNuis  = cbind( mocoNuis, mocoNuis2 )
mocoDeriv = rbind( rep( 0,  dim(mocoNuis)[2] ), diff( mocoNuis, 1 ) )
colnames( mocoDeriv ) = paste( "MocoDeriv", 1:ncol( mocoDeriv ) , sep='' )
nuisance = cbind( mocoNuis, mocoDeriv, tissueNuis, tissueDeriv, dvars=dvars )
nuisance = cbind( mocoNuis, mocoDeriv, globalMean=globalMean, dvars=dvars )
nuisance = cbind( nuisance, runs=runNuis )
if ( nCompCor > 0 )
  {
# use seg2bold and moco_img to get a better compcor set "anatomical compcor"
# http://www.ncbi.nlm.nih.gov/pubmed/25987368
  tempMask = thresholdImage( seg2bold, 1, 1 )
  tempMask = tempMask + thresholdImage( seg2bold, 3, 3 ) %>% iMath( "ME", 1 )
  tempMat = timeseries2matrix( fusedImg, tempMask )
  compcorNuis = compcor( tempMat, nCompCor )
  colnames( compcorNuis ) = paste("compcor",1:ncol(compcorNuis), sep='' )
  nuisance = cbind( nuisance, compcorNuis )
  rm( tempMat  )
  rm( tempMask )
  }

## ----smooth,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5------
if ( any( is.na( smoothingSigmas ) ) )
  {
  sptl    = sqrt( sum( antsGetSpacing(img)[1:3]^2  )) * 1.5
  smoothingSigmas = c( sptl, sptl, sptl, 1.0 )
  }
img     = smoothImage( fusedImg, smoothingSigmas, FWHM=TRUE )
boldMat = timeseries2matrix( img, mask )
if (  ( length( freqLimits ) == 2  ) & ( freqLimits[1] < freqLimits[2] ) )
  {
  locruns = unique( runNuis )
  boldMat <- frequencyFilterfMRI( boldMat[ runNuis == 1, ],
    tr=tr, freqLo=freqLimits[1], freqHi=freqLimits[2], opt="trig" )
  if ( max( locruns ) > 1 )
    {
    for ( myrun in locruns[ locruns > 1 ] )
      {
      boldMatTemp <- frequencyFilterfMRI( boldMat[ runNuis == myrun, ],
        tr=tr, freqLo=freqLimits[1], freqHi=freqLimits[2], opt="trig" )
      boldMat = rbind( boldMat , boldMatTemp )
      }
    }
  }
fusedImgFilt = matrix2timeseries( fusedImg, mask, boldMat )
#################
connMatNodes = NA
if ( ! is.na( structuralNodes ) )
  {
  dmnnodes = antsApplyTransforms(
    meanbold, structuralNodes, boldmap$fwdtransforms,
    interpolator = 'NearestNeighbor' )
  ulabs = sort( unique( dmnnodes[ mask == 1 & dmnnodes > 0 ] ) )
  dmnlist = list()
  for ( i in 1:length( ulabs ) )
    dmnlist[[i]] = thresholdImage( dmnnodes, ulabs[i], ulabs[i] )
  dmnpr = imageListToMatrix( dmnlist, mask )
  dmnref = ( boldMat %*% t(dmnpr) )
  connMatNodes = cor( dmnref )
  }

connMatNodesPartialCorr = NA
if ( usePkg( "corpcor" ) )
  connMatNodesPartialCorr = corpcor::cor2pcor( connMatNodes ) # partial correlation

# get priors for different networks
networkPriors2Bold=NA
betasI = NA
if ( ! exists( "networkPriors" ) ) # & notemplateMap )
  {
  networkPriors = getANTsRData( "fmrinetworks" )
  networkPriors2Bold = networkPriors$images
  for ( i in 1:length(networkPriors2Bold) )
    networkPriors2Bold[[i]] = antsApplyTransforms( meanbold,
      networkPriors2Bold[[i]], mni2boldmaps )
  if ( FALSE )
    {
    pr = imageListToMatrix( networkPriors2Bold, mask )
    refSignal = scale( boldMat %*% t(pr) )
    networkDf = data.frame( ROI=refSignal[goodtimes,1],  nuisance[goodtimes,] )
    mdl = lm( scale(boldMat[goodtimes,]) ~ . , data=networkDf )
    bmdl = bigLMStats( mdl , 0.01 )
    betas = bmdl$beta.t["ROI",]
    betasI = makeImage( mask, betas )
    loth = quantile(  betas, probs=0.8 )
    if ( verbose )
      plot( meanbold, betasI, axis=3, nslices=30, ncolumns=10,
            window.overlay = c( loth, max(betas) ) )
    }
  }

concatenatedMaps =
  list( toBold =  mni2boldmaps, toBoldInversion=rep(FALSE,4),
        toTemplate =  mni2boldmapsInv,
        toTemplateInversion = c( TRUE, FALSE, TRUE, FALSE ) )

boldToTemplate = NA
dmnAtBOLDres = NA
seg2template = NA
if ( exists("mni") & is.na( templateImage ) )
  templateImage = resampleImage( mni, rep( 2.0 , 3 ) )
if ( !is.na( templateImage ) )
  {
  ## map the fusedImg to the common template space
  boldToTemplate = antsApplyTransforms( fixed = templateImage, moving = fusedImg,
           transformlist = concatenatedMaps$toTemplate,
           whichtoinvert = concatenatedMaps$toTemplateInversion,
           imagetype = 3 )
  dmnnodes = antsImageRead( getANTsRData("mnidfn") )
  dmnAtBOLDres = resampleImageToTarget( dmnnodes, templateImage, 1  )
  seg2template = antsApplyTransforms( templateImage, structuralSeg,
    templateMap$invtransforms,
    interpolator = "NearestNeighbor" )
  }

######################################################
## FIXME - this only works if maps are to MNI space ##
######################################################
## ----networklabels,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5----
data( "powers_areal_mni_itk", package = "ANTsR", envir = environment() )
pts = antsApplyTransformsToPoints( 3, powers_areal_mni_itk,
         transformlist = concatenatedMaps$toTemplate,
         whichtoinvert = concatenatedMaps$toTemplateInversion )
powersLabels = makePowersPointsImage( pts, mask, radius = 6 )
if ( verbose )
  plot( meanbold, powersLabels, axis=3, nslices=30, ncolumns=10,
    window.overlay = c( 1, max(powersLabels) ) )

return(
      list(
          fusedImg      = fusedImg,
          fusedImgFilt  = fusedImgFilt,
          seg2bold      = seg2bold,
          nodes2bold    = dmnnodes,
          boldToTemplate = boldToTemplate,
          mapsToTemplate = concatenatedMaps,
          runID         = runNuis,
          boldMat       = boldMat,
          boldMask      = mask,
          motionCorr    = moco,
          polyNuis      = polyNuis,
          timevals      = timevals,
          nuisance      = nuisance,
          FD            = moco$fd$MeanDisplacement,
          badtimes      = badtimes,
          dmnAtBOLDres  = dmnAtBOLDres,
          seg2template  = seg2template,
          networkPriors2Bold = networkPriors2Bold,
          powersLabels  = powersLabels,
          powersPoints  = pts,
          connMatNodes  = connMatNodes,
          connMatNodesPartialCorr = connMatNodesPartialCorr
          )
       )

## ----roimeans,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5----
labelMask = powersLabels*1
labelMask[labelMask > 0] = 1
labelMask[mask == 0] = 0
labelVox = which(subset(labelMask, mask > 0)==1)

labeledBoldMat = boldMat[goodtimes,labelVox]
labels = powersLabels[labelMask > 0]

nLabels = max(labels)
roiMat = matrix(0, nrow=dim(labeledBoldMat)[1], ncol=nLabels)
for ( i in c(1:nLabels) ) {
  if (length(which(labels==i)) > 1 ) {
    roiMat[,i] = rowMeans(labeledBoldMat[,(labels==i)])
  }
}
nActualTimes = dim(roiMat)[1]


## ----sysmean,message=FALSE,warnings=FALSE, fig.width=7, fig.height=10, echo=TRUE----
systemNames = levels(pts$SystemName)
nSystems = length(systemNames)
sysMatMean = matrix(0, nrow=dim(labeledBoldMat)[1], ncol=nSystems)
sysMatSD = matrix(0, nrow=dim(labeledBoldMat)[1], ncol=nSystems)
systems = pts$SystemName[labels]

for ( i in 1:nSystems ) {
  sys = systemNames[i]
  sysIdx = which(systems==sys)
  if ( length(sysIdx) > 0)
    {
    sysMatMean[,i] = rowMeans(labeledBoldMat[,sysIdx])
    sysMatSD[,i] = apply(labeledBoldMat[,sysIdx], 1, sd)
    }
}


## ----corr,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5--------
missingROIs = which(colMeans(roiMat)==0)
goodROIs = (1:nLabels)
if ( length(missingROIs) > 0 ) {
  goodROIs = goodROIs[-missingROIs]
}

connMat = suppressWarnings(cor(roiMat))
diag(connMat) = rep(0, length(diag(connMat)) )
if ( length(missingROIs) > 0 ) {
  connMat[missingROIs,] = 0
  connMat[,missingROIs] = 0
}

## ----adjacency,message=FALSE,warnings=FALSE, fig.width=5, fig.height=5----
density = 0.1
nEdges = length( upper.tri( connMat ) ) * density
thresh = sort( connMat[upper.tri(connMat)], decreasing=T)[nEdges]
adj = 1 * ( connMat >= thresh )

bingraph = igraph::graph.adjacency(adj, mode="undirected", weighted=NULL)
components = igraph::clusters(bingraph)
maxID = which(components$csize == max(components$csize))[1]

adj[components$membership!=maxID,] = 0
adj[,components$membership!=maxID] = 0
bingraph = igraph::graph.adjacency(adj, mode="undirected", weighted=NULL)

if ( verbose ) invisible(plot(as.antsImage(adj)))

## ----adjacencyplot,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5, echo=FALSE----
if ( verbose )
  {
  pts$SystemName = factor(pts$SystemName, levels=c("Sensory/Somatomotor Hand", "Sensory/Somatomotor Mouth", "Cingulo-opercular Task Control", "Auditory", "Default Mode", "Memory Retrieval", "Visual", "Fronto-parietal Task Control", "Salience", "Subcortical", "Ventral Attention", "Dorsal Attention", "Cerebellar", "Uncertain"))
  graph = igraph::graph.adjacency( adj, mode="directed", weighted=NULL )
  igraph::V(graph)$name = pts$ROI
  igraph::V(graph)$comm = pts$SystemName
  igraph::V(graph)$degree = igraph::degree(graph)

  systems = levels(pts$SystemName)
  systemNames = as.character(systems)
  }

# Retain only the largest connected component
bingraph = igraph::graph.adjacency(adj, mode="undirected", weighted=NULL)
components = igraph::clusters(bingraph)
maxID = which(components$csize == max(components$csize))[1]
adj[components$membership!=maxID,] = 0
adj[,components$membership!=maxID] = 0
graph = igraph::graph.adjacency( adj, mode="undirected", weighted=NULL )

# Set node colors
graph = igraph::set.vertex.attribute(graph, "r", index=igraph::V(graph), value=as.double(pts$r))
graph = igraph::set.vertex.attribute(graph, "g", index=igraph::V(graph), value=as.double(pts$g))
graph = igraph::set.vertex.attribute(graph, "b", index=igraph::V(graph), value=as.double(pts$b))

# Set edge colors
edges = igraph::get.edges( graph, igraph::E(graph) )
nEdges = dim(edges)[1]
er = rep(200, nEdges)
eg = rep(200, nEdges)
eb = rep(200, nEdges)

# colors for intra-system connections
#  gray for inter-system connections
for ( e in c(1:nEdges) )
  {
  if ( pts$SystemName[edges[e,1]] == pts$SystemName[edges[e,2]] )
   {
    er[e] = pts$r[edges[e,1]]
    eg[e] = pts$g[edges[e,1]]
    eb[e] = pts$b[edges[e,1]]
    }
  }

graph = igraph::set.edge.attribute(graph, "r", index=igraph::E(graph), value=as.double(er))
graph = igraph::set.edge.attribute(graph, "g", index=igraph::E(graph), value=as.double(eg))
graph = igraph::set.edge.attribute(graph, "b", index=igraph::E(graph), value=as.double(eb))

# uncomment line below to write out graph
# write.graph(graph, "network.graphml", format="graphml", prefixAttr=FALSE)
graph = igraph::graph.adjacency( adj, mode="undirected", weighted=NULL )
deg = igraph::degree(graph)
deg[ deg == 0 ] = NA
pathsmat =  igraph::shortest.paths(graph, weights=NA)
pathsmat[!is.finite(pathsmat)] = NA
paths = rowMeans(pathsmat, na.rm=TRUE)
paths[paths==0] = NA
clust = igraph::transitivity(graph, type="local")
clust[deg < 2] = NA
pager = igraph::page.rank(graph)$vector
pager[deg < 2] = NA
# from http://pastebin.com/XqkEYtJS
leff <- numeric(length(deg))
goodnodes <- which(deg > 1)
leff[goodnodes] <- sapply( goodnodes,
  function( x )
    {
    neighbs <- igraph::neighbors(graph, v=x)
    g.sub <- igraph::induced.subgraph(graph, neighbs)
    Nv <- igraph::vcount(g.sub)
    lpaths <- igraph::shortest.paths(g.sub, weights=NA)
    lpaths <- paths[upper.tri(lpaths)]
    pathsup <- lpaths[upper.tri(lpaths)]
    2 / Nv / (Nv - 1) * sum(1 / lpaths[which(is.na(lpaths)==FALSE)])
    }
  )
leff[ deg < 2 ] = NA
leff[ which( is.na( deg ) == TRUE ) ] = NA

## ----cnodeplot,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5, echo=FALSE----
nNodes = length(deg)
cnode.dat = data.frame(Node=rep(1:nNodes,5))
cnode.dat$Value = c( deg, paths, leff, clust, pager )
cnode.dat$Metric = c(
  rep("Degree", nNodes),
  rep("Shortest Path", nNodes),
  rep("Local Efficiency", nNodes),
  rep("Clustering Coefficient", nNodes),
  rep("Page-Rank", nNodes) )

geff<-1/(igraph::shortest.paths(graph))
geff[!is.finite(geff)]<-NA
geff<-mean(geff,na.rm=TRUE)
cc = igraph::transitivity(graph)


}
