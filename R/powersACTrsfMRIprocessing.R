#' Powers and ants cortical thickness processing for fMRI.
#'
#' This function leverages structural image processing based on ANTs
#' cortical thickness to implement Powers' functional image processing
#' recommendations.  The function will perform motion correction and
#' produce a variety of nuisance regressors.  It will also perform
#' spatial and temporal filtering.  In sum, these regressors mitigate
#' the wide variety of confounds that impact functional MRI.
#'
#' @param img input time series antsImage
#' @param fdthresh threshold for framewise displacement.  determines what time
#' frames should be interpolated. Set typically between 0.1 and 0.5.
#' @param repeatMotionEst number of times to repeat motion estimation.
#' @param freqLimits pair defining bandwidth of interest from low to high.
#' @param nCompCor number of compcor components to use.
#' @param structuralImage the structural antsImage of the brain.
#' @param structuralSeg a 3 or greater class tissue segmentation of the structural image.
#' @param structuralNodes regions of interest for network analysis, in the structural image space.
#' @param verbose enables visualization as well as commentary.
#' @return outputs a list containing:
#' \itemize{
#'   \item{boldMat: }{Matrix of filtered BOLD data.}
#'   \item{boldMask: }{BOLD mask.}
#'   \item{nuisance: }{Nuisance variables.}
#'   \item{dmnBetas: }{Default mode network beta map.}
#'   \item{connMatPowers: }{Powers nodes connectivity matrix.}
#'   \item{connMatNodes: }{User provided nodal system connectivity matrix.}
#'   \item{connMatNodesPartialCorr: }{TUser provided nodal system partial correlation matrix.}
#'   \item{FD: }{mean framewise displacement.}
#'   \item{DVARS: }{signal variability.}
#'   \item{goodtimes: }{good time points.}
#' }
#' @author Avants BB, Duda JT
#' @examples
#' # this example is long-running ( perhaps 10 minutes on an OSX laptop 2016 )
#' \dontrun{
#' exrun = powersACTrsfMRIprocessing( verbose = TRUE ) # will download ex data
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
#' tt = powersACTrsfMRIprocessing( img, fdthresh=0.2, repeatMotionEst=1,
#'   structuralImage=t1, structuralSeg=seg, verbose= TRUE )
#' }
#'
#' @export powersACTrsfMRIprocessing
powersACTrsfMRIprocessing <- function( img,
  fdthresh=0.2,
  repeatMotionEst = 1,
  freqLimits = c( 0.008, 0.09 ),
  nCompCor = 4,
  structuralImage = NA,
  structuralSeg = NA,
  structuralNodes = NA,
  verbose = FALSE )
{
print( "TODO:
  input list of structuralNodes, allow mapping rsf to reference space,
  allow custom template, take ACT maps as input")
if ( ! usePkg( "ggplot2" ) ) stop("need ggplot2")
if ( ! usePkg( "igraph"  ) ) stop("need igraph")
if ( ! usePkg( "pracma"  ) ) stop("need pracma")
if ( ! usePkg( "dplyr"   ) ) stop("need dplyr")
if ( ! usePkg( "mFilter" ) ) stop("need mFilter")
if ( missing(img) ) # for stand-alone testing
  {
  img = antsImageRead(  getANTsRData("rsbold")     )
  mask = antsImageRead( getANTsRData("rsboldmask") )
  structuralSeg = antsImageRead(  getANTsRData("rsboldseg")  )
  structuralImage = antsImageClone( structuralSeg )
  structuralImage[ structuralImage > 3 ] = 2
  }
meanbold = getAverageOfTimeSeries( img )
mask = getMask( meanbold )
t1brain = structuralImage * thresholdImage( structuralSeg, 1, Inf )
if ( ! exists("boldmap") )
  boldmap = antsRegistration( meanbold * mask, t1brain,
    typeofTransform='SyNBoldAff', verbose=verbose )
if ( ! exists("mnimap") )
  {
  mni = antsImageRead( getANTsRData( "mni" ) )
  mnimap = antsRegistration( t1brain, mni, typeofTransform='SyN',
    verbose=verbose )
  }
mni2boldmaps = c( boldmap$fwdtransforms, mnimap$fwdtransforms )
mni2boldmapsInv = c(  mnimap$invtransforms , boldmap$invtransforms )
mni2bold = antsApplyTransforms( meanbold, mni, mni2boldmaps )
seg2bold = antsApplyTransforms( meanbold, structuralSeg, boldmap$fwdtransforms,
  interpolator = "NearestNeighbor" )
if ( verbose )
  {
  plot( meanbold , boldmap$warpedmovout %>% iMath("Canny", 10, 1, 1) )
  plot( meanbold , mni2bold %>% iMath("Canny", 10, 1, 1) )
  plot( meanbold , maskImage( seg2bold, seg2bold, 2 ) )
  }

# Find first steady state timepoint
tr = antsGetSpacing(img)[4]
steady = floor(10.0 / tr) + 1

# Global signal before cropping (save for visualization)
origmean = apply.antsImage(img, c(1,2,3), mean)
fullmean = rowMeans(timeseries2matrix(img, mask))
allTimes = dim(img)[4]

# Eliminate non steady-state timepoints
img = cropIndices(img, c(1,1,1,steady), dim(img) )

# exclusion area
if ( verbose )
  {
  noss.data = data.frame(Start=0)
  noss.data$Stop = (steady-1)*tr
  noss.rect.aes = ggplot2::aes(xmin=Start,xmax=Stop,ymin=-Inf,ymax=Inf,fill="pink",alpha=0.2)

  # mean signal in brain
  ss.dat <- data.frame(Time=rep(1:allTimes)*tr)
  ss.dat$Values = fullmean

  ssPlot <- ggplot2::ggplot(ss.dat)
    ssPlot <- ssPlot + ggplot2::geom_line(ggplot2::aes(x=Time, y=Values), size=0.5)
    ssPlot <- ssPlot + ggplot2::geom_rect(data=noss.data, noss.rect.aes)
    ssPlot <- ssPlot + ggplot2::theme(text=ggplot2::element_text((size=10), legend.position="none")
    ssPlot <- ssPlot + ggplot2::ggtitle("Exclude points previous to magnetization steady state")
  print(ssPlot)
  invisible(plot(origmean, axis=3, slices=1:30, ncolumns=10))
  }

## ----moco,message=FALSE,warnings=FALSE, fig.width=7, fig.height=3--------
if ( ! exists("moco") )
  {
  for ( i in 1:repeatMotionEst )
    {
    moco <- antsMotionCalculation( img, fixed=meanbold, txtype="Rigid" )
    meanbold = apply.antsImage( moco$moco_img, c(1,2,3), mean)
    }
  }

## ----mocoimg,message=FALSE,warnings=FALSE, fig.width=7, fig.height=3, echo=FALSE----
if ( verbose )
  invisible( plot( moco$moco_avg_img, axis=3, slices=1:30, ncolumns=10 ) )

## ----mocomatrix,message=FALSE,warnings=FALSE, fig.width=7, fig.height=3----
nVox = length(which(as.array(mask)==1))
vox = sample(1:nVox, 1000)
if ( verbose )
  {
  invisible(plot(as.antsImage( t(timeseries2matrix(img,mask)[,vox]))))
  invisible(plot(as.antsImage( t(timeseries2matrix(moco$moco_img,mask)[,vox]))))
  }

# extract just the transform parameters
reg_params <- as.matrix( moco$moco_params[,3:8] ) # FIXME this is bad coding

nTimes = dim(reg_params)[1]
orderedBreaks = c("Framewise", "X", "Y", "Z", "Pitch", "Roll", "Yaw" )
moco.dat <- data.frame(Time=rep(1:nTimes, length(orderedBreaks) )*tr)
moco.dat$Values = c( as.vector(reg_params), moco$fd$MeanDisplacement )
moco.dat$Category = c( rep("Angle", 3*nTimes), rep("Displacement", 4*nTimes) )
moco.dat$Type = rep(c("Pitch", "Roll", "Yaw","X", "Y", "Z", "Framewise"), each=nTimes)
if ( verbose )
  {
  regPlot <- ggplot2::ggplot(moco.dat, ggplot2::aes(x=Time, y=Values, group=Type, colour=Type) )
    regPlot <- regPlot + ggplot2::geom_line(size=0.5)
    regPlot <- regPlot + ggplot2::theme(text=ggplot2::element_text(size=10), legend.position="top")
    regPlot <- regPlot + ggplot2::ggtitle("Motion correction parameters")
    regPlot <- regPlot + ggplot2::facet_grid(Category ~ ., scales="free" )
    regPlot <- regPlot + ggplot2::scale_color_discrete(breaks=orderedBreaks)
  print(regPlot)
  }

## ----dvar,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5, echo=FALSE----
scaling <- 1000.0 / mean(moco$moco_avg_img[mask>0])
dvars <- scaling * computeDVARS(timeseries2matrix(moco$moco_img, mask))
orig_dvars <- scaling * computeDVARS(timeseries2matrix(img, mask))

if ( verbose )
  {
  dvarType <- c(rep("Original",length(orig_dvars)), rep("Moco",length(dvars)) )
  dvarTime <- c(1:length(orig_dvars), 1:length(dvars))*tr
  dvar.data <- data.frame(DVARS=c(orig_dvars, dvars), Type=dvarType, Time=dvarTime)
  dvarType = factor(dvarType, levels=c("Original", "Moco"))

  dvarPlot <- ggplot2::ggplot(dvar.data, ggplot2::aes(x=Time, y=DVARS, group=Type, colour=Type) )
  dvarPlot <- dvarPlot + ggplot2::geom_line(size=0.5)
  dvarPlot <- dvarPlot + ggplot2::theme(text=ggplot2::element_text(size=10), legend.position="top")
  dvarPlot <- dvarPlot + ggplot2::ggtitle("DVARS: pre and post motion correction")
  dvarPlot <- dvarPlot + scale_colour_discrete(breaks=c("Original", "Moco"))
  print(dvarPlot)
  }

## ----badtimes,message=FALSE,warnings=FALSE, fig.width=7, fig.height=3----
goodtimes = (1:nTimes)
badtimes = which(moco$fd$MeanDisplacement > fdthresh )
haveBadTimes = FALSE
if ( length( badtimes ) > 0 )
  {
  badtimes = sort(c(badtimes, badtimes+1))
  goodtimes = goodtimes[-badtimes]
  haveBadTimes = TRUE
  } else badtimes = NA

## ----badtimesplot,message=FALSE,warnings=FALSE, fig.width=7, fig.height=3, echo=FALSE----
if ( haveBadTimes & verbose )
  {
  badstarts = which(moco$fd$MeanDisplacement > fdthresh )
  if ( length( badstarts ) == 0 ) badstarts = c(1)

  bad.data = data.frame(Time=(1:nTimes)*tr)
  bad.data$FD = moco$fd$MeanDisplacement

  bad.data.rect = data.frame(Start=badstarts*tr)
  bad.data.rect$Stop = (badstarts+1)*tr
  rect.aes = ggplot2::aes(xmin=Start,xmax=Stop,ymin=-Inf,ymax=Inf,fill="pink",alpha=0.2)

  badPlot <- ggplot2::ggplot(bad.data) + ggplot2::geom_line(ggplot2::aes(x=Time, y=FD))
  badPlot <- badPlot + ggplot2::geom_hline( yintercept=fdthresh, linetype="dashed", alpha=0.5 )
  badPlot <- badPlot + ggplot2::theme(text=ggplot2::element_text(size=10), legend.position="none")
  badPlot <- badPlot + ggplot2::ggtitle("Bad timepoints")
  badPlot <- badPlot + ggplot2::geom_rect(data=bad.data.rect, rect.aes)
  print(badPlot)
  }

## ----detrend,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5-----
global_pre <- rowMeans(timeseries2matrix(img, mask))
global_moco <- rowMeans(timeseries2matrix(moco$moco_img, mask))
boldMat = timeseries2matrix(moco$moco_img, mask)
boldMat[goodtimes,] = pracma::detrend(boldMat[goodtimes,])
if ( haveBadTimes ) boldMat[badtimes,] = NA

global_moco_detrend = rowMeans(boldMat)
if ( haveBadTimes ) global_pre[badtimes] = NA
if ( haveBadTimes ) global_moco[badtimes] = NA

## ----detrendplot,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5, echo=FALSE----
if ( verbose )
  {
  trend.dat = data.frame( Time=rep(1:nTimes,3) )
  trendType = c( rep("Original", nTimes), rep("Motion-corrected",nTimes) )
  trendType = c(trendType, rep("Moco & Detrended",nTimes) )
  trendNames = c(rep("Original",nTimes*2), rep("Detrended", nTimes))
  trendCategory = factor(trendNames, levels=c("Original", "Detrended"))
  trend.dat$Signal = c(global_pre, global_moco, global_moco_detrend)
  trend.dat$Type = trendType
  trend.dat$Category = trendCategory
  trendPlot <- ggplot2::ggplot(trend.dat, ggplot2::aes(x=Time, y=Signal, group=Type, colour=Type) )
  trendPlot <- trendPlot + ggplot2::geom_line(size=0.5)
  trendPlot <- trendPlot + ggplot2::theme(text=ggplot2::element_text(size=10), legend.position="top")
  trendPlot <- trendPlot + ggplot2::facet_grid(Category ~ ., scales="free" )
  trendPlot <- trendPlot + ggplot2::ggtitle("Detrending the time-series")
  print(trendPlot)
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
#csfMean = rowMeans(timeseries2matrix(detrendImg, csfMask))

globalMean = rowMeans(boldMat)
compcorTemp = compcor( boldMat[goodtimes,], nCompCor )
compcorNuis = matrix(0, nTimes, nCompCor )
compcorNuis[goodtimes, ] = compcorTemp
if ( haveBadTimes ) compcorNuis[badtimes, ] = NA
colnames( compcorNuis ) = paste("compcor",1:ncol(compcorNuis), sep='' )
tissueNuis = cbind(globalMean, wmMean, csfMean)
if ( haveBadTimes ) {
  for ( v in c(1:dim(tissueNuis)[2]) ) {
    tissueInterp = spline( c(1:nTimes)[goodtimes], tissueNuis[goodtimes,v],
      method='natural', xout=badtimes )$y
    tissueNuis[badtimes,v]=tissueInterp
    }
  }
tissueDeriv = rbind( rep(0,dim(tissueNuis)[2]), diff(tissueNuis,1) )

if ( verbose )
  {
  tissueType = c( rep("Global", nTimes), rep("White matter",nTimes), rep("CSF",nTimes) )
  tissueType = c(tissueType, rep("CompCor1",nTimes), rep("CompCor2",nTimes))
  tissueType = c(tissueType, rep("CompCor3",nTimes), rep("CompCor4",nTimes) )

  tissueCategory = c(rep("Tissue", nTimes*3), rep("CompCor", nTimes*4))

  signal = c(global_moco_detrend, wmMean, csfMean, compcorNuis[,1], compcorNuis[,2])
  signal = c(signal, compcorNuis[,3], compcorNuis[,4])

  tissue.dat = data.frame( Time=rep(1:nTimes,7) )
  tissue.dat$Signal = signal
  tissue.dat$Type = tissueType
  tissue.dat$Category = tissueCategory

  tissuePlot <- ggplot2::ggplot(tissue.dat, ggplot2::aes(x=Time, y=Signal, group=Type, colour=Type) )
  tissuePlot <- tissuePlot + ggplot2::geom_line(size=0.5)
  tissuePlot <- tissuePlot + ggplot2::theme(text=ggplot2::element_text(size=10), legend.position="top")
  tissuePlot <- tissuePlot + ggplot2::facet_grid(Category ~ ., scales="free" )
  tissuePlot <- tissuePlot + ggplot2::ggtitle("Nuisance parameters")
  print(tissuePlot)
  }

# Save mean cortex signal for later plotting
ctxMask = seg2bold*1
ctxMask[ ctxMask != 2] = 0
ctxMask[ ctxMask == 2 ] = 1
ctxVox = which(subset(ctxMask, mask > 0)==1)
ctxMean = rowMeans(boldMat[,ctxVox])

## ----regression,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5----
mocoNuis = cbind(reg_params, reg_params*reg_params)
mocoNuis = pracma::detrend(mocoNuis)
mocoDeriv = rbind( rep(0,dim(mocoNuis)[2]), diff(mocoNuis,1) )

nuisance = cbind( mocoNuis, mocoDeriv, tissueNuis, tissueDeriv, compcorNuis, dvars=dvars )

boldMat[goodtimes,] <- residuals( lm( boldMat[goodtimes,] ~ nuisance[goodtimes,] ) )

## ----regressionplot,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5, echo=FALSE----
ctxMeanRegressed = rowMeans(boldMat[,ctxVox])

if ( verbose )
  {
  cortex.dat =  data.frame( Time=rep(1:nTimes,2) )
  cortex.dat$Values = c(ctxMean, ctxMeanRegressed)
  cortex.dat$Type = c(rep("Original",nTimes), rep("Regressed",nTimes))
  cortexPlot = ggplot2::ggplot(cortex.dat, ggplot2::aes(x=Time, y=Values, group=Type, colour=Type))
  cortexPlot = cortexPlot + ggplot2::geom_line(size=0.5)
  cortexPlot = cortexPlot + ggplot2::theme(text=ggplot2::element_text(size=10), legend.position="top")
  cortexPlot = cortexPlot + ggplot2::ggtitle("Effect of nuisance parameter regression")
  cortexPlot = cortexPlot + ggplot2::facet_grid(Type ~ ., scales="free" )
  print(cortexPlot)
  }

## ----frequency,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5----
if ( haveBadTimes ) {
  for ( v in c(1:nVox) ) {
    boldMat[badtimes,v]=spline( c(1:nTimes)[goodtimes], boldMat[goodtimes,v],
      method='natural', xout=badtimes )$y
    }
  } else { # FIXME - may not want to do this ie may want to avoid using badtimes
    badtimes  = NA
    haveBadTimes = FALSE
    goodtimes = ( 1:nTimes )
    }

# save interpolated values for plotting
ctxMeanSpline = rowMeans(boldMat[,ctxVox])

if (  ( length( freqLimits ) == 2  ) & ( freqLimits[1] < freqLimits[2] ) )
  boldMat <- frequencyFilterfMRI( boldMat, tr=tr, freqLo=freqLimits[1],
    freqHi=freqLimits[2], opt="trig" )

# save filtered values for plotting
ctxMeanFiltered = rowMeans(boldMat[,ctxVox])
if ( haveBadTimes ) ctxMeanFiltered[badtimes] = NA

## ----smooth,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5------
img     = matrix2timeseries( img, mask, boldMat )
sptl    = sqrt( sum( antsGetSpacing(img)[1:3]^2  ))
img     = smoothImage(img, c(rep(sptl,3),0), FWHM=TRUE )
boldMat = timeseries2matrix(img, mask)

## ----smoothplot,message=FALSE,warnings=FALSE, echo=FALSE, fig.width=7, fig.height=5----
ctxMeanSmoothed = rowMeans(boldMat[,ctxVox])
if ( haveBadTimes ) ctxMeanSmoothed[badtimes] = NA

if ( verbose )
  {
  freq.dat =  data.frame( Time=rep(1:nTimes,3) )
  freq.dat$Values = c(ctxMeanSpline, ctxMeanFiltered, ctxMeanSmoothed)
  freq.dat$Type = c(rep("Original",nTimes), rep("Filtered",nTimes), rep("Smoothed",nTimes))
  freq.dat$Data = freq.dat$Type
  freq.dat$Data[badtimes] = "Interpolated"
  freq.dat$Type = factor(freq.dat$Type, levels=c("Original", "Filtered", "Smoothed"))
  freq.dat$Data = factor(freq.dat$Data, levels=c("Original", "Interpolated", "Filtered", "Smoothed"))
  freqPlot = ggplot2::ggplot(freq.dat, ggplot2::aes(x=Time, y=Values, group=Type, colour=Data))
  freqPlot = freqPlot + ggplot2::geom_line(size=0.5)
  freqPlot = freqPlot + ggplot2::theme(text=ggplot2::element_text(size=10), legend.position="top")
  freqPlot = freqPlot + ggplot2::ggtitle("Effect of bandpass filtering & spatial smoothing")
  freqPlot = freqPlot + ggplot2::facet_grid(Type ~ ., scales="free" )
  print(freqPlot)
  }

## ----networklabels,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5----
data("powers_areal_mni_itk", package = "ANTsR", envir = environment())
pts = antsApplyTransformsToPoints( 3, powers_areal_mni_itk, transformlist = mni2boldmapsInv )
pts[ , 4:ncol(pts) ] = powers_areal_mni_itk[ , 4:ncol(pts) ]
labelImg = mask*0
nPts = dim(pts)[1]
rad = 5
n = ceiling(rad / antsGetSpacing(mask))

for ( r in 1:nPts) {
  pt = as.numeric(c(pts$x[r], pts$y[r], pts$z[r] ))
  idx = antsTransformPhysicalPointToIndex(mask,pt)

  for ( i in c(-n[1]:n[1]) ) {
    for (j in c(-n[2]:n[2])) {
      for (k in c(-n[3]:n[3])) {
        local = idx + c(i,j,k)
        localpt = antsTransformIndexToPhysicalPoint(mask,local)
        dist = sqrt( sum( (localpt-pt)*(localpt-pt) ))
        inImage = ( prod(idx <= dim(mask))==1) && ( length(which(idx<1)) == 0 )
        if ( (dist <= rad) && ( inImage == TRUE ) ) {
          rlocal = round( local )
          labelImg[ rlocal[1], rlocal[2], rlocal[3] ] = pts$ROI[r]
         }
        }
      }
    }
  }
if ( verbose )
  plot( meanbold, labelImg, axis=3, nslices=30, ncolumns=10,
        window.overlay = c( 1, max(labelImg) ) )

## ----roimeans,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5----
labelMask = labelImg*1
labelMask[labelMask > 0] = 1
labelMask[mask == 0] = 0
labelVox = which(subset(labelMask, mask > 0)==1)

labeledBoldMat = boldMat[goodtimes,labelVox]
labels = labelImg[labelMask > 0]

nLabels = max(labels)
roiMat = matrix(0, nrow=dim(labeledBoldMat)[1], ncol=nLabels)
for ( i in c(1:nLabels) ) {
  if (length(which(labels==i)) > 1 ) {
    roiMat[,i] = rowMeans(labeledBoldMat[,(labels==i)])
  }
}
nActualTimes = dim(roiMat)[1]

## ----roiplot,message=FALSE,warnings=FALSE, fig.width=7, fig.height=10, echo=FALSE----
if ( verbose )
  {
  plotMat = roiMat + min(roiMat)
  plotMat = plotMat / max(plotMat)

  means.dat = data.frame(Time=rep( (1:nActualTimes)*tr, nLabels))
  yoffset = (rep( 1:nLabels, each=nActualTimes)-1)*0.5
  means.dat$Signal = (as.vector(plotMat)/2)+yoffset
  means.dat$ID = factor(rep( 1:nLabels, each=nActualTimes))

  meanPlot = ggplot2::ggplot(means.dat, ggplot2::aes(x=Time, y=Signal, group=ID, colour=ID))
  meanPlot = meanPlot + ggplot2::geom_line(size=0.5)
  meanPlot = meanPlot + ggplot2::theme(text=ggplot2::element_text(size=10), legend.position="none", axis.text.y=ggplot2::element_blank() )
  meanPlot = meanPlot + ggplot2::ggtitle("Mean BOLD signal in network ROIs")
  print(meanPlot)
  }

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

## ----sysmeanplot,message=FALSE,warnings=FALSE, fig.width=7, fig.height=15, echo=FALSE----
if ( verbose )
  {
  systemNickNames = c("Motor/Hand", "Motor/Mouth", "CO-Task", "Auditory", "Default", "Memory", "Visual", "FP-Task", "Salience", "Subcortical", "V Attention", "D Attention", "Cerebellar", "Uncertain" )
  lut = list("Motor/Hand"="cyan3", "Motor/Mouth"="orange", "CO-Task"="purple", "Auditory"="pink2", "Default"="red", "Memory"="gray50", "Visual"="blue", "FP-Task"="yellow2", "Salience"="black", "Subcortical"="chocolate4", "V Attention"="aquamarine4", "D Attention"="green", "Cerebellar"="cadetblue1", "Uncertain"="peachpuff2" )
  sys.dat = data.frame(Time=rep( (1:nActualTimes)*tr, nSystems))
  sys.dat$Signal = as.vector(sysMatMean)
  sys.dat$System = factor( rep( systemNickNames, foreach=nActualTimes), levels=systemNickNames)
  sys.dat$Lower = as.vector(sysMatMean) - as.vector(sysMatSD)
  sys.dat$Upper = as.vector(sysMatMean) + as.vector(sysMatSD)
  sysPlot = ggplot2::ggplot(sys.dat)
  sysPlot = sysPlot + ggplot2::geom_line(ggplot2::aes(x=Time, y=Signal, group=System), size=0.5)
  sysPlot = sysPlot + ggplot2::geom_ribbon(  ggplot2::aes( x=Time, ymin=Lower, ymax=Upper, alpha=0.05, fill=System ) )
  sysPlot = sysPlot + ggplot2::scale_fill_manual( values = lut, na.value="gray80", name="System", breaks=systemNickNames, drop=FALSE)
  sysPlot = sysPlot + ggplot2::theme(text=ggplot2::element_text(size=10), legend.position="none")
  sysPlot = sysPlot + ggplot2::ggtitle("Mean BOLD signal in systems")
  sysPlot = sysPlot + ggplot2::facet_grid(System ~ ., scales="free" )
  print(sysPlot)
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
# mygraph = makeGraph( connMat, 0.25 )


## ----adjacency,message=FALSE,warnings=FALSE, fig.width=5, fig.height=5----

density = 0.1
nEdges = length(upper.tri(connMat))*density
thresh = sort( connMat[upper.tri(connMat)], decreasing=T)[nEdges]
adj = 1*(connMat >= thresh)

bingraph = graph.adjacency(adj, mode="undirected", weighted=NULL)
components = clusters(bingraph)
maxID = which(components$csize == max(components$csize))[1]

adj[components$membership!=maxID,] = 0
adj[,components$membership!=maxID] = 0
bingraph = graph.adjacency(adj, mode="undirected", weighted=NULL)

if ( verbose ) invisible(plot(as.antsImage(adj)))


## ----adjacencyplot,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5, echo=FALSE----
if ( verbose )
  {
  pts$SystemName = factor(pts$SystemName, levels=c("Sensory/Somatomotor Hand", "Sensory/Somatomotor Mouth", "Cingulo-opercular Task Control", "Auditory", "Default Mode", "Memory Retrieval", "Visual", "Fronto-parietal Task Control", "Salience", "Subcortical", "Ventral Attention", "Dorsal Attention", "Cerebellar", "Uncertain"))
  graph = graph.adjacency( adj, mode="directed", weighted=NULL )
  igraph::V(graph)$name = pts$ROI
  igraph::V(graph)$comm = pts$SystemName
  igraph::V(graph)$degree = igraph::degree(graph)

  systems = levels(pts$SystemName)
  systemNames = as.character(systems)

  node_list <- get.data.frame(graph, what = "vertices")
  edge_list <- get.data.frame(graph, what = "edges") %>%
    inner_join(node_list %>% select(name, comm), by = c("from" = "name")) %>%
    inner_join(node_list %>% select(name, comm), by = c("to" = "name")) %>%
    dplyr::mutate(group = ifelse(comm.x == comm.y, comm.x, NA) %>% factor())

  all_nodes <- sort(node_list$name)
  plot_data <- edge_list %>% dplyr::mutate(
          to = factor(to, levels = all_nodes),
          from = factor(from, levels = all_nodes))

  name_order <- (node_list %>% arrange(comm))$name
  plot_data <- edge_list %>% dplyr::mutate(
          to = factor(to, levels = name_order),
          from = factor(from, levels = name_order))

  plot_data$group = as.integer(plot_data$group)
  for ( i in 1:length(systems) ) { plot_data$group[ which( plot_data$group == i) ] = as.character( systems[i] ) }

  lut = c("Sensory/Somatomotor Hand"="cyan3", "Sensory/Somatomotor Mouth"="orange", "Cingulo-opercular Task Control"="purple", "Auditory" = "pink2", "Default Mode"="red", "Memory Retrieval"="gray50", "Visual"="blue", "Fronto-parietal Task Control"="yellow2", "Salience"="black", "Subcortical"="chocolate4", "Ventral Attention"="aquamarine4", "Dorsal Attention"="green", "Cerebellar"="cadetblue1", "Uncertain"="peachpuff2" )

  adjplot = ggplot2::ggplot( plot_data,
    ggplot2::aes(x = from, y = to, fill = group)) +
    ggplot2::geom_raster() + ggplot2::theme_bw() + ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::scale_y_discrete(drop = FALSE) +
    ggplot2::theme( axis.title=ggplot2::element_blank(),
    axis.ticks=ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    aspect.ratio = 1 ) +
    ggplot2::scale_fill_manual( values = lut, na.value="gray80", name="System",  breaks=systemNames, drop=FALSE )

  print(adjplot)
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
edges = igraph::get.edges( graph, E(graph) )
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
deg = degree(graph)
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

if ( verbose )
  {
  cnodePlot = ggplot2::ggplot(cnode.dat, ggplot2::aes(x=Node, y=Value, group=Metric, fill=Metric, colour=Metric))
  cnodePlot = cnodePlot + ggplot2::geom_point()
  cnodePlot = cnodePlot + ggplot2::theme(text=ggplot2::element_text(size=10), legend.position="none")
  cnodePlot = cnodePlot + ggplot2::ggtitle("Node metrics")
  cnodePlot = cnodePlot + ggplot2::facet_grid(Metric~ ., scales="free")
  invisible(print(cnodePlot))
  }

geff<-1/(igraph::tshortest.paths(graph))
geff[!is.finite(geff)]<-NA
geff<-mean(geff,na.rm=TRUE)
cc = igraph::transitivity(graph)
refSignal = sysMatMean[ , systemNames == "Default Mode"  ]

# get priors for different networks
if ( ! exists( "networkPriors" ) )
  {
  networkPriors = getANTsRData( "fmrinetworks" )
  ilist = networkPriors$images
  for ( i in 1:length(ilist) )
    ilist[[i]] = antsApplyTransforms( meanbold, ilist[[i]], mni2boldmaps )
  }
pr = imageListToMatrix( ilist, mask )
refSignal = ( boldMat %*% t(pr) )
networkDf = data.frame( ROI=refSignal[goodtimes,1],  nuisance[goodtimes,] )
mdl = lm( boldMat[ goodtimes, ] ~ . , data=networkDf )
bmdl = bigLMStats( mdl, 1.e-4 )
betas = bmdl$beta.t["ROI",]
betasI = makeImage( mask, betas )
loth = quantile(  betas, probs=0.8 )
if ( verbose )
  plot( meanbold, betasI, axis=3, nslices=30, ncolumns=10,
        window.overlay = c( loth, max(betas) ) )
connMatNodes = NA
if ( is.na( structuralNodes )  )
  {
  dmnnodes = antsImageRead( getANTsRData("mnidfn") )
  dmnnodes = antsApplyTransforms( meanbold, dmnnodes, mni2boldmaps,
                                    interpolator = 'NearestNeighbor' )
  ulabs = sort( unique( dmnnodes[ mask == 1 & dmnnodes > 0 ] ) )
  dmnlist = list()
  for ( i in 1:length( ulabs ) )
    dmnlist[[i]] = thresholdImage(  dmnnodes, ulabs[i], ulabs[i]  )
  dmnpr = imageListToMatrix( dmnlist, mask )
  dmnref = ( boldMat %*% t(dmnpr) )
  connMatNodes = cor( dmnref )
  } else {
    dmnnodes = antsApplyTransforms(
      meanbold, structuralNodes, boldmap$fwdtransforms,
      interpolator = 'NearestNeighbor' )
    ulabs = sort( unique( dmnnodes[ mask == 1 & dmnnodes > 0 ] ) )
    dmnlist = list()
    for ( i in 1:length( ulabs ) )
      dmnlist[[i]] = thresholdImage(  dmnnodes, ulabs[i], ulabs[i]  )
    dmnpr = imageListToMatrix( dmnlist, mask )
    dmnref = ( boldMat %*% t(dmnpr) )
    connMatNodes = cor( dmnref )
    }
  connMatNodesPartialCorr = NA
  if ( usePkg( "corpcor" ) )
    connMatNodesPartialCorr = corpcor::cor2pcor( connMatNodes ) # partial correlation
return(
    list(
        boldMat       = boldMat,
        boldMask      = mask,
        nuisance      = nuisance,
        dmnBetas      = betasI,
        connMatPowers = connMat,
        connMatNodes  = connMatNodes,
        connMatNodesPartialCorr = connMatNodesPartialCorr,
        FD            = moco$fd$MeanDisplacement,
        goodtimes     = goodtimes
        )
      )
}
