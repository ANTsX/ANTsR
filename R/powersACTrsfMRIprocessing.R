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
#' @param templateMap antsRegistration output mapping template space (as moving) to struturalImage (fixed).
#' @param smoothingSigmas 4-vector defining amount of smoothing in FWHM units
#' @param extraRuns a list containing additional BOLD images (runs) to be merged with the first image
#' @param verbose enables visualization as well as commentary.
#' @return outputs a list containing:
#' \itemize{
#'   \item{boldMat: }{Matrix of filtered BOLD data.}
#'   \item{boldMask: }{BOLD mask.}
#'   \item{nuisance: }{Nuisance variables.}
#'   \item{dmnBetas: }{Default mode network beta map.}
#'   \item{connMatPowers: }{Powers nodes connectivity matrix. Assumes template maps are to MNI space.}
#'   \item{connMatNodes: }{User provided nodal system connectivity matrix.}
#'   \item{connMatNodesPartialCorr: }{TUser provided nodal system partial correlation matrix.}
#'   \item{FD: }{mean framewise displacement.}
#'   \item{DVARS: }{signal variability.}
#'   \item{goodtimes: }{good time points.}
#'   \item{seg2bold: }{strutural segmentation in bold space.}
#'   \item{nodes2bold: }{strutural nodes in bold space.}
#'   \item{mapsToTemplate: }{invertible maps from bold to template space.}
#'   \item{networkPriors2Bold: }{standard network priors in BOLD space.}
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
#' @export powersACTrsfMRIprocessing
powersACTrsfMRIprocessing <- function( img,
  fdthresh=0.2,
  repeatMotionEst = 1,
  freqLimits = c( 0.008, 0.09 ),
  nCompCor = 4,
  structuralImage = NA,
  structuralSeg = NA,
  structuralNodes = NA,
  templateMap  = NA,
  smoothingSigmas = NA,
  extraRuns = NA,
  verbose = FALSE )
{
powers_areal_mni_itk <- NULL
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
# start with basic mean bold image
meanbold = getAverageOfTimeSeries( img )
mask = getMask( meanbold )
# Find first steady state timepoint
tr = antsGetSpacing(img)[4]
steady = floor(10.0 / tr) + 1

# Global signal before cropping (save for visualization)
origmean = apply.antsImage(img, c(1,2,3), mean)
fullmean = rowMeans(timeseries2matrix(img, mask))
allTimes = dim(img)[4]
runNuis  = NA
# Eliminate non steady-state timepoints
img = cropIndices(img, c(1,1,1,steady), dim(img) )

if ( ! all( is.na( extraRuns ) ) )
  {
  if ( class( extraRuns )[[1]] != "list"  )
    stop("extraRuns must be a list of antsImages.")
  runNuis = rep(1, dim(img)[4] )
  for ( i in 1:length( extraRuns ) )
    {
    timg = extraRuns[[i]]
    allTimes = allTimes + dim( timg )[4]
    timg = cropIndices( timg, c(1,1,1,steady), dim(timg) )
    extraRuns[[i]] = timg
    runNuis = c( runNuis, rep(i+1, dim(timg)[4] ) )
    }
  }


## ----moco,message=FALSE,warnings=FALSE, fig.width=7, fig.height=3--------
mocoTxType = "Rigid"
for ( i in 1:repeatMotionEst )
  {
  moco <- antsMotionCalculation( img, fixed=meanbold, txtype=mocoTxType )
  }
if ( repeatMotionEst < 1 )
  moco = antsMotionCalculation( img, fixed=meanbold, txtype=mocoTxType, moreaccurate = 0 )

meanbold = apply.antsImage( moco$moco_img, c(1,2,3), mean)

# now add any additional runs and merge moco results
if ( ! all( is.na( extraRuns ) ) )
  {
  for ( i in 1:length( extraRuns ) )
    {
    timg = extraRuns[[i]]
    # do a more accurate registration for this stage b/c it's a different run
    mocoTemp <- antsMotionCalculation( timg, fixed=meanbold, txtype=mocoTxType, moreaccurate=2 )
    if ( verbose ) print("merge corrected image ( and tsDisplacement? )")
    if ( usePkg("abind") )
      {
      ttmo = as.array( moco$moco_img )
      ttmo = abind::abind( ttmo, as.array( mocoTemp$moco_img ) )
      moco$moco_img = antsCopyImageInfo( moco$moco_img, as.antsImage(ttmo) )
      rm( ttmo )
      moco$tsDisplacement = NA
      } else stop( "need abind package for the extraRuns feature")
    if ( verbose ) print("merge parameters, fd and dvars")
    moco$moco_params = rbind( moco$moco_params, mocoTemp$moco_params )
    moco$fd = rbind( moco$fd, mocoTemp$fd )
    moco$dvars = c( moco$dvars, mocoTemp$dvars )
    }
  }

## ----mocoimg,message=FALSE,warnings=FALSE, fig.width=7, fig.height=3, echo=FALSE----
if ( verbose )
  invisible( plot( moco$moco_avg_img, axis=3, slices=1:30, ncolumns=10 ) )

#
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
  boldmap = antsRegistration( meanbold * mask, t1brain,
    typeofTransform='SyNBoldAff', verbose=verbose )

notemplateMap = FALSE
if ( any( is.na( templateMap ) ) )
  {
  notemplateMap = TRUE
  mni = antsImageRead( getANTsRData( "mni" ) )
  templateMap = antsRegistration( t1brain, mni, typeofTransform='SyN',
    verbose=verbose )
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
reg_params <- as.matrix( moco$moco_params[,3:8] ) # FIXME this is bad coding
dvars <- computeDVARS( timeseries2matrix( moco$moco_img, mask ) )

## ----badtimes,message=FALSE,warnings=FALSE, fig.width=7, fig.height=3----
goodtimes = (1:nrow( moco$moco_img ))
badtimes = which(moco$fd$MeanDisplacement > fdthresh )
haveBadTimes = FALSE
if ( length( badtimes ) > 0 )
  {
  badtimes = sort(c(badtimes, badtimes+1))
  goodtimes = goodtimes[-badtimes]
  haveBadTimes = TRUE
  } else badtimes = NA

## ----detrend,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5-----
global_moco <- rowMeans( timeseries2matrix( moco$moco_img, mask) )
boldMat = timeseries2matrix( moco$moco_img, mask )
boldMat[goodtimes,] = pracma::detrend(boldMat[goodtimes,])
if ( haveBadTimes ) boldMat[badtimes,] = NA

global_moco_detrend = rowMeans(boldMat)
if ( haveBadTimes ) global_moco[badtimes] = NA

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
if ( ! all( is.na( runNuis ) ) )
  nuisance = cbind( nuisance, runs=factor(runNuis) )

boldMat[goodtimes,] <- residuals( lm( boldMat[goodtimes,] ~ nuisance[goodtimes,] ) )

## ----regressionplot,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5, echo=FALSE----
ctxMeanRegressed = rowMeans(boldMat[,ctxVox])


## ----frequency,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5----
if ( haveBadTimes )
  {
  for ( v in c(1:nVox) ) {
    boldMat[badtimes,v]=spline( c(1:nTimes)[goodtimes], boldMat[goodtimes,v],
      method='natural', xout=badtimes )$y
    }
  # FIXME - may not want to do this ie may want to avoid using badtimes
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
img     = matrix2timeseries( moco$moco_img, mask, boldMat )
if ( any( is.na( smoothingSigmas ) ) )
  {
  sptl    = sqrt( sum( antsGetSpacing(img)[1:3]^2  )) * 1.5
  smoothingSigmas = c( sptl, sptl, sptl, 1.0 )
  }
img     = smoothImage(img, smoothingSigmas, FWHM=TRUE )
boldMat = timeseries2matrix(img, mask)

## ----smoothplot,message=FALSE,warnings=FALSE, echo=FALSE, fig.width=7, fig.height=5----
ctxMeanSmoothed = rowMeans(boldMat[,ctxVox])
if ( haveBadTimes ) ctxMeanSmoothed[badtimes] = NA


## ----networklabels,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5----
data( "powers_areal_mni_itk", package = "ANTsR", envir = environment() )
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
nEdges = length(upper.tri(connMat))*density
thresh = sort( connMat[upper.tri(connMat)], decreasing=T)[nEdges]
adj = 1*(connMat >= thresh)

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

#  node_list <- igraph::get.data.frame(graph, what = "vertices")
#  edge_list <- igraph::get.data.frame(graph, what = "edges") %>%
#    dplyr::inner_join(node_list %>% select(name, comm), by = c("from" = "name")) %>%
#    dplyr::inner_join(node_list %>% select(name, comm), by = c("to" = "name")) %>%
#    dplyr::mutate(group = ifelse(comm.x == comm.y, comm.x, NA) %>% factor())

#  all_nodes <- sort(node_list$name)
#  plot_data <- edge_list %>% dplyr::mutate(
#          to = factor(to, levels = all_nodes),
#          from = factor(from, levels = all_nodes))

#  name_order <- (node_list %>% arrange(comm))$name
#  plot_data <- edge_list %>% dplyr::mutate(
#          to = factor(to, levels = name_order),
#          from = factor(from, levels = name_order))

#  plot_data$group = as.integer(plot_data$group)
#  for ( i in 1:length(systems) ) { plot_data$group[ which( plot_data$group == i) ] = as.character( systems[i] ) }

#  lut = c("Sensory/Somatomotor Hand"="cyan3", "Sensory/Somatomotor Mouth"="orange", "Cingulo-opercular Task Control"="purple", "Auditory" = "pink2", "Default Mode"="red", "Memory Retrieval"="gray50", "Visual"="blue", "Fronto-parietal Task Control"="yellow2", "Salience"="black", "Subcortical"="chocolate4", "Ventral Attention"="aquamarine4", "Dorsal Attention"="green", "Cerebellar"="cadetblue1", "Uncertain"="peachpuff2" )

#  adjplot = ggplot2::ggplot( plot_data,
#    ggplot2::aes(x = from, y = to, fill = group)) +
#    ggplot2::geom_raster() + ggplot2::theme_bw() + ggplot2::scale_x_discrete(drop = FALSE) +
#    ggplot2::scale_y_discrete(drop = FALSE) +
#    ggplot2::theme( axis.title=ggplot2::element_blank(),
#    axis.ticks=ggplot2::element_blank(),
#    axis.text = ggplot2::element_blank(),
#    aspect.ratio = 1 ) +
#    ggplot2::scale_fill_manual( values = lut, na.value="gray80", name="System",  breaks=systemNames, drop=FALSE )

#  print(adjplot)
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
refSignal = sysMatMean[ , systemNames == "Default Mode"  ]

# get priors for different networks
networkPriors2Bold=NA
if ( ! exists( "networkPriors" ) & notemplateMap )
  {
  networkPriors = getANTsRData( "fmrinetworks" )
  networkPriors2Bold = networkPriors$images
  for ( i in 1:length(networkPriors2Bold) )
    networkPriors2Bold[[i]] = antsApplyTransforms( meanbold,
      networkPriors2Bold[[i]], mni2boldmaps )
  pr = imageListToMatrix( networkPriors2Bold, mask )
  refSignal = ( boldMat %*% t(pr) )
  networkDf = data.frame( ROI=refSignal[goodtimes,1],  nuisance[goodtimes,] )
  if ( TRUE )
    {
    dnz<-aslDenoiseR( boldMat[goodtimes,], refSignal[goodtimes,1],
      covariates=nuisance[goodtimes,],
      selectionthresh=0.1, maxnoisepreds=c(2:12), verbose=verbose,
      polydegree='loess', crossvalidationgroups=6 )
    networkDf = data.frame( ROI=refSignal[goodtimes,1],  dnz$covariates , dnz$noiseu )
    }
  mdl = lm( boldMat[goodtimes,] ~ . , data=networkDf )
  bmdl = bigLMStats( mdl, 1.e-4 )
  betas = bmdl$beta.t["ROI",]
  betasI = makeImage( mask, betas )
  loth = quantile(  betas, probs=0.8 )
  if ( verbose )
    plot( meanbold, betasI, axis=3, nslices=30, ncolumns=10,
          window.overlay = c( loth, max(betas) ) )
  } else betasI = NA
connMatNodes = NA
if ( is.na( structuralNodes ) & notemplateMap )
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
  }

  if ( ! is.na( structuralNodes ) )
    {
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

  concatenatedMaps =
    list( toBold =  mni2boldmaps, toBoldInversion=rep(FALSE,4),
          toTemplate =  mni2boldmapsInv,
          toTemplateInversion = c(TRUE,FALSE,TRUE,FALSE) )

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
        goodtimes     = goodtimes,
        seg2bold      = seg2bold,
        nodes2bold    = dmnnodes,
        mapsToTemplate = concatenatedMaps,
        networkPriors2Bold = networkPriors2Bold
        )
      )
}
