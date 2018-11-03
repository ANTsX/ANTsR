## ---- echo = FALSE, message = FALSE, include = FALSE---------------------
library( knitr )
library( rmarkdown )
knitr::opts_chunk$set(collapse = T, comment = "#>")
library(ANTsR)
library(ggplot2)
library(igraph)
library(pracma)
library(dplyr)


## ----data ,message=FALSE,warnings=FALSE,eval=FALSE-----------------------
#  library(mFilter)
#  fdthresh = 0.1 # threshold for throwing out bad data, typically between 0.1 and 0.5
#  # based on ACT output or sample data
#  id = "BAvants"
#  pre = paste("~/rsfTest/",id,"/",sep='')
#  fn = path.expand( paste( pre, "rsfMRI0/",id,"_rsfMRI0.nii.gz", sep='' ) )
#  testData = 1
#  if ( file.exists( fn ) )
#    {
#    fdthresh = 0.2 # threshold for throwing out bad data, typically between 0.1 and 0.5
#    testData = 3
#    imgIn  = antsImageRead( fn  )
#    meanbold = getAverageOfTimeSeries( imgIn )
#    mask = getMask( meanbold )
#    sfn = path.expand( paste( pre, "/act/", id,"_STRUCTURAL_BrainSegmentation.nii.gz", sep='' ) )
#    seg = antsImageRead( sfn   )
#    t1fn = path.expand( paste( pre, "/act/", id,"_STRUCTURAL_BrainSegmentation0N4.nii.gz", sep='' ) )
#    t1 = antsImageRead( t1fn   )
#    } else { # use sample data
#    imgIn = antsImageRead(getANTsRData("rsbold"))
#    meanbold = getAverageOfTimeSeries( imgIn )
#    mask = antsImageRead(getANTsRData("rsboldmask"))
#    seg = antsImageRead(getANTsRData("rsboldseg"))
#    t1 = antsImageClone( seg )
#    t1[ t1 > 3 ] = 2
#    }

## ----mapT1toMNI,eval=FALSE-----------------------------------------------
#  t1brain = t1 * thresholdImage( seg, 1, 6 )
#  if ( ! exists("boldmap") )
#    boldmap = antsRegistration( meanbold, t1brain, typeofTransform='SyNBoldAff' )
#  if ( ! exists("mnimap") )
#    {
#    mni = antsImageRead( getANTsRData( "mni" ) )
#    mnimap = antsRegistration( t1brain, mni, typeofTransform='SyN' )
#    }
#  mni2boldmaps = c( boldmap$fwdtransforms, mnimap$fwdtransforms )
#  mni2boldmapsInv = c(  mnimap$invtransforms , boldmap$invtransforms )
#  mni2bold = antsApplyTransforms( meanbold, mni, mni2boldmaps )
#  seg2bold = antsApplyTransforms( meanbold, seg, boldmap$fwdtransforms, interpolator = "NearestNeighbor" )
#  plot( meanbold , boldmap$warpedmovout %>% iMath("Canny", 10, 1, 1) )
#  plot( meanbold , mni2bold %>% iMath("Canny", 10, 1, 1) )
#  plot( meanbold , maskImage( seg2bold, seg2bold, 2 ) )

## ----steadystate ,message=FALSE,warnings=FALSE,eval=FALSE----------------
#  # Find first steady state timepoint
#  tr = antsGetSpacing(imgIn)[4]
#  steady = floor(10.0 / tr) + 1
#  
#  # Global signal before cropping (save for visualization)
#  origmean = getAverageOfTimeSeries( imgIn )
#  fullmean = rowMeans(timeseries2matrix(imgIn, mask))
#  allTimes = dim(imgIn)[4]
#  
#  # Eliminate non steady-state timepoints
#  img = cropIndices(imgIn, c(1,1,1,steady), dim(imgIn) )

## ----ssplot,message=FALSE,warnings=FALSE, fig.width=7, fig.height=3, echo=FALSE,eval=FALSE----
#  # exclusion area
#  noss.data = data.frame(Start=0)
#  noss.data$Stop = (steady-1)*tr
#  noss.rect.aes = aes(xmin=Start,xmax=Stop,ymin=-Inf,ymax=Inf,fill="pink",alpha=0.2)
#  
#  # mean signal in brain
#  ss.dat <- data.frame(Time=rep(1:allTimes)*tr)
#  ss.dat$Values = fullmean
#  
#  ssPlot <- ggplot(ss.dat)
#    ssPlot <- ssPlot + geom_line(aes(x=Time, y=Values), size=0.5)
#    ssPlot <- ssPlot + geom_rect(data=noss.data, noss.rect.aes)
#    ssPlot <- ssPlot + theme(text=element_text(size=10), legend.position="none")
#    ssPlot <- ssPlot + ggtitle("Exclude points previous to magnetization steady state")
#  print(ssPlot)

## ----rawplot,message=FALSE,warnings=FALSE, fig.width=7, fig.height=3, echo=FALSE,eval=FALSE----
#  invisible(plot(origmean, axis=3, slices=1:30, ncolumns=10))

## ----moco,message=FALSE,warnings=FALSE, fig.width=7, fig.height=3,eval=FALSE----
#  if ( ! exists("moco") )
#    {
#    meanbold <- getAverageOfTimeSeries( img )
#    for ( i in 1:testData )
#      {
#      moco <- antsrMotionCalculation( img, fixed=meanbold, typeofTransform="Rigid" )
#      meanbold = getAverageOfTimeSeries( moco$moco_img )
#      }
#    }

## ----mocoimg,message=FALSE,warnings=FALSE, fig.width=7, fig.height=3, echo=FALSE,eval=FALSE----
#  invisible(plot(moco$moco_avg_img, axis=3, slices=1:30, ncolumns=10))

## ----mocomatrix,message=FALSE,warnings=FALSE, fig.width=7, fig.height=3,eval=FALSE----
#  nVox = length(which(as.array(mask)==1))
#  vox = sample(1:nVox, 1000)
#  invisible(plot(as.antsImage( t(timeseries2matrix(img,mask)[,vox]))))
#  invisible(plot(as.antsImage( t(timeseries2matrix(moco$moco_img,mask)[,vox]))))

## ----mocoplots,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5, echo=FALSE,eval=FALSE----
#  # extract just the transform parameters
#  reg_params <- as.matrix(moco$moco_params )
#  
#  nTimes = dim(reg_params)[1]
#  orderedBreaks = c("Framewise", "X", "Y", "Z", "Pitch", "Roll", "Yaw" )
#  moco.dat <- data.frame(Time=rep(1:nTimes, 7)*tr)
#  moco.dat$Values = c( as.vector(reg_params), moco$fd$MeanDisplacement )
#  moco.dat$Category = c( rep("Angle", 3*nTimes), rep("Displacement", 4*nTimes) )
#  moco.dat$Type = rep(c("Pitch", "Roll", "Yaw","X", "Y", "Z", "Framewise"), each=nTimes)
#  regPlot <- ggplot(moco.dat, aes(x=Time, y=Values, group=Type, colour=Type) )
#    regPlot <- regPlot + geom_line(size=0.5)
#    regPlot <- regPlot + theme(text=element_text(size=10), legend.position="top")
#    regPlot <- regPlot + ggtitle("Motion correction parameters")
#    regPlot <- regPlot + facet_grid(Category ~ ., scales="free" )
#    regPlot <- regPlot + scale_color_discrete(breaks=orderedBreaks)
#  print(regPlot)

## ----dvar,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5, echo=FALSE,eval=FALSE----
#  scaling <- 1000.0 / mean(moco$moco_avg_img)# [ moco$moco_mask > 0 ])
#  dvars <- scaling * computeDVARS(timeseries2matrix(moco$moco_img, mask))
#  orig_dvars <- scaling * computeDVARS(timeseries2matrix(img, mask))

## ----dvarplots,message=FALSE,warnings=FALSE, fig.width=7, fig.height=3, echo=FALSE,eval=FALSE----
#  dvarType <- c(rep("Original",length(orig_dvars)), rep("Moco",length(dvars)) )
#  dvarTime <- c(1:length(orig_dvars), 1:length(dvars))*tr
#  dvar.data <- data.frame(DVARS=c(orig_dvars, dvars), Type=dvarType, Time=dvarTime)
#  dvarType = factor(dvarType, levels=c("Original", "Moco"))
#  
#  dvarPlot <- ggplot(dvar.data, aes(x=Time, y=DVARS, group=Type, colour=Type) )
#  dvarPlot <- dvarPlot + geom_line(size=0.5)
#  dvarPlot <- dvarPlot + theme(text=element_text(size=10), legend.position="top")
#  dvarPlot <- dvarPlot + ggtitle("DVARS: pre and post motion correction")
#  dvarPlot <- dvarPlot + scale_colour_discrete(breaks=c("Original", "Moco"))
#  print(dvarPlot)

## ----badtimes,message=FALSE,warnings=FALSE, fig.width=7, fig.height=3,eval=FALSE----
#  fdthresh = quantile( moco$fd$MeanDisplacement, 0.98 )
#  badtimes = which(moco$fd$MeanDisplacement > fdthresh )
#  if ( length( badtimes ) == 0 ) badtimes = c(1)
#  badtimes = sort(c(badtimes, badtimes+1))
#  goodtimes = (1:nTimes)[-badtimes]

## ----badtimesplot,message=FALSE,warnings=FALSE, fig.width=7, fig.height=3, echo=FALSE,eval=FALSE----
#  badstarts = which(moco$fd$MeanDisplacement > fdthresh )
#  if ( length( badstarts ) == 0 ) badstarts = c(1)
#  
#  bad.data = data.frame(Time=(1:nTimes)*tr)
#  bad.data$FD = moco$fd$MeanDisplacement
#  
#  bad.data.rect = data.frame(Start=badstarts*tr)
#  bad.data.rect$Stop = (badstarts+1)*tr
#  rect.aes = aes(xmin=Start,xmax=Stop,ymin=-Inf,ymax=Inf,fill="pink",alpha=0.2)
#  
#  badPlot <- ggplot(bad.data) + geom_line(aes(x=Time, y=FD))
#  badPlot <- badPlot + geom_hline( yintercept=fdthresh, linetype="dashed", alpha=0.5 )
#  badPlot <- badPlot + theme(text=element_text(size=10), legend.position="none")
#  badPlot <- badPlot + ggtitle("Bad timepoints")
#  badPlot <- badPlot + geom_rect(data=bad.data.rect, rect.aes)
#  print(badPlot)

## ----detrend,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5,eval=FALSE----
#  global_pre <- rowMeans(timeseries2matrix(img, mask))
#  global_moco <- rowMeans(timeseries2matrix(moco$moco_img, mask))
#  
#  boldMat = timeseries2matrix(moco$moco_img, mask)
#  boldMat[goodtimes,] = detrend(boldMat[goodtimes,])
#  boldMat[badtimes,] = NA
#  
#  global_moco_detrend = rowMeans(boldMat)
#  global_pre[badtimes] = NA
#  global_moco[badtimes] = NA

## ----detrendplot,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5, echo=FALSE,eval=FALSE----
#  trend.dat = data.frame( Time=rep(1:nTimes,3) )
#  trendType = c( rep("Original", nTimes), rep("Motion-corrected",nTimes) )
#  trendType = c(trendType, rep("Moco & Detrended",nTimes) )
#  trendNames = c(rep("Original",nTimes*2), rep("Detrended", nTimes))
#  trendCategory = factor(trendNames, levels=c("Original", "Detrended"))
#  trend.dat$Signal = c(global_pre, global_moco, global_moco_detrend)
#  trend.dat$Type = trendType
#  trend.dat$Category = trendCategory
#  trendPlot <- ggplot(trend.dat, aes(x=Time, y=Signal, group=Type, colour=Type) )
#  trendPlot <- trendPlot + geom_line(size=0.5)
#  trendPlot <- trendPlot + theme(text=element_text(size=10), legend.position="top")
#  trendPlot <- trendPlot + facet_grid(Category ~ ., scales="free" )
#  trendPlot <- trendPlot + ggtitle("Detrending the time-series")
#  print(trendPlot)

## ----nuisance,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5,eval=FALSE----
#  # white matter is labeled as 3
#  wmMask = seg2bold*1*mask
#  wmMask[ wmMask != 3] = 0
#  wmMask[ wmMask == 3 ] = 1
#  wmMask = iMath( wmMask, "ME", 1)
#  wmVox = which(subset(wmMask, mask > 0 )==1)
#  wmMean = rowMeans(boldMat[,wmVox])
#  
#  # CSF is labeled as 1
#  csfMask = seg2bold*1
#  csfMask[ csfMask != 1] = 0
#  #csfMask = iMath( csfMask, "ME", 1)
#  csfVox = which(subset(csfMask, mask > 0)==1)
#  csfMean= rowMeans(boldMat[,csfVox])
#  #csfMean = rowMeans(timeseries2matrix(detrendImg, csfMask))
#  
#  globalMean = rowMeans(boldMat)
#  ncompcor = 4
#  compcorTemp = compcor(boldMat[goodtimes,], ncompcor = ncompcor)
#  compcorNuis = matrix(0, nTimes, ncompcor )
#  compcorNuis[goodtimes, ] = compcorTemp
#  compcorNuis[badtimes, ] = NA
#  colnames( compcorNuis ) = paste("compcor",1:ncol(compcorNuis), sep='' )
#  tissueNuis = cbind(globalMean, wmMean, csfMean)
#  if ( length(badtimes) > 0 ) {
#    for ( v in c(1:dim(tissueNuis)[2]) ) {
#      tissueInterp = spline( c(1:nTimes)[goodtimes], tissueNuis[goodtimes,v],
#        method='natural', xout=badtimes )$y
#      tissueNuis[badtimes,v]=tissueInterp
#      }
#    }
#  tissueDeriv = rbind( rep(0,dim(tissueNuis)[2]), diff(tissueNuis,1) )

## ----nuissanceplot,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5, echo=FALSE,eval=FALSE----
#  tissueType = c( rep("Global", nTimes), rep("White matter",nTimes), rep("CSF",nTimes) )
#  tissueType = c(tissueType, rep("CompCor1",nTimes), rep("CompCor2",nTimes))
#  tissueType = c(tissueType, rep("CompCor3",nTimes), rep("CompCor4",nTimes) )
#  
#  tissueCategory = c(rep("Tissue", nTimes*3), rep("CompCor", nTimes*4))
#  
#  signal = c(global_moco_detrend, wmMean, csfMean, compcorNuis[,1], compcorNuis[,2])
#  signal = c(signal, compcorNuis[,3], compcorNuis[,4])
#  
#  tissue.dat = data.frame( Time=rep(1:nTimes,7) )
#  tissue.dat$Signal = signal
#  tissue.dat$Type = tissueType
#  tissue.dat$Category = tissueCategory
#  
#  tissuePlot <- ggplot(tissue.dat, aes(x=Time, y=Signal, group=Type, colour=Type) )
#  tissuePlot <- tissuePlot + geom_line(size=0.5)
#  tissuePlot <- tissuePlot + theme(text=element_text(size=10), legend.position="top")
#  tissuePlot <- tissuePlot + facet_grid(Category ~ ., scales="free" )
#  tissuePlot <- tissuePlot + ggtitle("Nuisance parameters")
#  print(tissuePlot)
#  
#  # Save mean cortex signal for later plotting
#  ctxMask = seg2bold*1
#  ctxMask[ ctxMask != 2] = 0
#  ctxMask[ ctxMask == 2 ] = 1
#  ctxVox = which(subset(ctxMask, mask > 0)==1)
#  ctxMean = rowMeans(boldMat[,ctxVox])

## ----regression,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5,eval=FALSE----
#  mocoNuis = cbind(reg_params, reg_params*reg_params)
#  mocoNuis = detrend(mocoNuis)
#  mocoDeriv = rbind( rep(0,dim(mocoNuis)[2]), diff(mocoNuis,1) )
#  
#  nuissance = cbind( mocoNuis, mocoDeriv, tissueNuis, tissueDeriv, compcorNuis )
#  
#  boldMat[goodtimes,] <- residuals( lm( boldMat[goodtimes,] ~ nuissance[goodtimes,] ) )

## ----regressionplot,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5, echo=FALSE,eval=FALSE----
#  ctxMeanRegressed = rowMeans(boldMat[,ctxVox])
#  
#  cortex.dat =  data.frame( Time=rep(1:nTimes,2) )
#  cortex.dat$Values = c(ctxMean, ctxMeanRegressed)
#  cortex.dat$Type = c(rep("Original",nTimes), rep("Regressed",nTimes))
#  cortexPlot = ggplot(cortex.dat, aes(x=Time, y=Values, group=Type, colour=Type))
#  cortexPlot = cortexPlot + geom_line(size=0.5)
#  cortexPlot = cortexPlot + theme(text=element_text(size=10), legend.position="top")
#  cortexPlot = cortexPlot + ggtitle("Effect of nuisance parameter regression")
#  cortexPlot = cortexPlot + facet_grid(Type ~ ., scales="free" )
#  print(cortexPlot)

## ----frequency,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5,eval=FALSE----
#  if ( length(badtimes) > 0 ) {
#    for ( v in c(1:nVox) ) {
#      boldMat[badtimes,v]=spline( c(1:nTimes)[goodtimes], boldMat[goodtimes,v],
#        method='natural', xout=badtimes )$y
#      }
#    }
#  
#  # save interpolated values for plotting
#  ctxMeanSpline = rowMeans(boldMat[,ctxVox])
#  
#  fboldMat <- frequencyFilterfMRI( boldMat, tr=tr, freqLo=0.009, freqHi=0.08, opt="trig" )
#  
#  # save filtered values for plotting
#  ctxMeanFiltered = rowMeans(fboldMat[,ctxVox])
#  ctxMeanFiltered[badtimes] = NA

## ----smooth,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5,eval=FALSE----
#  fimg = matrix2timeseries( img, mask, fboldMat )
#  sptl = sqrt( sum( antsGetSpacing(img)[1:3]^2  ))
#  simg = smoothImage( fimg, c(rep(sptl,3),0), FWHM=TRUE )
#  sboldMat = timeseries2matrix(simg, mask)

## ----smoothplot,message=FALSE,warnings=FALSE, echo=FALSE, fig.width=7, fig.height=5,eval=FALSE----
#  ctxMeanSmoothed = rowMeans( sboldMat[,ctxVox] )
#  ctxMeanSmoothed[badtimes] = NA
#  
#  freq.dat =  data.frame( Time=rep(1:nTimes,3) )
#  freq.dat$Values = c(ctxMeanSpline, ctxMeanFiltered, ctxMeanSmoothed)
#  freq.dat$Type = c(rep("Original",nTimes), rep("Filtered",nTimes), rep("Smoothed",nTimes))
#  freq.dat$Data = freq.dat$Type
#  freq.dat$Data[badtimes] = "Interpolated"
#  freq.dat$Type = factor(freq.dat$Type, levels=c("Original", "Filtered", "Smoothed"))
#  freq.dat$Data = factor(freq.dat$Data, levels=c("Original", "Interpolated", "Filtered", "Smoothed"))
#  freqPlot = ggplot(freq.dat, aes(x=Time, y=Values, group=Type, colour=Data))
#  freqPlot = freqPlot + geom_line(size=0.5)
#  freqPlot = freqPlot + theme(text=element_text(size=10), legend.position="top")
#  freqPlot = freqPlot + ggtitle("Effect of bandpass filtering & spatial smoothing")
#  freqPlot = freqPlot + facet_grid(Type ~ ., scales="free" )
#  print(freqPlot)

## ----networklabels,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5,eval=FALSE----
#  data(powers_areal_mni_itk)
#  pts = antsApplyTransformsToPoints( 3, powers_areal_mni_itk, transformlist = mni2boldmapsInv )
#  pts[ , 4:ncol(pts) ] = powers_areal_mni_itk[ , 4:ncol(pts) ]
#  labelImg = makePointsImage( pts, mask )
#  nPts = dim(pts)[1]
#  plot( meanbold, labelImg, axis=3, nslices=30, ncolumns=10,
#          window.overlay = c( 1, max(labelImg) ) )

## ----roimeans,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5,eval=FALSE----
#  labelMask = labelImg*1
#  labelMask[labelMask > 0] = 1
#  labelMask[mask == 0] = 0
#  labelVox = which(subset(labelMask, mask > 0)==1)
#  
#  labeledBoldMat = boldMat[goodtimes,labelVox]
#  labels = labelImg[labelMask > 0]
#  
#  nLabels = max(labels)
#  roiMat = matrix(0, nrow=dim(labeledBoldMat)[1], ncol=nLabels)
#  for ( i in c(1:nLabels) ) {
#    if (length(which(labels==i)) > 1 ) {
#      roiMat[,i] = rowMeans(labeledBoldMat[,(labels==i)])
#    }
#  }
#  nActualTimes = dim(roiMat)[1]

## ----roiplot,message=FALSE,warnings=FALSE, fig.width=7, fig.height=10, echo=FALSE,eval=FALSE----
#  plotMat = roiMat + min(roiMat)
#  plotMat = plotMat / max(plotMat)
#  
#  means.dat = data.frame(Time=rep( (1:nActualTimes)*tr, nLabels))
#  yoffset = (rep( 1:nLabels, each=nActualTimes)-1)*0.5
#  means.dat$Signal = (as.vector(plotMat)/2)+yoffset
#  means.dat$ID = factor(rep( 1:nLabels, each=nActualTimes))
#  
#  meanPlot = ggplot(means.dat, aes(x=Time, y=Signal, group=ID, colour=ID))
#  meanPlot = meanPlot + geom_line(size=0.5)
#  meanPlot = meanPlot + theme(text=element_text(size=10), legend.position="none", axis.text.y=element_blank())
#  meanPlot = meanPlot + ggtitle("Mean BOLD signal in network ROIs")
#  print(meanPlot)

## ----sysmean,message=FALSE,warnings=FALSE, fig.width=7, fig.height=10, echo=TRUE,eval=FALSE----
#  systemNames = unique(pts$SystemName)
#  
#  nSystems = length(systemNames)
#  sysMatMean = matrix(0, nrow=dim(labeledBoldMat)[1], ncol=nSystems)
#  sysMatSD = matrix(0, nrow=dim(labeledBoldMat)[1], ncol=nSystems)
#  
#  systems = pts$SystemName[labels]
#  
#  for ( i in 1:nSystems ) {
#    sys = systemNames[i]
#    sysIdx = which(systems==sys)
#    if ( length(sysIdx) > 0)
#      {
#      sysMatMean[,i] = rowMeans(labeledBoldMat[,sysIdx])
#      sysMatSD[,i] = apply(labeledBoldMat[,sysIdx], 1, sd)
#      }
#  }

## ----sysmeanplot,message=FALSE,warnings=FALSE, fig.width=7, fig.height=15, echo=FALSE,eval=FALSE----
#  
#  systemNickNames = c("Motor/Hand", "Motor/Mouth", "CO-Task", "Auditory", "Default", "Memory", "Visual", "FP-Task", "Salience", "Subcortical", "V Attention", "D Attention", "Cerebellar", "Uncertain" )
#  
#  lut = list("Motor/Hand"="cyan3", "Motor/Mouth"="orange", "CO-Task"="purple", "Auditory"="pink2", "Default"="red", "Memory"="gray50", "Visual"="blue", "FP-Task"="yellow2", "Salience"="black", "Subcortical"="chocolate4", "V Attention"="aquamarine4", "D Attention"="green", "Cerebellar"="cadetblue1", "Uncertain"="peachpuff2" )
#  
#  
#  sys.dat = data.frame( Time=rep( (1:nActualTimes)*tr, length(systemNickNames ) ) )
#  sys.dat$Signal = as.vector(sysMatMean)
#  sys.dat$System = factor( rep( systemNickNames, foreach=nActualTimes), levels=systemNickNames)
#  sys.dat$Lower = as.vector(sysMatMean) - as.vector(sysMatSD)
#  sys.dat$Upper = as.vector(sysMatMean) + as.vector(sysMatSD)
#  
#  sysPlot = ggplot(sys.dat)
#  sysPlot = sysPlot + geom_line(aes(x=Time, y=Signal, group=System), size=0.5)
#  sysPlot = sysPlot + geom_ribbon(aes(x=Time, ymin=Lower, ymax=Upper, alpha=0.05, fill=System))
#  sysPlot = sysPlot + scale_fill_manual( values = lut, na.value="gray80", name="System", breaks=systemNickNames, drop=FALSE)
#  sysPlot = sysPlot + theme(text=element_text(size=10), legend.position="none")
#  sysPlot = sysPlot + ggtitle("Mean BOLD signal in systems")
#  sysPlot = sysPlot + facet_grid(System ~ ., scales="free" )
#  print(sysPlot)
#  

## ----corr,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5,eval=FALSE----
#  missingROIs = which(colMeans(roiMat)==0)
#  goodROIs = (1:nLabels)
#  if ( length(missingROIs) > 0 ) {
#    goodROIs = goodROIs[-missingROIs]
#  }
#  
#  connMat = suppressWarnings(cor(roiMat))
#  diag(connMat) = rep(0, length(diag(connMat)) )
#  if ( length(missingROIs) > 0 ) {
#    connMat[missingROIs,] = 0
#    connMat[,missingROIs] = 0
#  }

## ----mi,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5,eval=FALSE----
#  if ( usePkg( "entropy" ) ) {
#    miMat = connMat * 0
#    nb = 10 # can have a strong influence on results
#    rr1 = rr2 = range( roiMat )
#    for ( i in goodROIs )
#      for ( j in goodROIs[ goodROIs > i ] ) {
#        if ( i != j) {
#          rr1 = range(roiMat[,i])
#          rr2 = range(roiMat[,j])
#          y2d = discretize2d( roiMat[,i], roiMat[,j] , nb, nb, r1=rr1, r2=rr2 )
#  #        miMat[ i, j ] =  mi.Dirichlet( y2d, a=1/8)
#          miMat[ i, j ] =  mi.empirical( y2d )
#          }
#      }
#    image( miMat + t(miMat) )
#  }

## ----adjacency,message=FALSE,warnings=FALSE, fig.width=5, fig.height=5,eval=FALSE----
#  
#  density = 0.1
#  nEdges = length(upper.tri(connMat))*density
#  thresh = sort( connMat[upper.tri(connMat)], decreasing=T)[nEdges]
#  adj = 1*(connMat >= thresh)
#  
#  bingraph = graph.adjacency(adj, mode="undirected", weighted=NULL)
#  components = clusters(bingraph)
#  maxID = which(components$csize == max(components$csize))[1]
#  
#  adj[components$membership!=maxID,] = 0
#  adj[,components$membership!=maxID] = 0
#  bingraph = graph.adjacency(adj, mode="undirected", weighted=NULL)
#  
#  invisible(plot(as.antsImage(adj)))
#  

## ----adjacencyplot,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5, echo=FALSE,eval=FALSE----
#  
#  pts$SystemName = factor(pts$SystemName, levels=c("Sensory/Somatomotor Hand", "Sensory/Somatomotor Mouth", "Cingulo-opercular Task Control", "Auditory", "Default Mode", "Memory Retrieval", "Visual", "Fronto-parietal Task Control", "Salience", "Subcortical", "Ventral Attention", "Dorsal Attention", "Cerebellar", "Uncertain"))
#  
#  graph = graph.adjacency( adj, mode="directed", weighted=NULL )
#  V(graph)$name = pts$ROI
#  V(graph)$comm = pts$SystemName
#  V(graph)$degree = degree(graph)
#  
#  systems = levels(pts$SystemName)
#  systemNames = as.character(systems)
#  
#  node_list <- get.data.frame(graph, what = "vertices")
#  edge_list <- get.data.frame(graph, what = "edges") %>%
#    inner_join(node_list %>% select(name, comm), by = c("from" = "name")) %>%
#    inner_join(node_list %>% select(name, comm), by = c("to" = "name")) %>%
#    mutate(group = ifelse(comm.x == comm.y, comm.x, NA) %>% factor())
#  
#  all_nodes <- sort(node_list$name)
#  plot_data <- edge_list %>% mutate(
#          to = factor(to, levels = all_nodes),
#          from = factor(from, levels = all_nodes))
#  
#  name_order <- (node_list %>% arrange(comm))$name
#  plot_data <- edge_list %>% mutate(
#          to = factor(to, levels = name_order),
#          from = factor(from, levels = name_order))
#  
#  plot_data$group = as.integer(plot_data$group)
#  for ( i in 1:length(systems) ) { plot_data$group[ which( plot_data$group == i) ] = as.character( systems[i] ) }
#  
#  lut = c("Sensory/Somatomotor Hand"="cyan3", "Sensory/Somatomotor Mouth"="orange", "Cingulo-opercular Task Control"="purple", "Auditory" = "pink2", "Default Mode"="red", "Memory Retrieval"="gray50", "Visual"="blue", "Fronto-parietal Task Control"="yellow2", "Salience"="black", "Subcortical"="chocolate4", "Ventral Attention"="aquamarine4", "Dorsal Attention"="green", "Cerebellar"="cadetblue1", "Uncertain"="peachpuff2" )
#  
#  adjplot = ggplot(plot_data, aes(x = from, y = to, fill = group)) + geom_raster() + theme_bw() + scale_x_discrete(drop = FALSE) + scale_y_discrete(drop = FALSE) + theme( axis.title=element_blank(), axis.ticks=element_blank(), axis.text = element_blank(),  aspect.ratio = 1 ) + scale_fill_manual( values = lut, na.value="gray80", name="System",  breaks=systemNames, drop=FALSE )
#  
#  print(adjplot)

## ----graphml,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5,eval=FALSE----
#  
#  # Retain only the largest connected component
#  bingraph = graph.adjacency(adj, mode="undirected", weighted=NULL)
#  components = clusters(bingraph)
#  maxID = which(components$csize == max(components$csize))[1]
#  
#  adj[components$membership!=maxID,] = 0
#  adj[,components$membership!=maxID] = 0
#  graph = graph.adjacency( adj, mode="undirected", weighted=NULL )
#  
#  # Set node colors
#  graph = set.vertex.attribute(graph, "r", index=V(graph), value=as.double(pts$r))
#  graph = set.vertex.attribute(graph, "g", index=V(graph), value=as.double(pts$g))
#  graph = set.vertex.attribute(graph, "b", index=V(graph), value=as.double(pts$b))
#  
#  # Set edge colors
#  edges = get.edges( graph, E(graph) )
#  nEdges = dim(edges)[1]
#  er = rep(200, nEdges)
#  eg = rep(200, nEdges)
#  eb = rep(200, nEdges)
#  
#  # colors for intra-system connections
#  #  gray for inter-system connections
#  for ( e in c(1:nEdges) )
#    {
#    if ( pts$SystemName[edges[e,1]] == pts$SystemName[edges[e,2]] )
#     {
#      er[e] = pts$r[edges[e,1]]
#      eg[e] = pts$g[edges[e,1]]
#      eb[e] = pts$b[edges[e,1]]
#      }
#    }
#  
#  graph = set.edge.attribute(graph, "r", index=E(graph), value=as.double(er))
#  graph = set.edge.attribute(graph, "g", index=E(graph), value=as.double(eg))
#  graph = set.edge.attribute(graph, "b", index=E(graph), value=as.double(eb))
#  
#  # uncomment line below to write out graph
#  # write.graph(graph, "network.graphml", format="graphml", prefixAttr=FALSE)

## ----constnode,message=FALSE,warnings=FALSE, fig.width=7, fig.height=7,eval=FALSE----
#  
#  graph = graph.adjacency( adj, mode="undirected", weighted=NULL )
#  
#  deg = degree(graph)
#  deg[deg==0] = NA
#  
#  pathsmat =  shortest.paths(graph, weights=NA)
#  pathsmat[!is.finite(pathsmat)] = NA
#  paths = rowMeans(pathsmat, na.rm=TRUE)
#  paths[paths==0] = NA
#  
#  clust = transitivity(graph, type="local")
#  clust[deg < 2] = NA
#  
#  pager = page.rank(graph)$vector
#  pager[deg < 2] = NA
#  
#  # from http://pastebin.com/XqkEYtJS
#  leff <- numeric(length(deg))
#  goodnodes <- which(deg > 1)
#  leff[goodnodes] <- sapply(goodnodes, function(x) {
#      neighbs <- neighbors(graph, v=x)
#      g.sub <- induced.subgraph(graph, neighbs)
#      Nv <- vcount(g.sub)
#  
#      lpaths <- shortest.paths(g.sub, weights=NA)
#      lpaths <- paths[upper.tri(lpaths)]
#  
#      pathsup <- lpaths[upper.tri(lpaths)]
#      2 / Nv / (Nv - 1) * sum(1 / lpaths[which(is.na(lpaths)==FALSE)])
#      }
#    )
#  leff[deg < 2] = NA
#  leff[which(is.na(deg)==TRUE)] = NA
#  
#  
#  

## ----cnodeplot,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5, echo=FALSE,eval=FALSE----
#  nNodes = length(deg)
#  
#  cnode.dat = data.frame(Node=rep(1:nNodes,5))
#  cnode.dat$Value = c( deg, paths, leff, clust, pager )
#  cnode.dat$Metric = c( rep("Degree", nNodes), rep("Shortest Path", nNodes), rep("Local Efficiency", nNodes), rep("Clustering Coefficient", nNodes), rep("Page-Rank", nNodes) )
#  
#  cnodePlot = ggplot(cnode.dat, aes(x=Node, y=Value, group=Metric, fill=Metric, colour=Metric))
#  cnodePlot = cnodePlot + geom_point()
#  cnodePlot = cnodePlot + theme(text=element_text(size=10), legend.position="none")
#  cnodePlot = cnodePlot + ggtitle("Node metrics")
#  cnodePlot = cnodePlot + facet_grid(Metric~ ., scales="free")
#  invisible(print(cnodePlot))
#  

## ----globalnode,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5,eval=FALSE----
#  
#  geff<-1/(shortest.paths(graph))
#  geff[!is.finite(geff)]<-NA
#  geff<-mean(geff,na.rm=TRUE)
#  
#  cc = transitivity(graph)
#  

## ----dmnMap,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5,eval=FALSE----
#  refSignal = sysMatMean[ , systemNames == "Default Mode"  ]
#  # get priors for different networks
#  if ( ! exists( "networkPriors" ) )
#    {
#    networkPriors = getANTsRData("fmrinetworks")
#    ilist = networkPriors$images
#    for ( i in 1:length(ilist) )
#      ilist[[i]] = antsApplyTransforms( meanbold, ilist[[i]], mni2boldmaps )
#    }
#  pr = imageListToMatrix( ilist, mask )
#  refSignal = ( sboldMat %*% t(pr) )[,1]
#  networkDf = data.frame( ROI=refSignal[goodtimes],
#                          nuissance[goodtimes,grep("compcor",colnames(nuissance))]  )
#  mdl = lm( sboldMat[goodtimes,] ~ . , data=networkDf )
#  bmdl = bigLMStats( mdl, 1.e-4 )
#  betas = bmdl$beta.t["ROI",]
#  betasI = makeImage( mask, betas )
#  loth = quantile(  betas, probs=0.5 )
#  plot( meanbold, betasI, axis=3, nslices=30, ncolumns=10,
#          window.overlay = c( 2, max(betas) ) )
#  
#  # same thing, different denoising strategy
#  dnz = aslDenoiseR( fboldMat[goodtimes,], refSignal[goodtimes],
#    polydegree = 2, crossvalidationgroups=8, verbose=T )
#  networkDf = data.frame(
#    ROI=refSignal[goodtimes],
#    dnz$noiseu, dnz$covariates )
#  mdl = lm( sboldMat[goodtimes,] ~ . , data=networkDf )
#  bmdl = bigLMStats( mdl, 1.e-4 )
#  betas = bmdl$beta.t["ROI",]
#  betasI = makeImage( mask, betas )
#  loth = quantile(  betas, probs=0.5 )
#  plot( meanbold, betasI, axis=3, nslices=30, ncolumns=10,
#          window.overlay = c( 2, max(betas) ) )
#  

