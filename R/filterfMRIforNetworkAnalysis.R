#' Basic pre-processing for BOLD or ASL-based network analysis.
#' 
#' This function works for either raw BOLD time-series data, ASL-based BOLD
#' time-series data or ASL-based CBF time series data.  In all 3 cases, this
#' function performs motion-correction, factoring out motion and compcor
#' nuisance paramters, frequency filtering and masking.  The output contains
#' the filtered time series (matrix form), the mask and a vector of temporal
#' signal variance. Some ASL MR sequences allow network analysis of either BOLD
#' or ASL signal.  See "Implementation of Quantitative Perfusion Imaging
#' Techniques for Functional Brain Mapping using Pulsed Arterial Spin Labeling"
#' by Wong et al, 1997 for an overview.  This function employs "surround"
#' techniques for deriving either CBF or BOLD signal from the input ASL.  This
#' is a WIP.
#' 
#' 
#' @param asl_antsr_image_or_filename The filename to an antsr image or pointer
#' to an antsr image
#' @param tr=<value> The sequence's TR value , typically 3 or 4.
#' @param freqLo=<value> The lower frequency limit, e.g. 0.01 in band-pass
#' filter
#' @param freqHi=<value> The higher frequency limit, e.g. 0.1 in band-pass
#' filter
#' @param cbfnetwork="ASLCBF" A string dictating whether to do nothing special
#' (standard BOLD) or get CBF (ASLCBF) or BOLD (ASLBOLD) signal from ASL
#' @param maskThresh=<value> A thresholding value for the mask ... may need to
#' be adjusted up or down for your data although the mask does not need to be
#' perfect.
#' @param smoother=<value> A smoothing parameter for space and time.
#' @param outputprefix=<string> filename prefix - if defined , this function
#' will write out some sanity checking images.
#' @return output is a list containing "filteredTimeSeries" "mask"
#' "temporalvar"
#' 
#' or
#' 
#' 1 -- Failure
#' @author Avants BB
#' @examples
#' 
#' \dontrun{
#' fmat<-timeseries2matrix( img, mask )
#' myres<-filterfMRIforNetworkAnalysis(fmat,tr=4,0.01,0.1,cbfnetwork="BOLD", mask = mask )
#' }
#' 
#' @export filterfMRIforNetworkAnalysis
filterfMRIforNetworkAnalysis <- function(aslmat, tr, freqLo = 0.01, freqHi = 0.1, cbfnetwork = "ASLCBF", mask = NA,
  labels = NA, graphdensity = 0.5, seg = NA, useglasso = NA, nuisancein=NA, usesvd = FALSE , robustcorr = FALSE  ) {
  pixtype <- "float"
  myusage <- "usage: filterfMRIforNetworkAnalysis( timeSeriesMatrix, tr, freqLo=0.01, freqHi = 0.1, cbfnetwork=c(\"BOLD,ASLCBF,ASLBOLD\") , mask = NA,  graphdensity = 0.5 )"
  if (nargs() == 0) {
    print(myusage)
    return(NULL)
  }
  if (!is.numeric(tr) | missing(tr)) {
    print("TR parameter is missing or not numeric type - is typically between 2 and 4 , depending on your fMRI acquisition")
    print(myusage)
    return(NULL)
  }
  if (!is.numeric(freqLo) | !is.numeric(freqHi)) {
    print("freqLo/Hi is not numeric type")
    print(myusage)
    return(NULL)
  }
  if (missing(aslmat)) {
    print("Missing first (image) parameter")
    print(myusage)
    return(NULL)
  }
  if ( usesvd ) {
  pckg <- try(require(irlba))
  if (!pckg) {
    getPckg("irlba")
  }
  library(irlba)
  }
  freqLo <- freqLo * tr
  freqHi <- freqHi * tr
  # network analysis
  wb <- (mask > 0)  # whole brain
  leftinds <- shift(c(1:nrow(aslmat)), 1)
  rightinds <- shift(c(1:nrow(aslmat)), -1)
  oaslmat <- aslmat
  if (cbfnetwork == "ASLCBF") {
    # surround subtraction for cbf networks
    aslmat <- oaslmat - 0.5 * (oaslmat[leftinds, ] + oaslmat[rightinds, ])
    taginds <- c(1:(nrow(aslmat)/2)) * 2
    controlinds <- taginds - 1
    aslmat[controlinds, ] <- aslmat[controlinds, ] * (-1)  # ok! done w/asl specific stuff
    # plot( apply( aslmat[controlinds,],1, mean) , type='l') # should be (+) everywhere
  }
  if (cbfnetwork == "ASLBOLD") {
    # surround addition for bold networks
    aslmat <- oaslmat + 0.5 * (oaslmat[leftinds, ] + oaslmat[rightinds, ])
    # plot( apply( aslmat[controlinds,],1, mean) , type='l')
  }
  voxLo <- round((1/freqLo))  # remove anything below this (high-pass)
  voxHi <- round((1/freqHi))  # keep anything above this
  myTimeSeries <- ts(aslmat, frequency = 1/tr)
  filteredTimeSeries<-frequencyFilterfMRI( aslmat, tr=tr, freqLo=freqLo, freqHi=freqHi, opt='trig')
  vox <- round(ncol(filteredTimeSeries) * 0.5)  #  a test voxel
  spec.pgram(filteredTimeSeries[, vox], taper = 0, fast = FALSE, detrend = F, demean = F, log = "n")
  temporalvar <- apply(filteredTimeSeries, 2, var)
  wh <- which(temporalvar == 0)
  for (x in wh) {
    filteredTimeSeries[, x] <- sample(filteredTimeSeries, nrow(filteredTimeSeries))
  }
  if (!is.na(labels)) {
    # do some network thing here
    oulabels <- sort(unique(labels[labels > 0]))
    whvec <- (mask == 1)
    ulabels <- sort(unique(labels[whvec]))
    if (ulabels[1] == 0)
      ulabels <- ulabels[2:length(ulabels)]
    labelvec <- labels[whvec]
    if (!is.na(seg))
      segvec <- seg[whvec] else segvec <- NA
    labmat <- matrix(data = rep(NA, length(oulabels) * nrow(filteredTimeSeries)), nrow = length(oulabels))
    nrowts <- nrow(filteredTimeSeries)
    for (mylab in ulabels) {
      if (!is.na(seg)) {
        dd <- (labelvec == mylab & segvec == 2)
      } else {
        dd <- labelvec == mylab
      }

      submat <- filteredTimeSeries[, dd]
      # if ( length( c( submat ) ) > nrowts ) myavg<-svd( submat )$u[,1] else myavg<-submat

      if (length(c(submat)) > nrowts) {
        myavg <- apply(submat, MARGIN = 1, FUN = mean)
        if ( usesvd )
            {
                eanat<-irlba(submat, nu = 1, nv = 0, maxit=50 )
                myavg<-eanat$u[,1]
            }
      } else {
        myavg <- submat
      }

      if (length(myavg) > 0) {
        labmat[which(ulabels == mylab), ] <- myavg
      } else {
        labmat[which(ulabels == mylab), ] <- NA
      }
      # if ( length(myavg) > 0 ) labmat[ mylab, ]<-myavg else labmat[ mylab, ]<-NA
    }
    nbrainregions<-length(oulabels)
    tlabmat<-t(labmat)
    ocormat <- cor(tlabmat, tlabmat)
    rcormat<-ocormat
    if ( robustcorr )
    for ( i in 1:nrow(rcormat) ) {
        for ( j in i:nrow(rcormat) ) {
            if ( i != j ) {
            a<-scale(tlabmat[,i])
            b<-scale(tlabmat[,j])
            rcormat[i,j]<-rcormat[j,i]<-as.numeric(coefficients(rlm(a~b)))[2]
            }
        }
    }
    if ( ! is.na( nuisancein ) )
        {
        tlabmat<-cbind( tlabmat, nuisancein )
        ocormat<-cor( tlabmat, tlabmat )
        }
    cormat<-ocormat
    pcormat<-solve( cormat+(diag(ncol(cormat))+0.01) )
    diagmag<-sqrt( diag(pcormat) %o% diag(pcormat) )*(-1)
    pcormat<-pcormat/diagmag
    diag(pcormat)<-1
    gcormat<-pcormat
    if ( !is.na(useglasso) )
    if ( useglasso > 0 ) # treat useglasso as rho
      {
      gcormat<-glasso( cormat, useglasso )$wi
      diagmag<-sqrt( diag(gcormat) %o% diag(gcormat) )*(-1)
      gcormat<-gcormat/diagmag
      myinds<-( abs( gcormat ) < 1.e-4 )
      gcormat[ myinds ]<-0
      diag(gcormat)<-1
      rgdens<-( 0.5*sum(gcormat>0&gcormat<1)/ (0.5*ncol(gcormat)*(ncol(gcormat)-1)) )
      print(paste("Use Glasso",useglasso,"density",rgdens))
      }
    if ( ! is.na( nuisancein ) ) pcormat<-pcormat[1:nbrainregions,1:nbrainregions]
    if ( ! is.na( nuisancein ) ) gcormat<-pcormat[1:nbrainregions,1:nbrainregions]
    if ( ! is.na( nuisancein ) ) cormat<-cormat[1:nbrainregions,1:nbrainregions]
    if ( ! is.na( nuisancein ) ) ocormat<-ocormat[1:nbrainregions,1:nbrainregions]
    gmet <- makeGraph(cormat, graphdensity = graphdensity)
    return( list(filteredTimeSeries = filteredTimeSeries, mask = mask, temporalvar = temporalvar, network = labmat,
      graph = gmet, corrmat = ocormat, partialcorrmat=pcormat, glassocormat=gcormat,rcormat=rcormat ))
  } else {
    return(list(filteredTimeSeries = filteredTimeSeries, mask = mask, temporalvar = temporalvar))
  }
}
