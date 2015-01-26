#' Linear Model for FMRI Data
#' 
#' Create the expected BOLD response for a given task indicator function.
#' Borrowed from the fmri package.
#' 
#' The functions calculates the expected BOLD response for the task indicator
#' function given by the argument as a convolution with the hemodynamic
#' response function. The latter is modelled by the difference between two
#' gamma functions as given in the reference (with the defaults for a1, a2, b1,
#' b2, cc given therein):
#' 
#' \deqn{\left(\frac{t}{d_1}\right)^{a_1} \exp \left(-\frac{t-d_1}{b_1}\right)
#' }{(x/d1)^a1 * exp(-(x - d1)/b1) - c * (x/d2)^a2 * exp(-(x - d2)/b2)}\deqn{-
#' c \left(\frac{t}{d_2}\right)^{a_2} \exp }{(x/d1)^a1 * exp(-(x - d1)/b1) - c
#' * (x/d2)^a2 * exp(-(x - d2)/b2)}\deqn{\left(-\frac{t-d_2}{b_2}\right)
#' }{(x/d1)^a1 * exp(-(x - d1)/b1) - c * (x/d2)^a2 * exp(-(x - d2)/b2)}
#' 
#' The parameters of this function can be changed through the arguments
#' \code{a1}, \code{a2}, \code{b1}, \code{b2}, \code{cc}.
#' 
#' The dimension of the function value is set to \code{c(scans,1)}.
#' 
#' If \code{!is.null(times)} durations are specified in seconds.
#' 
#' If \code{mean} is TRUE (default) the resulting vector is corrected to have
#' zero mean.
#' 
#' @param scans number of scans
#' @param onsets vector of onset times (in scans)
#' @param durations vector of duration of ON stimulus in scans or seconds (if
#' \code{!is.null(times)})
#' @param rt time between scans in seconds (TR)
#' @param times onset times in seconds. If present \code{onsets} arguments is
#' ignored.
#' @param mean logical. if TRUE the mean is substracted from the resulting
#' vector
#' @param a1 parameter of the hemodynamic response function (see details)
#' @param a2 parameter of the hemodynamic response function (see details)
#' @param b1 parameter of the hemodynamic response function (see details)
#' @param b2 parameter of the hemodynamic response function (see details)
#' @param cc parameter of the hemodynamic response function (see details)
#' @return Vector with dimension \code{c(scans, 1)}.
#' @author Karsten Tabelow \email{tabelow@@wias-berlin.de}
#' @references Worsley, K.J., Liao, C., Aston, J., Petre, V., Duncan, G.H.,
#' Morales, F., Evans, A.C. (2002). A general statistical analysis for fMRI
#' data. NeuroImage, 15:1-15.
#' 
#' Polzehl, J. and Tabelow, K. (2007) \emph{fmri: A Package for Analyzing fmri
#' Data}, R News, 7:13-17 .
#' @keywords regression design
#' @examples
#' 
#'   # Example 1
#'   hrf <- hemodynamicRF(107, c(18, 48, 78), 15, 2)
#'   # Example 2: effect of varying parameter cc
#'   cc<-round(seq(0,1,length.out=10),2)
#'   nlev<-length(cc)
#'   cscale<-rgb(seq(0,1,length.out=nlev),seq(1,0,length.out=nlev),0,1)
#'   mat<-matrix(NA,nrow=nlev,ncol=20)
#'   for (i in 1:nlev) {
#'     hrf<-ts( hemodynamicRF(scans=20, onsets=1, durations=2, rt=1,cc=cc[i],a1=4,a2=3))
#'     mat[i,]<-hrf
#'   }
#'   matplot(seq(1,20),t(mat),'l',lwd=1,col=cscale,xlab='Time',ylab='Response',main='Parameter cc')
#'   legend(x='topleft',legend=cc,text.col=cscale)
#'   # Example 3: effect of varying parameter a1
#'   a1<-seq(1,10)
#'   nlev<-length(a1)
#'   cscale<-rgb(seq(0,1,length.out=nlev),seq(1,0,length.out=nlev),0,1)
#'   mat<-matrix(NA,nrow=nlev,ncol=20)
#'   for (i in 1:nlev) {
#'     hrf<-ts( hemodynamicRF(scans=20, onsets=1, durations=2, rt=1,a1=a1[i],a2=3))
#'     mat[i,]<-hrf
#'   }
#'   matplot(seq(1,20),t(mat),'l',lwd=1,col=cscale,xlab='Time',ylab='Response',main='Parameter a1')
#'   legend(x='topleft',legend=a1,text.col=cscale)
#'   # Example 4: effect of varying parameter a2
#'   a2<-seq(1,10)
#'   nlev<-length(a2)
#'   cscale<-rgb(seq(0,1,length.out=nlev),seq(1,0,length.out=nlev),0,1)
#'   mat<-matrix(NA,nrow=nlev,ncol=20)
#'   for (i in 1:nlev) {
#'     hrf<-ts( hemodynamicRF(scans=20, onsets=1, durations=2, rt=1,a1=4,a2=a2[i]))
#'     mat[i,]<-hrf
#'   }
#'   matplot(seq(1,20),t(mat),'l',lwd=1,col=cscale,xlab='Time',ylab='Response',main='Parameter a2')
#'   legend(x='topleft',legend=a2,text.col=cscale)
#'   # Example 5: effect of varying parameter b1
#'   b1<-seq(0.4,1.3,by=0.1)
#'   nlev<-length(b2)
#'   cscale<-rgb(seq(0,1,length.out=nlev),seq(1,0,length.out=nlev),0,1)
#'   mat<-matrix(NA,nrow=nlev,ncol=20)
#'   for (i in 1:nlev) {
#'     hrf<-ts( hemodynamicRF(scans=20, onsets=1, durations=2, rt=1,a1=4,a2=3, b1=b1[i]))
#'     mat[i,]<-hrf
#'   }
#'   matplot(seq(1,20),t(mat),'l',lwd=1,col=cscale,xlab='Time',ylab='Response',main='Parameter b1')
#'   legend(x='topleft',legend=b1,text.col=cscale)
#'   # Example 6: effect of varying parameter b2
#'   b2<-seq(0.4,1.3,by=0.1)
#'   nlev<-length(b2)
#'   cscale<-rgb(seq(0,1,length.out=nlev),seq(1,0,length.out=nlev),0,1)
#'   mat<-matrix(NA,nrow=nlev,ncol=20)
#'   for (i in 1:nlev) {
#'     hrf<-ts( hemodynamicRF(scans=20, onsets=1, durations=2, rt=1,a1=4,a2=3, b2=b2[i]))
#'     mat[i,]<-hrf
#'   }
#'   matplot(seq(1,20),t(mat),'l',lwd=1,col=cscale,xlab='Time',ylab='Response',main='Parameter b2')
#'   legend(x='topleft',legend=b2,text.col=cscale)
#' 
#' @export hemodynamicRF
hemodynamicRF <- function(scans = 1, onsets = c(1), durations = c(1), rt = 3, times = NULL, 
  mean = TRUE, a1 = 6, a2 = 12, b1 = 0.9, b2 = 0.9, cc = 0.35) {
  
  mygamma <- function(x, a1, a2, b1, b2, c) {
    d1 <- a1 * b1
    d2 <- a2 * b2
    c1 <- (x/d1)^a1
    c2 <- c * (x/d2)^a2
    res <- c1 * exp(-(x - d1)/b1) - c2 * exp(-(x - d2)/b2)
    res
  }
  
  
  
  if (is.null(times)) {
    scale <- 1
  } else {
    scale <- 100
    onsets <- times/rt * scale
    durations <- durations/rt * scale
    rt <- rt/scale
    scans <- scans * scale
  }
  numberofonsets <- length(onsets)
  
  if (length(durations) == 1) {
    durations <- rep(durations, numberofonsets)
  } else if (length(durations) != numberofonsets) {
    stop("Length of duration vector does not match the number of onsets!")
  }
  stimulus <- rep(0, scans)
  
  for (i in 1:numberofonsets) {
    for (j in onsets[i]:(onsets[i] + durations[i] - 1)) {
      stimulus[j] <- 1
    }
  }
  stimulus <- c(rep(0, 20 * scale), stimulus, rep(0, 20 * scale))
  # just fill with zeros to avoid bounding effects in convolve
  hrf <- convolve(stimulus, mygamma(((40 * scale) + scans):1, a1, a2, b1/rt, b2/rt, 
    cc))/scale
  hrf <- hrf[-(1:(20 * scale))][1:scans]
  hrf <- hrf[unique((scale:scans)%/%scale) * scale]
  
  dim(hrf) <- c(scans/scale, 1)
  
  if (mean) {
    hrf - mean(hrf)
  } else {
    hrf
  }
  hrf<-hrf-min(hrf)
  hrf<-hrf/sum(hrf)
} 
