#' Block stimulus model for FMRI Data
#' 
#' Create the block BOLD stimulus for a given task indicator function. Current
#' units in terms of volumes.  Sort of a bug, should be in TR.
#' 
#' None
#' 
#' @param scans number of scans
#' @param onsets vector of onset times (in scans)
#' @param durations vector of duration of ON stimulus in scans or seconds (if
#' \code{!is.null(times)})
#' @param rt time between scans in seconds (TR)
#' @return Vector with dimension \code{c(scans, 1)}.
#' @author b avants \email{stnava@@gmail.com}
#' @references Worsley, K.J., Liao, C., Aston, J., Petre, V., Duncan, G.H.,
#' Morales, F., Evans, A.C. (2002). A general statistical analysis for fMRI
#' data. NeuroImage, 15:1-15.
#' @keywords regression design
#' @examples
#' 
#'   # Example 1
#'   hrf <- blockStimulus(107, c(18, 48, 78), 15, 2)
#' 
#' @export blockStimulus
blockStimulus <- function(scans = 1, onsets = c(1), durations = c(1), rt = 3) {
  stimulus <- rep(0, scans)
  if (length(durations) > 1 & length(durations) != length(onsets)) {
    print("length of durations should be 1 or length of onsets")
    q()
  }
  if (length(durations) == 1) 
    durations <- rep(durations[1], length(onsets))
  for (stim in 1:length(onsets)) {
    stimulus[onsets[stim]:(onsets[stim] + durations[stim] - 1)] <- 1
  }
  return(stimulus)
} 
