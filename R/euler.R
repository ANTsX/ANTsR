#' Euler Character
#'
#' Mostly an for internal use in conjunction with \code{resels}. Calculates the
#' euler characteristic at a threshold level.
#'
#' @param u statistical value (typically the maxima of a cluster or statistical field)
#' @param df degrees of freedom expressed as df = c(degrees of interest, degrees of error)
#' @param fieldType:
#' \itemize{
#' \item{T: } {T-field} 
#' \item{F: } {F-field} 
#' \item{X: } {Chi-square field'} 
#' \item{Z: } {Gaussian field}
#' }
#' @return A vector of estimated euler characteristics for dimensions 0:D
#'
#' @references 
#' Worlsey K.J., (1996) A Unified Statistical Approach for Determining Significant Signals in Images of Cerebral Activation.
#' @author Zachary P. Christensen
#' @seealso rftPval, resels
#' @examples
#' mask <- getMask(antsImageRead(getANTsRData('mni')))
#' myresels <- resels(mask, c(1, 1, 1))
#' # pretend these results are from a t-statistical field fitted with 10 df to
#' # find a peak value. 
#' df <- c(1, 10) # made up degrees of freedom
#' ec <- euler(4, df, fieldType = "T") # peak value is 4
#' ez <- euler(3, df, fieldType = "T") # we thresholded at 3
#' # uncorrected peak-level p-value
#' pvox <- sum(ec * myresels)/sum(ez * myresels)
#' 
#' @export euler
euler <- function(u, df, fieldType) {
  if (missing(fieldType)) 
    stop("Must specify fieldType") 
  else if (missing(df)) 
    stop("Must specify df")
  else if (missing(u)) 
    stop("Must specify u")
  
  ec <- rep(0, 4)
  if (fieldType == "T") {
    ec[1] <- 1 - pt(u, df[2])
    ec[2] <- (((4 * log(2))^(1/2))/(2 * pi)) * ((1 + ((u^2)/df[2]))^(-1/2 * (df[2] - 1)))
    ec[3] <- (4 * log(2))/((2 * pi)^(3/2)) * ((1 + u^2/df[2])^((1 - df[2])/2)) * u/((df[2]/2)^(1/2)) * 
             exp(lgamma((df[2] + 1)/2) - lgamma(df[2]/2))
    ec[4] <- (((4 * log(2))^(3/2))/((2 * pi)^2)) * ((1 + ((u^2)/df[2]))^(-1/2 * (df[2] - 1))) * 
             ((((df[2] - 1)/df[2]) * (u^2)) - 1)
  } else if (fieldType == "F") {
    ec[1] <- 1 - pf(u, df[1], df[2])
    ec[2] <- ((4 * log(2))/(2 * pi))^(1/2) * exp(lgamma((df[2] + df[1] - 1)/2) - (lgamma(df[2]/2) + 
             lgamma(df[1]/2))) * 2^(1/2) * (df[1] * u/df[2])^(1/2 * (df[1] - 1)) * (1 + df[1] * 
             u/df[2])^(-1/2 * (df[2] + df[1] - 2))
    ec[3] <- ((4 * log(2))/(2 * pi)) * exp(lgamma((df[2] + df[1] - 2)/2) - (lgamma(df[2]/2) + 
             lgamma(df[1]/2))) * (df[1] * u/df[2])^(1/2 * (df[1] - 2)) * (1 + df[1] * u/df[2])^(-1/2 * 
             (df[2] + df[1] - 2)) * ((df[2] - 1) * df[1] * u/df[2] - (df[1] - 1))
    ec[4] <- ((4 * log(2))/(2 * pi))^(3/2) * exp(lgamma((df[2] + df[1] - 3)/2) - (lgamma(df[2]/2) + 
             lgamma(df[1]/2))) * 2^(-1/2) * (df[1] * u/df[2])^(1/2 * (df[1] - 3)) * (1 + df[1] * 
             u/df[2])^(-1/2 * (df[2] + df[1] - 2)) * ((df[2] - 1) * (df[2] - 2) * (df[1] * u/df[2])^2 - 
             (2 * df[2] * df[1] - df[2] - df[1] - 1) * (df[1] * u/df[2]) + (df[1] - 1) * (df[1] - 2))
  } else if (fieldType == "X") {
    ec[1] <- 1 - pchisq(u, df[2])
    ec[2] <- ((4 * log(2))/(2 * pi))^(1/2) * (u^(1/2 * (df[2] - 1)) * exp(-u/2 - lgamma(df[2]/2))/2^((df[2] - 2)/2))
    ec[3] <- ((4 * log(2))/(2 * pi)) * (u^(1/2 * (df[2] - 1)) * exp(-u/2 - lgamma(df[2]/2))/2^((df[2] - 
             2)/2)) * (u - (df[2] - 1))
    ec[4] <- ((4 * log(2))/(2 * pi))^(3/2) * (u^(1/2 * (df[2] - 1)) * exp(-u/2 - lgamma(df[2]/2))/2^((df[2] - 
             2)/2)) * (u^2 - (2 * df[2] - 1) * u + (df[2] - 1) * (df[2] - 2))
  } else if (fieldType == "Z") {
    ec[1] <- 1 - pnorm(u, df[2])
    ec[2] <- (4 * log(2))^(1/2)/(2 * pi) * exp(-u^2/2)
    ec[3] <- (4 * log(2))/((2 * pi)^(3/2)) * exp(-u^2/2) * u
    ec[4] <- (4 * log(2))^(3/2)/((2 * pi)^2) * exp(-u^2/2) * (u^2 - 1)
  }
  ec
} 