#' Generate the points from a pseudo Gaussian Spectrum.
#' 
#' Given the locations and intensities of peaks, generate the data points of pseudo gaussian spectrum.
#' 
#' @param x A \code{numric} vector: the locations of the peaks.
#' @param y A \code{numeric} vector: the intensities of the peaks.
#' @param sd A \code{numeric} value: the standard deviation of the pseodo gaussian
#'           distribution. It controls the wideness of the distribution.
#' @param xlim The desired range of simulated data points.
#' @importFrom stats dnorm
#' @export
#' @return A \code{data.frame} of coordinates and intensities.
#' @author Yang Yang
#' @examples 
#' set.seed(1)
#' ## The x-coordinates of peaks
#' x <- c(100, 200, 350, 400, 550, 600, 800)
#' ## The intensities of peaks
#' y <- sample(1:100, size=length(x))
#' ## Generate the points of pseudo gaussian spectrum of peaks
#' ans <- generatePseudoGaussianSpectrum(x, y, sd=5, xlim=c(1,1000))
#' ## Plot the pseudo gaussian spectrum
#' plot(ans$x, ans$y, type="l")

generatePseudoGaussianSpectrum <- function(x, y, sd, xlim=range(x)){
  stopifnot(length(x) == length(y))
  plot_x <- seq(xlim[1], xlim[2], by=1)
  ans_y <- numeric(length(plot_x))
  for(i in 1:length(x)){
    plot_y <- dnorm(plot_x, mean=x[i], sd=sd)
    plot_y <- y[i] / max(plot_y) * plot_y
    ans_y <- pmax(ans_y, plot_y, na.rm=TRUE)
  }
  return(data.frame(x=plot_x, y=ans_y))
}


