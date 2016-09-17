\name{generatePseudoGaussianSpectrum}
\alias{generatePseudoGaussianSpectrum}

\title{
  generatePseudoGaussianSpectrum
}
\description{
  Given the locations and intensities of peaks, 
  generate the data points of pseudo gaussian spectrum.
}
\usage{
generatePseudoGaussianSpectrum(x, y, sd, xlim=range(x))
}

\arguments{
  \item{x}{
    A \code{numric} vector: the locations of the peaks.
  }
  \item{y}{
    A \code{numeric} vector: the intensities of the peaks.
  }
  \item{sd}{
    A \code{numeric} value: the standard deviation of the pseodo gaussian
    distribution. It controls the wideness of the distribution.
  }
  \item{xlim}{
    The desired range of simulated data points.
  }
}

\value{
  A \code{data.frame} of coordinates and intensities.
}

\author{
  Yang Yang
}

\seealso{

}

\examples{
  set.seed(1)
  
  ## The x-coordinates of peaks
  x <- c(100, 200, 350, 400, 550, 600, 800)
  
  ## The intensities of peaks
  y <- sample(1:100, size=length(x))
  
  ## Generate the points of pseudo gaussian spectrum of peaks
  ans <- generatePseudoGaussianSpectrum(x, y, sd=5, xlim=c(1,1000))
  
  ## Plot the pseudo gaussian spectrum
  plot(ans$x, ans$y, type="l")
}
