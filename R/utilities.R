### -----------------------------------------------------------------
### generate the points from a pseudo Gaussian Spectrum.
### Exported!
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


