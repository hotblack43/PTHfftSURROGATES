phase_scramble2 <- function(series){
# Perform 'phase-scrambling' on an input series given the series
# Uses code from BOC
# NOTE: output is returned in the input argument
  fft_signal <- fft(series)
  sd_org <- sd(series)

  nmax <- length(fft_signal)
  if (even(nmax)) {   # nmax  even
    theta <- runif(nmax/2-1,min=0,max=2.*pi)
    factor <- complex(real=cos(theta),imaginary=sin(theta))
    factor <- c(1,factor,1,Conj(rev(factor[])))
  }
  if (odd(nmax)) {
    theta <- runif(nmax/2,min=0,max=2.*pi)
    factor <- complex(real=cos(theta),imaginary=sin(theta))
    factor <- c(1,factor,Conj(rev(factor[])))
  }
  new_signal <- Re(fft(fft_signal*factor,inverse=TRUE))/nmax
  new_signal <- new_signal/sd(new_signal)*sd_org
    # Return the syntehtic signal
  return(new_signal) # This could overwrite the input - be careful!
}
