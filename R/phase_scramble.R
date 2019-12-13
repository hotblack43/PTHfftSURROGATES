phase_scramble <- function(fft_signal){
# Perform 'phase-scrambling' on an input series given its FFT
# Uses code from BOC

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
    # Return the syntehtic signal
  return(new_signal)
}
