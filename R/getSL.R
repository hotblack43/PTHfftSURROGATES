getSL <- function(Nboot,method_str,series1,series2){
# correlation significance level via phase scrambled surrogate data
# Returns the two-sided p-value, that is, the probability that Robs is a fluke
# NOTE: performs filling of holes with median values
  Robs <- cor(series1,series2,use="complete.obs", method=method_str)
  #print(c("Robs: ",Robs))
  Rvalues <- NULL
  for (iboot in 1:Nboot){
    idx <- which(is.na(series2))
    series2filled <- series2
    series2filled[idx] <- median(series2,na.rm=TRUE)
    fft_signal <- fft(series2filled)
    series2_scrambled <- phase_scramble(fft_signal)
    Rvalues <- c(Rvalues,cor(series1,series2_scrambled, use="complete.obs",method=method_str))
  }
  SL <- length(which(abs(Rvalues) > abs(Robs)))/Nboot
  return(SL)
}
