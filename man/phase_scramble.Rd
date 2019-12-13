\name{phase_scramble}
\alias{phase_scramble}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
function to perform phase scrambling - based on an input FFT of the series you want to scramble
}
\description{
A time series can be 'phase scrambled' by randomly reordering the Fourier components in the frequency domian, and then inverse Fourier transforming. This procedure will produce time series that are unrelated to the input time series in all ways other than having the same power spectrum.
}
\usage{
phase_scramble(fft_of_signal)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fft_of_signal}{
The forward Fourier Transform of the timeseries: fft_of_signal <- fft(signal)
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
 \item{series_scr}{The phase-scrambled real-valued series}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
J. Theiler, D. Prichard, Constrained-realization Monte-Carlo method for hypothesis testing, Physica D 94 (1996) 221–235
}
\author{
Peter Thejll and Bo Christiansen
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
fft_signal <- fft(series)
series_scrambled <- phase_scramble(fft_signal)}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
