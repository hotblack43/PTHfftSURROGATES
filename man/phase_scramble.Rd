\name{phase_scramble}
\alias{phase_scramble}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
function to perform phase scrambling
}
\description{
A time series can be 'phase scrambled' by randomly reordering the Fourier components in the frequency domian, and then inverse Fourier transforming. This procedure will produce time series that are unrelated to the input time series in all ways other than having the same power spectrum.
}
\usage{
phase_scramble(fft_of_signal)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Theiler reference here as well as more moderns
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
