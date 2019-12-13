\name{phase_scramble2}
\alias{phase_scramble2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
function to perform phase scrambling - based on an input series (that gets overwritten)
}
\description{
A time series can be 'phase scrambled' by randomly reordering the Fourier components in the frequency domian, and then inverse Fourier transforming. This procedure will produce time series that are unrelated to the input time series in all ways other than having the same power spectrum.
}
\usage{
phase_scramble2(series)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{series}{
The series you want to get phase-scrambled copies of
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
 \item{series_scrambled}{The phase-scrambled real-valued series}
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
org_series <- series
IOseries <- series
series_scrambled <- phase_scramble2(IOseries)}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
