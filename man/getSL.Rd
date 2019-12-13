\name{getSL}
\alias{getSL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimate correlation significance levels using phase-scrambling technqiues.
}
\description{
Use the phase_scramble function to generate many (Nboot) surrogate-data based correlations, compare to the observed correlation, calculate the fraction of trials exceeding the observed value and return the significance level as a fraction.
}
\usage{
getSL(Nboot,method,series1,series2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Nboot}{Number of Monte-Carlo phase-scrambling trials to perform}
  \item{method}{pearson, kendall or spearman}
  \item{series1,series2}{the two series of numbers to correlate}
  }
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{SL    }{Significance levels, or fraction of surrogat edata trials exceeding the observed correlation. Two sided!}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Peter Thejll, DMI. (pth@dmi.dk)
}
\note{
Version 0.1, Dec 2019
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
library(PTHfftSURROGATES)
require(gtools)
set.seed(6) # for reproducibility

#Example 1 - two probably uncorrelated series
series1 <- rnorm(1000)
series2 <- rnorm(1000)
method_str="pearson" # pearson, kendall or spearman
Robs <- cor(series1,series2,method=method_str)
print(c("The observed correlation is ",Robs))
Nboot <- 1000
SL <- getSL(Nboot,method=method_str,series1,series2)
print(c("The p-value of the observed correlation is:",SL))

#Example 2 - two probably correlated series
series2 <- 2*rnorm(1000) + 1*series1
Robs <- cor(series1,series2,method=method_str)
print(c("The observed correlation is ",Robs))
SL <- getSL(Nboot,method=method_str,series1,series2)
print(c("The p-value of the observed correlation is",SL))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
