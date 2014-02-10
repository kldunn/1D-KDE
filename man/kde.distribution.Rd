\name{kde.distribution}
\alias{kde.distribution}
\title{Kernel Density Estimator}
\description{
  The kde.distribution function estimates the probability density function
  of an unknown distribution.
}
\usage{kde.distribution(kernel, x, xi, hi, lbound = FALSE, rbound = FALSE)}
\arguments{
  \item{kernel}{any kernel function k(u)}
  \item{x}{vector of values representing the function's domain}
  \item{xi}{sample of N observations from the unknown distribution}
  \item{hi}{bandwidth value(s)}
  \item{lbound}{set to TRUE if unknown pdf is bounded on the left}
  \item{rbound}{set to TRUE if unknown pdf is bounded on the right}
}
\details{
  The kernel density estimation of the probability density function is
  computed using the following summation:\cr\cr                                
  f(x) = (1/N) sum[i=1:N] (1/h) k[ (x-xi)/h ]
}
\value{
  A vector containing the estimated probability distribution f(x)
  evaluated at the user-provided domain values x.
}
\references{
  K. Banerjee, "Kernel Density Estimator Methods for Monte Carlo
  Radiation Transport," Ph.D. Thesis, Nuclear Engineering and
  Radiological Sciences, University of Michigan (2010).
}
\author{Kerry Dunn, UW-Madison Nuclear Engineering PhD Student}
\note{
  This function will accept both a global bandwidth and a region based
  bandwidth.  To use a global bandwidth, hi must be of length 1.  If hi
  is greater than length 1 it will be assumed that it is of the same
  length as xi and represents region based bandwidths (i.e. each point
  xi is defined by its own bandwidth hi).
}
\examples{
par(mfcol=c(2,2))

# This example plots the KDE approximation using a global bandwidth
# and the actual pdf for the normal distribution.
x <- c(seq(-4,4,by=0.2));
xi <- rnorm(10000);
hi <- opt.bandwidth(xi);
fx <- kde.distribution(kernel.epanechnikov,x,xi,hi);
plot(x,fx,main="Normal")
points(x,dnorm(x),type='l')

# This example plots the KDE approximation using a global bandwidth
# and the actual pdf for the exponential distribution.
x <- c(seq(0,6,by=0.2));
xi <- rexp(10000);
hi <- opt.bandwidth(xi);
fx <- kde.distribution(kernel.epanechnikov,x,xi,hi,lbound=TRUE);
plot(x,fx,main="Exponential")
points(x,dexp(x),type='l')

# This example plots the KDE approximation using a global bandwidth
# and the actual pdf for the uniform distribution.
x <- c(seq(0,1,by=0.1));
xi <- runif(10000);
hi <- opt.bandwidth(xi);
fx <- kde.distribution(kernel.epanechnikov,x,xi,hi,lbound=TRUE,rbound=TRUE);
plot(x,fx,ylim=c(0,1.2),main="Uniform")
points(x,dunif(x),type='l')

# This example plots the KDE approximation of the uniform/normal
# mixture using a region bandwidth.
n <- 10000;
xi <- c(rep(0,n));
for (i in 1:n) {
  if (runif(1) < 0.5)
    xi[i] <- runif(1)
  else
    xi[i] <- rnorm(1,0.5,sqrt(0.001))
}

b <- c(0,0.25,0.5,0.75,1);
regions <- sortinto.regions(xi,b);
bandwidth <- region.bandwidth(regions);
x <- c(seq(0,0.3,by=0.1), seq(0.4,0.6,by=0.01), seq(0.7,1,by=0.1));
xi <- bandwidth$xi
hi <- bandwidth$hi;
fx <- kde.distribution(kernel.epanechnikov,x,xi,hi,lbound=TRUE,rbound=TRUE);
plot(x,fx,main="Uniform/Normal Mixture",ylim=c(0,7))
points(x,0.5*dunif(x) + 0.5*dnorm(x,0.5,sqrt(0.001)),type='l') 
}
