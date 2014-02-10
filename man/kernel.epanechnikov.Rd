\name{kernel.epanechnikov}
\alias{kernel.epanechnikov}
\title{Epanechnikov Kernel}
\description{
  The kernel.epanechnikov function evaluates the Epanechnikov Kernel.
}
\usage{kernel.epanechnikov(u)}
\arguments{
  \item{u}{vector of values for which the kernel is to be evaluated}
}
\details{
  The Epanechnikov Kernel is computed using the following equation:\cr\cr
  k(u) = (3/4)(1-u^2) for x = [-1,1]
}
\value{vector of kernel evaluations k(u)}
\references{
  K. Banerjee, "Kernel Density Estimator Methods for Monte Carlo
  Radiation Transport," Ph.D. Thesis, Nuclear Engineering and
  Radiological Sciences, University of Michigan (2010).
}
\author{Kerry Dunn, UW-Madison Nuclear Engineering PhD Student}
\examples{
u <- c(seq(-1,1,by=0.01));
k <- kernel.epanechnikov(u);
plot(u,k,type="l",col="blue",main="Epanechnikov Kernel")
}
