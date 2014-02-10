\name{KDE.1D-package}
\alias{KDE.1D-package}
\alias{KDE.1D}
\docType{package}
\title{Kernel Density Estimator for Approximating 1D Probability Distributions}
\description{
  This package contains a set of functions that allow the user to use kernel
  density estimation techniques to approximate a 1D probability distribution
  based on some input data.
}
\details{
\tabular{ll}{
Package: \tab KDE.1D\cr
Type: \tab Package\cr
Version: \tab 1.00\cr
Date: \tab 2011-05-10\cr
License: \tab GPL(>=2)\cr
LazyLoad: \tab yes\cr
LazyData: \tab yes\cr
}
The primary function in this package is kde.distribution, which does the
majority of the work in approximating a probability distribution.  While
no other functions are needed to use this package, some are included to
make the approximation as good as it can be.  A brief summary of each of
the available functions follows:

opt.bandwidth - computes the optimal bandwidth to use\cr
region.bandwidth - determines the optimal region based bandwidths to use\cr
kernel.epanechnikov - provides a function for the Epanechnikov Kernel\cr
kernel.boundary - provides a function for the Boundary Correction Kernel\cr
sortinto.regions - sorts a vector of values into m distinct regions\cr

In addition to the above functions, there are also four data sets that
contain 100000 observations from sample probability distributions.  These
data sets were included so that a consistent set of values was available
to test the accuracy and behavior of the kde.distribution function.
}
\author{
Kerry Dunn

Maintainer: Kerry Dunn <klhaney@wisc.edu>
}
\references{
  K. Banerjee, "Kernel Density Estimator Methods for Monte Carlo
  Radiation Transport," Ph.D. Thesis, Nuclear Engineering and
  Radiological Sciences, University of Michigan (2010).
}
