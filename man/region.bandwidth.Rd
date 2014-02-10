\name{region.bandwidth}
\alias{region.bandwidth}
\alias{sortinto.regions}
\title{Region Bandwidth}
\description{
  The region.bandwidth function takes the output list of regions from the
  sortinto.regions function and computes a region-based bandwidth for a
  kernel density estimation of a probability distribution.
}
\usage{
region.bandwidth(regions)
sortinto.regions(x, b)
}
\arguments{
  \item{regions}{output list from sortinto.regions function}
  \item{x}{vector of values to be sorted}
  \item{b}{vector of m-1 boundary locations of the m regions}
}
\details{
  The sortinto.regions function sorts the values from the vector of values
  x into m regions that when combined span the entire domain (-inf,inf).
  Each x value will only be associated with one region.  This function
  produces a list whose components are the regions containing a vector of
  all the values that belong to that region.  From this list, the optimal
  bandwidth in each region can be computed using the region.bandwidth
  function.

  The region bandwidth for a region r is computed from the equation:\cr\cr
  h(r)= 1.06 * sd(r) * N(r)^(-1/5)\cr\cr
  where sd(r) is the standard deviation of the N(r) observations within
  the region r.
}
\value{
  A list of vectors xi and hi is returned by the region.bandwidth function,
  where hi[i] is the optimal bandwidth for the observation xi[i].
}
\references{
  K. Banerjee, "Kernel Density Estimator Methods for Monte Carlo
  Radiation Transport," Ph.D. Thesis, Nuclear Engineering and
  Radiological Sciences, University of Michigan (2010).
}
\author{Kerry Dunn, UW-Madison Nuclear Engineering PhD Student}
\note{
  If a region contains one or less values then it will be ignored because
  the standard deviation requires at least two values to be valid.  This
  will reduce the sample size for xi that is returned.
}
\examples{
x <- runif(10);
b <- c(0,0.5,1);
regions <- sortinto.regions(x,b)
regions
region.bandwidth(regions) 
}