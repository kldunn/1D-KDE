\name{region.bandwidth}
\alias{region.bandwidth}
\alias{sortinto.regions}
\title{Region Bandwidth}
\description{
  The region.bandwidth function takes up to two output lists of regions from
  the sortinto.regions function and computes regional bandwidths for a kernel
  density estimation of a probability distribution.
}
\usage{
region.bandwidth(xi_regions, xj_regions = NULL)
sortinto.regions(x, b, finite = FALSE)
}
\arguments{
  \item{xi_regions}{output list from sortinto.regions for observations xi}
  \item{xj_regions}{output list from sortinto.regions for calculation points xj}
  \item{x}{vector of values to be sorted}
  \item{b}{vector of boundary locations of the m regions}
  \item{finite}{set to TRUE if region boundaries are defined on a finite domain}
}
\details{
  The sortinto.regions function sorts the values in the vector x into m regions
  based on the boundaries defined in b.  Each x value will only be associated
  with one region m.  This function produces a list whose components are the
  regions containing a vector of all the values belonging to that region.  The
  default case uses an infinite domain (-inf, inf).  To use a finite domain,
  set finite to TRUE.  This will produce a list of regions on [b_min, b_max].

  The region.bandwidth function takes up to two output lists from the
  sortinto.regions function and computes optimal bandwidths in each of the
  regions.  The first list, representing the observations xi, is always
  required as it is used to compute the regional bandwidths.  The second list,
  representing the calculation points xj, is optional.  If included, this will
  assign regional bandwidths to both the xi and xj values.  If the latter option
  is used, make sure that the region boundaries are the same for xi and xj.

  The region bandwidth for a region r is computed from the equation:\cr\cr
  h(r)= 1.06 * sd(r) * N(r)^(-1/5)\cr\cr
  where sd(r) is the standard deviation of the N(r) observations within
  the region r.
}
\value{
  A list of (xi, hi) and/or (xj, hj) vector pairs is returned by the
  region.bandwidth function, where hi[i] is the optimal regional bandwidth for
  the observation xi[i], and hj[j] is the optimal regional bandwidth for the
  calculation point xj[j].
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
  will reduce the sample size for xi and/or the number of calculation points
  xj that are returned.
}
\examples{
# This example assigns regional bandwidths to the observations xi based on
# an infinite domain.
x <- runif(10);
b <- c(0.5);
regions <- sortinto.regions(x,b)
regions
region.bandwidth(regions)

# This example assigns regional bandwidths to both the observations xi and
# the calculation points xj based on a finite domain.
xi <- runif(10);
xj <- c(seq(0,1,by=0.1))
b <- c(0.2,0.5,1.0);
xi_regions <- sortinto.regions(xi,b,finite=TRUE)
xi_regions
xj_regions <- sortinto.regions(xj,b,finite=TRUE)
xj_regions
region.bandwidth(xi_regions, xj_regions)
}
