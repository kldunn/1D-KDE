\name{opt.bandwidth}
\alias{opt.bandwidth}
\title{Optimal Bandwidth}
\description{
  The opt.bandwidth function determines the optimal bandwidth to use
  for a kernel density estimation of a probability distribution.
}
\usage{opt.bandwidth(xi)}
\arguments{
  \item{xi}{sample of N observations from the unknown distribution}
}
\details{
  The optimal bandwidth is computed using the following equation:\cr\cr
  h.opt = 1.06 * sd * N^(-1/5)\cr\cr
  where sd is the standard deviation of the N observations in the
  sample xi.
}
\value{returns the optimal bandwidth value h.opt}
\references{
  K. Banerjee, "Kernel Density Estimator Methods for Monte Carlo
  Radiation Transport," Ph.D. Thesis, Nuclear Engineering and
  Radiological Sciences, University of Michigan (2010).
}
\author{Kerry Dunn, UW-Madison Nuclear Engineering PhD Student}
\examples{
xi <- rnorm(10000);
opt.bandwidth(xi)
}
