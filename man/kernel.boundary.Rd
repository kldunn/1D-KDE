\name{kernel.boundary}
\alias{kernel.boundary}
\title{Boundary Correction Kernel}
\description{
  The kernel.boundary function evaluates the Boundary Correction Kernel.
}
\usage{kernel.boundary(kernel, boundary, p, u)}
\arguments{
  \item{kernel}{any kernel function k(u)}
  \item{boundary}{choose "left" or "right" boundary}
  \item{p}{argument for the integral functions ai(p)}
  \item{u}{vector of values for which the kernel is to be evaluated}
}
\details{
  The Boundary Correction Kernel is computed using the following equation:\cr\cr
  kb(u) = [ (a2(p) - a1(p)*u) * k(u) ] / [ a0(p)*a2(p) - a1(p)^2 ]\cr\cr
  where ai(p) = integrate( u^i * k(u), lower, upper ).
}
\value{vector of boundary kernel evaluations kb(u)}
\references{
  K. Banerjee, "Kernel Density Estimator Methods for Monte Carlo
  Radiation Transport," Ph.D. Thesis, Nuclear Engineering and
  Radiological Sciences, University of Michigan (2010).
}
\author{Kerry Dunn, UW-Madison Nuclear Engineering PhD Student}
\note{
  The integration limits depend on whether a "left" or "right"
  boundary is used.  If "left" the integration will be performed
  over [-1,p], and if "right" it will be done over [p,1].    
}
\examples{
kernel <- kernel.epanechnikov;
 
p1 <- 0;
u1 <- c(seq(-1,p1,by=0.01));
kb1 <- kernel.boundary(kernel,"left",p1,u1);
plot(u1,kb1,type="l",col="blue",
     xlim=c(-1,1),ylim=c(-1,6),
     xlab="u",ylab="Boundary Kernel")
 
p2 <- 0.25;
u2 <- c(seq(-1,p2,by=0.01));
kb2 <- kernel.boundary(kernel,"left",p2,u2);
points(u2,kb2,type="l",col="red")

p3 <- 1;
u3 <- c(seq(-1,p3,by=0.01));
kb3 <- kernel.boundary(kernel,"left",p3,u3);
points(u3,kb3,type="l",col="green")

legend(0.5,6,c("p=0","p=0.25","p=1"),
       col=c("blue","red","green"),lty=1)
}
