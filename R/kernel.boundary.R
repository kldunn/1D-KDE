kernel.boundary <- function(kernel, boundary, p, u)
{
    n = length(u)        # get number of evaluations to perform
    kbu = c(rep(0,n))    # initialize kbu with zeroes

    # create functions needed for the integrals ai(p)
    f1 = function(u) { return(u * kernel(u)) }
    f2 = function(u) { return(u^2 * kernel(u)) }

    # determine the ai(p) integral values
    if (boundary == "left")
    {
        a0 = integrate(kernel, -1, p)$value
        a1 = integrate(f1, -1, p)$value
        a2 = integrate(f2, -1, p)$value 
    }
    else
    {
        a0 = integrate(kernel, p, 1)$value
        a1 = integrate(f1, p, 1)$value
        a2 = integrate(f2, p, 1)$value
    }

    # compute boundary kernel values
    for (i in 1:n)
    {
        kbu[i] = (a2 - a1 * u[i]) * kernel(u[i]) / (a0*a2 - a1^2)
    }

    return(kbu)
}
