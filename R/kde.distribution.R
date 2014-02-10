kde.distribution <- function(kernel, x, xi, hi, lbound = FALSE, rbound = FALSE)
{
    H = length(hi)      # number of bandwidths used
    N = length(xi)      # number of observations used in the estimation
    m = length(x)       # number of calculation points in the domain
    fx = c(rep(0,m))    # initialize fx with zeroes

    # sort the vector x and get the min/max values of the domain
    x = sort(x)
    xmin = x[1]
    xmax = x[length(x)]

    # define interior domain point boundaries if using global bandwidth
    # (interval of points that do not require a boundary correction)
    if (H == 1)
    {
        h = hi
        xleft = xmin + h
        xright = xmax - h
    }

    # compute the sum over observations xi for each of the x values
    for (i in 1:N)
    {
        # modify bandwidth and boundaries if using region bandwidth
        if (H > 1)
        {
            h = hi[i]
            xleft = xmin + h
            xright = xmax - h  
        }

        # iterate over the x values
        for (j in 1:m)
        {  
            u = (x[j] - xi[i]) / h           

            # evaluate kernel only if u is in the interval [-1,1]
            # Note: this significantly speeds up the code as zero
            # contributions will be ignored
            cutoff = 1 + 1e-10

            if (abs(u) < cutoff)
            {
                if (lbound == TRUE & x[j] < xleft)
                {
                    p = (x[j] - xmin) / h
                    fx[j] = fx[j] + kernel.boundary(kernel,"left",p,u) / h
                }
                else if (rbound == TRUE & x[j] > xright)
                {
                    p = (x[j] - xmax) / h
                    fx[j] = fx[j] + kernel.boundary(kernel,"right",p,u) / h
                }
                else # interior calculation point
                {
                    fx[j] = fx[j] + kernel(u) / h
                }
            }
        }
    }

    # adjust f(x) by the number of observations N
    fx = fx / N

    return(fx)
}
