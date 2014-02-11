sortinto.regions <- function(x, b, finite = FALSE)
{
    # determine number of distinct regions (infinite domain is default)
    m = length(b) + 1

    if (finite)
    {
        m = length(b) - 1
    }

    tol = 1e-10       # tolerance to account for truncation errors

    # set up an empty list of m regions
    regions = vector("list", m)

    # make sure that the vector b is sorted in increasing order
    b = sort(b)

    # sort the x values into their appropriate regions
    for (r in 1:m)
    {
        values = NULL  # default assumes no x values in region r

        if (r == m)
        {
            # get all values in last region
            if (finite)
            {
                values = x[which(x <= b[m+1] + tol)]
                values = values[which(values >= b[m] - tol)]
            }
            else  # infinite
            {
                values = x[which(x >= b[m-1] - tol)]
            }
        }
        else if (r == 1)
        {
            # get all values in first region
            if (finite)
            {
                values = x[which(x < b[2])]
                values = values[which(values >= b[1] - tol)]
            }
            else  # infinite
            {
                values = x[which(x < b[1])]
            }
        }
        else  # interior region
        {
            # get all values in the rth interior region
            if (finite)
            {
                values = x[which(x < b[r+1])]
                values = values[which(values >= b[r] - tol)]
            }
            else  # infinite
            {
                values = x[which(x < b[r])]
                values = values[which(values >= b[r-1] - tol)]
            }
        }

        if (length(values) != 0)
        {
            regions[[r]] = values
        }
        else
        {
            regions[[r]] = NULL
        }
    }

    return(regions)
}
