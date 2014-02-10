region.bandwidth <- function(regions)
{
    m = length(regions)   # number of distinct regions

    # create empty vectors to store xi and hi values
    xi = NULL
    hi = NULL  

    for (r in 1:m)
    {
        # get all values in region r
        region = regions[[r]]
        n = length(region)

        if (n > 1)
        {
            # compute bandwidth h for region r
            region.sd = sd(region)
            region.size = length(region)
            hr = 1.06 * region.sd * region.size^(-0.2)
 
            # add values in region and their bandwidth value to xi & hi
            xi = append(xi, region)
            hi = append(hi, c(rep(hr, region.size)))
        }
    }

    # return list of xi and hi vectors
    h.list = list(xi, hi)
    names(h.list) <- c("xi", "hi")

    return(h.list)
}
