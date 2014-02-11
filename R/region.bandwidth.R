region.bandwidth <- function(xi_regions, xj_regions = NULL)
{
    m_xi = length(xi_regions)   # number of regions for observations
    m_xj = length(xj_regions)   # number of regions for calculation points

    # create empty vectors to store (xi, hi) and (xj, hj) pairs
    xi = NULL
    hi = NULL
    xj = NULL
    hj = NULL

    # iterate through regions (assumes xi and xj use same region boundaries)
    for (r in 1:m_xi)
    {
        # get all xi values in region r
        xi_region = xi_regions[[r]]
        xi_region.size = length(xi_region)

        if (xi_region.size > 1)
        {
            # compute bandwidth h for region r
            xi_region.sd = sd(xi_region)
            hr = 1.06 * xi_region.sd * xi_region.size^(-0.2)
 
            # add values in xi_region and their bandwidths to xi & hi
            xi = append(xi, xi_region)
            hi = append(hi, c(rep(hr, xi_region.size)))

            # check values in xj_regions
            if (!is.null(xj_regions) & m_xi == m_xj)
            {
                # get all xj values in region r
                xj_region = xj_regions[[r]]

                # add values in xj_region and their bandwidths to xj & hj
                xj = append(xj, xj_region)
                hj = append(hj, c(rep(hr, length(xj_region))))
            }
        }
    }

    # return list of (xi, hi) and (xj, hj) vector pairs
    h.list = list(xi, hi, xj, hj)
    names(h.list) = c("xi", "hi", "xj", "hj")

    return(h.list)
}
