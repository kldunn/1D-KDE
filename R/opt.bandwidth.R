opt.bandwidth <- function(xi)
{
    n = length(xi)       # size of the sample xi
    h = NULL             # bandwidth value

    if (n > 1)
    {
        h = 1.06 * sd(xi) * n^(-0.2)
    }

    return(h)
}
