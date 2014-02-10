kernel.epanechnikov <-
function(u)
{

  n <- length(u)       # get number of evaluations to perform
  ku <- c( rep(0,n) )  # initialize ku with zeroes

  for ( i in 1:n ) {

    if ( abs( u[i] ) < 1 )
      ku[i] <- 0.75 * ( 1 - u[i]^2 )
   
  }

  return(ku)

}

