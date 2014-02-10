sortinto.regions <-
function(x,b)
{

  m <- length(b)+1   # number of distinct regions
  tol <- 1e-10       # tolerance to account for truncation errors

  # set up an empty list of m regions
  regions <- vector( "list", m )

  # make sure that the vector b is sorted in increasing order
  b <- sort(b)

  # sort the x values into their appropriate regions
  for ( r in 1:m ) {

    values <- NULL  # default assumes no x values in region r

    if ( r == 1 ) {

      # first region
      i <- which( x < b[1] )
      if ( length(i) != 0 )
        values <- x[ i ]

    }
    else if ( r == m ) {
      
      # last region
      i <- which( x >= b[m-1] - tol )
      if ( length(i) != 0 ) 
        values <- x[ i ]

    }
    else {

      # get all x that are less than the right boundary
      i <- which( x < b[r] )
      if ( length(i) != 0 )
        values <- x[ i ]
      
      # get all x that are greater than or equal to the left boundary
      if ( !is.null( values ) ) {

        i <- which( values >= b[r-1] - tol )      
      
        if ( length(i) != 0 )
          values <- values[ i ]
        else
          values <- NULL

      }
      
    }

    if ( !is.null(values) )
      regions[[r]] <- values

  }

  return(regions)

}

