compute_cbf <- function( cbf , img , method = 0 , labelfirst = 1 )
{
casl_Chen2011 <- function( cbf , img , labelfirst )
{
	numdiffs <- floor( dim(img)[4] / 2 )

	if( labelfirst )
	{
	controlimg <- img[ , , , seq( 2 , by = 2 , length.out = numdiffs ) ]
	}
	else
	{
	controlimg <- img[ , , , seq( 1 , by = 2 , length.out = numdiffs ) ]
	}
	lambda <- 0.9
	alpha <- 0.68

	dim( controlimg ) <- c( dim(img)[1] * dim(img)[2] * dim(img)[3] , numdiffs )
	M0 <- rowMeans( controlimg )
	dim( M0 ) <- dim( img )[1:3]

	T1b <- 1664
	omega <- 1
	tau <- 2

	cbf <- ( lambda * cbf ) / ( 2 * alpha * M0 * T1b * ( exp( -omega / T1b ) - exp( -( tau + omega ) / T1b ) ) )

	return( cbf )
}

pasl_Chen2011 <- function( cbf , m0img , labelfirst )
{
	lambda <- 0.9
	alpha <- 0.95
	TI1 <- 700
	TI2 <- 1700
	T1b <- 1664

	cbf <- ( lambda * cbf ) / ( 2 * alpha * M0 * TI1 * exp( -TI2 / T1b ) )

	return( cbf )
}

if( is.character( img ) )
{
  img <- antsImageRead( img ,  4 )
}else if( class( img ) == "antsImage" )
{
  if( img@dimension != 4 ) 
  {
    print( "'img' must have dimension '4'" )
    return( NULL )
  }
}else
{
  print( "'img' must be a filename or an 'antsImage'" )
  return( NULL )
}

if( is.character( cbf ) )
{
  cbf <- antsImageRead( cbf , 4 )
}else if( class( cbf ) == "antsImage" )
{
  if( cbf@dimension != 3 ) 
  {
    print( "'cbf' must have dimension '3'" )
    return( NULL )
  }
}else
{
  print( "'cbf' must be a filename or an 'antsImage'" )
  return( NULL )
}

if( method == 0 )
{
return( casl_Chen2011( as.array( cbf ) , as.array( img ) , labelfirst ) )
}else if( method == 1 )
{
return( pasl_Chen2011( as.array( cbf ) , as.array( img ) , labelfirst ) )
}

}
