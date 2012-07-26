# Wang 2012 paper CASL ; paper does not mention the parameters so using them form the reference [25] mentioned in the paper
# --------------------------------------------------------------------------------------
cbf_casl4 <- function( aslimg_filename , Xvar )
{
	img <- as.array( antsImageRead(  aslimg_filename , "double" , 4 ) )

	numdiffs <- floor( dim(img)[4] / 2 )
	Xideal <- c( -0.5 , 0.5 )
	Xideal <- rep( Xideal , length.out = dim(img)[4] )
	cbfform <- formula( Y ~ Xideal + Xvar )

	cbf <- array( 0 , dim(img)[1:3] )
	lambda <- 0.9     # stolen
	T1a <- 1.6        # stolen
	R1a <- 1 / T1a    # stolen
	w <- 1
	alpha <- 0.95     # stolen
	tau <- 2

	for( i in 1:(dim(img)[1]) )
	     for( j in 1:(dim(img)[2]) )
	     	  for( k in 1:(dim(img)[3]) )
		  {
			Y <- img[ i , j , k , ]
			cbfmodel <- lm( cbfform )
			c <- mean( Y )
			bideal <- (cbfmodel$coefficients)[2]
			cbf[ i , j , k ] <- ( bideal * lambda * R1a * exp( w * R1a ) ) / ( 2 * c * alpha * ( 1 - exp( -tau * R1a ) ) )
		  }

	return( cbf )
}
