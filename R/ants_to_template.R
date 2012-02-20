ants_to_template <- function( dim = 0 , fixed_img = "" , moving_img = "" , timeseries_img = "" , max_iterations = "30x90x20" )
{
	if( dim == 0 )
	{
		print( " Problem with specified image-dimension " ) ;
		print(" USAGE ::  " ) ;
		print( " ants_to_template(dim=<image-dimension>,fixed_img=<fixed-image>,moving_img=<moving-image>,timeseries_img=<time-series-image>,[max_iterations=<max-iterations>" ) ;
		print( " Max-Iterations in form :    JxKxL where " ) ;
		print( " J = max iterations at coarsest resolution (here, reduce by power of 2^2) " ) ;
		print( " K = middle resolution iterations ( here, reduce by power of 2 ) " ) ;
		print( " L = fine resolution iterations ( here, full resolution ) -- this level takes much more time per iteration " ) ;
		print( " an extra  Ix before JxKxL would add another level " ) ;
		print( " Default max-iterations is " , max_iterations , " --you can often get away with fewer for many apps " ) ;
		print( " Other parameters are updates of the defaults used in the A. Klein evaluation paper in Neuroimage, 2009 " ) ;
		return ;
	}

	if( nchar( fixed_img ) == 0 )
	{
		print( " Problem with specified Fixed Image " ) ;
		print(" USAGE ::  " ) ;
		print( " ants_to_template(dim=<image-dimension>,fixed_img=<fixed-image>,moving_img=<moving-image>,timeseries_img=<time-series-image>,[max_iterations=<max-iterations>" ) ;
		print( " Max-Iterations in form :    JxKxL where " ) ;
		print( " J = max iterations at coarsest resolution (here, reduce by power of 2^2) " ) ;
		print( " K = middle resolution iterations ( here, reduce by power of 2 ) " ) ;
		print( " L = fine resolution iterations ( here, full resolution ) -- this level takes much more time per iteration " ) ;
		print( " an extra  Ix before JxKxL would add another level " ) ;
		print( " Default max-iterations is " , max_iterations , " --you can often get away with fewer for many apps " ) ;
		print( " Other parameters are updates of the defaults used in the A. Klein evaluation paper in Neuroimage, 2009 " ) ;
		return ;
	}

	if( nchar( moving_img ) == 0 )
	{
		print( " Problem with specified Moving Image " ) ;
		print(" USAGE ::  " ) ;
		print( " ants_to_template(dim=<image-dimension>,fixed_img=<fixed-image>,moving_img=<moving-image>,timeseries_img=<time-series-image>,[max_iterations=<max-iterations>" ) ;
		print( " Max-Iterations in form :    JxKxL where " ) ;
		print( " J = max iterations at coarsest resolution (here, reduce by power of 2^2) " ) ;
		print( " K = middle resolution iterations ( here, reduce by power of 2 ) " ) ;
		print( " L = fine resolution iterations ( here, full resolution ) -- this level takes much more time per iteration " ) ;
		print( " an extra  Ix before JxKxL would add another level " ) ;
		print( " Default max-iterations is " , max_iterations , " --you can often get away with fewer for many apps " ) ;
		print( " Other parameters are updates of the defaults used in the A. Klein evaluation paper in Neuroimage, 2009 " ) ;
		return ;
	}

	if( nchar( timeseries_img ) == 0 )
	{
		print( " Problem with specified TimeSeries Image " ) ;
		print(" USAGE ::  " ) ;
		print( " ants_to_template(dim=<image-dimension>,fixed_img=<fixed-image>,moving_img=<moving-image>,timeseries_img=<time-series-image>,[max_iterations=<max-iterations>" ) ;
		print( " Max-Iterations in form :    JxKxL where " ) ;
		print( " J = max iterations at coarsest resolution (here, reduce by power of 2^2) " ) ;
		print( " K = middle resolution iterations ( here, reduce by power of 2 ) " ) ;
		print( " L = fine resolution iterations ( here, full resolution ) -- this level takes much more time per iteration " ) ;
		print( " an extra  Ix before JxKxL would add another level " ) ;
		print( " Default max-iterations is " , max_iterations , " --you can often get away with fewer for many apps " ) ;
		print( " Other parameters are updates of the defaults used in the A. Klein evaluation paper in Neuroimage, 2009 " ) ;
		return ;
	}

	# split the string into filename and extension
	split_img = strsplit( timeseries_img , "." , fixed = TRUE )[[1]] ;
	filename = split_img[1] ;
	if( length( split_img ) == 2 )
	{
		extension = paste( "" , split_img[2] , sep = "." ) ;
	}
	else if( length( split_img ) == 3 )
	{
		extension = paste( "" , split_img[2] , split_img[3] , sep = "." ) ;
	}
	
	ANTS( dim , "-m" , paste( "CC[" , paste( fixed_img , moving_img , 1 , 3 , sep = "," ) , "]" , sep = "" ) , "-t" , "SyN[0.25]" , "-r" , "Gauss[3,0]" , "-o" , filename , "--use-Histogram-Matching" , "-i" , max_iterations , "--number-of-affine-iterations" , "10000x10000x1000" ) ;

	deformed_img = paste( filename , "deformed" , extension , sep = "" ) ;
	warp_img = paste( filename , "Warp" , extension , sep = "" ) ;
	affine_txt = paste( filename , "Affine" , ".txt" , sep = "" ) ;
  	WarpTimeSeriesImageMultiTransform( 4 , moving_img , deformed_img , warp_img , affine_txt , "-R" , fixed_img ) ;
}
