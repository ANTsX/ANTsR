ants_motion_estimation <- function( img = "" )
{
	# check if called with no arguments and print usage
	if( nchar( img ) == 0 )
	{
		print( "usage: ants_motion_estimation( <time-series-image> )" ) ;
		return ;
	}

	# check if there is an extension
	if( length( strsplit( img , "." , fixed = TRUE )[[1]] ) < 2 )
	{
		print( "There appears to be no extension to the input file. Please provide a [nii|nii.gz] file." ) ;
		return ;
	}

	# split the string into filename and extension
	split_img = strsplit( img , "." , fixed = TRUE )[[1]] ;
	filename = split_img[1] ;
	if( length( split_img ) == 2 )
	{
		extension = paste( "" , split_img[2] , sep = "." ) ;
	}
	else if( length( split_img ) == 3 )
	{
		extension = paste( "" , split_img[2] , split_img[3] , sep = "." ) ;
	}

	# rigid
	avg_img = paste( filename , "_avg" , extension , sep = "" ) ;
	antsMotionCorr( "-d" , 3 , "-a" , img , "-o" , avg_img ) ;

	moco_img = paste( filename , "_moco" , extension , sep = "" ) ;
	antsMotionCorr( "-d" , 3 , "-o" , paste( "[" , paste( filename , moco_img , avg_img , sep = "," ) , "]" , sep = "" ) , "-m" , paste( "MI[" , paste( avg_img , img , 1 , 32 , 50 , sep = "," ) , "]" , sep = "" ) , "-t" , "Rigid[0.01]" , "-i" , 25 , "-u" , 1 , "-e" , 1 , "-s" , 0 , "-f" , 1 , "-n" , 25 ) ;

	# non-rigid
#	avgnr_img = paste( filename , "_avgnr" , extension , sep = "" ) ;
#	antsMotionCorr( "-d" , 3 , "-a" , img , "-o" , avgnr_img ) ;

#	moconr_img = paste( filename , "_moconr" , extension , sep = "" ) ;
#	antsMotionCorr( "-d" , 3 , "-o" , paste( "[" , paste( filename , moconr_img , avgnr_img , sep = "," ) , "]" , sep = "" ) , "-m" , paste( "MI[" , paste( avgnr_img , img , 1 , 20 , 50 , sep = "," ) , "]" , sep = "" ) , "-t" , "Rigid[0.01]" , "-i" , 25 , "-u" , 1 , "-e" , 1 , "-s" , 0 , "-f" , 1 , "-n" , 25 , "-m" , paste( "CC[" , paste( avgnr_img , img , 1 , 2 , sep = "," ) , "]" , sep = "" ) , "-t" , "GaussianDisplacementField[0.15,3,0.5]" , "-i" , 10 , "-u" , 1 , "-e" , 1 , "-s" , 0 , "-f" , 1 , "-n" , 10 ) ;
}
