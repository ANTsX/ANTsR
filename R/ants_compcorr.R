ants_compcorr <- function( moco_img = "" , bm_img = "" , avg_img = "" , mocoparams_csv = "" )
{
	if( nchar( moco_img ) == 0 )
	{
		print( " please set a valid moco image " ) ;
		print( " usage : ants_compcorr <moco-image> <brain-mask-image> <average-image> <analysishome> " ) ;
		return ;
	}
	if( nchar( bm_img ) == 0 )
	{
		print( " please set a valid brain-mask image " ) ;
		print( " usage : ants_compcorr <moco-image> <brain-mask-image> <average-image> <analysishome> " ) ;
		return ;
	}
	if( nchar( avg_img ) == 0 )
	{
		print( " please set a valid average image " ) ;
		print( " usage : ants_compcorr <moco-image> <brain-mask-image> <average-image> <analysishome> " ) ;
		return ;
	}

	# split the string into filename and extension
	split_img = strsplit( moco_img , "." , fixed = TRUE )[[1]] ;
	filename = split_img[1] ;
	if( length( split_img ) == 2 )
	{
		extension = paste( "" , split_img[2] , sep = "." ) ;
	}
	else if( length( split_img ) == 3 )
	{
		extension = paste( "" , split_img[2] , split_img[3] , sep = "." ) ;
	}

	compcorr_img = paste( filename , "_compcorr" , extension , sep = "" ) ;
	ImageMath( 4 , compcorr_img , "CompCorrAuto" , moco_img , bm_img , 6 ) ;

	N3BiasFieldCorrection( 3 , avg_img , avg_img , 2 , bm_img ) ;
	for( x in 1:3 )
	{
		N3BiasFieldCorrection( 3 , avg_img , avg_img , 1 , bm_img ) ;
	}
	
	compcorr_variance_img = paste( filename , "_compcorr_variance" , extension , sep = "" ) ;
	seg_img = paste( filename , "_seg" , extension , sep = "" ) ;
	Atropos( "-d" , 3 , "-a" , avg_img , "-a" , compcorr_variance_img , "-m" , "[0.3,1x1x1]" , "-o" , seg_img , "-c" , "[5,0]" , "-i" , "kmeans[3]" , "-x" , bm_img ) ; 

	cortmask_img = paste( filename , "_cortmask" , extension , sep = "" ) ;
	ThresholdImage( 3 , seg_img , cortmask_img , 2 , 2 ) ;
	csv = paste( filename , ".csv" , sep = "" ) ;
	sccan( "--timeseriesimage-to-matrix" , paste( "[" , paste( moco_img , cortmask_img , 0.0 , 0.0 , sep = "," ) , "]" , sep = "" ) , "-o" , csv ) ;

	compcorr_csv = paste( filename , "_compcorr_compcorr" , ".csv" , sep = "" ) ;
	filt_csv = paste( filename , "_filt" , ".csv" , sep = "" ) ;
	RSF_Networks_img = paste( filename , "_RSF_Networks" , extension , sep = "" ) ;
	antsr_frequency_filter( csv , filt_csv , 1.5 , 0.03 , 0.10 , compcorr_csv ) ;
	
	sccan( "--sparse-svd" , paste( "[" , paste( filt_csv , cortmask_img , -0.15 , sep = "," ) , "]" , sep = "" ) , "-n" , 40 , "-i" , 40 , "--PClusterThresh" , 50 , "-o" , RSF_Networks_img ) ;
	
	ea_rsf = paste( filename , "_ea_rsf" , sep = "" ) ;
	RSF_NetworksprojectionsView1ve_csv = paste( filename , "_RSF_NetworksprojectionsView1vec" , ".csv" , sep = "" ) ;
	antsr_resting_state_corr_eigenanat( ea_rsf , RSF_NetworksprojectionsView1vec_csv , RSF_NetworksprojectionsView1vec_csv , compcorr_csv, mocoparams_csv ) ;
}
