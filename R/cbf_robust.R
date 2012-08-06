cbf_robust <- function( asl_img )
{
moco_results <- motion_correction( asl_img )

moco_mask_img <- get_mask( moco_results$moco_avg_img , thresh_lo = 500 , thresh_hi = 1e9 )

mat <- timeseries2matrix( moco_results$moco_img , moco_mask_img )

predictors <- get_perfusion_predictors( mat , moco_results$moco_params )

cbf <- perfussionregression( moco_mask_img, mat , predictors$xideal , predictors$nuis )

cbf <- compute_cbf( cbf , asl_img )

return( cbf )
}
