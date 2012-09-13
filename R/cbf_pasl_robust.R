cbf_pasl_robust <- function( asl_img, m0_img, labelfirst=1, robust=TRUE )
{
  moco_results <- motion_correction( asl_img )

  moco_results_m0 <- motion_correction( m0_img, fixed=moco_results$moco_avg_img )

  moco_mask_img <- get_mask( moco_results$moco_avg_img , thresh_lo = 500 , thresh_hi = 1e9 )

  mat <- timeseries2matrix( moco_results$moco_img , moco_mask_img )

  motionparams <- as.data.frame( moco_results$moco_params )

  predictors <- get_perfusion_predictors( as.matrix(mat) , motionparams, labelfirst=labelfirst )

  deltaM <- perfusionregression( moco_mask_img, mat , predictors$xideal , predictors$nuis, robust )

  lambda <- 0.9
  alpha <- 0.95
  TI1 <- 700
  TI2 <- 1700
  T1b <- 1664
  
  values <- ( lambda * as.array(deltaM) ) / ( 2 * alpha * as.array(moco_results_m0$moco_avg_img) * TI1 * exp( -TI2 / T1b ) )

  dim(values) <- dim( deltaM )

  return(as.antsImage(values))
}
