cbf_robust <- function(asl_img) {
  moco_results <- motion_correction(asl_img)
  
  moco_mask_img <- getMask(moco_results$moco_avg_img, lowThresh = 500, highThresh = 1e+09, 
    cleanup = TRUE)
  
  mat <- timeseries2matrix(moco_results$moco_img, moco_mask_img)
  
  predictors <- get_perfusion_predictors(mat, moco_results$moco_params)
  
  cbf <- perfusionregression(moco_mask_img, mat, predictors$xideal, predictors$nuis)
  
  cbf <- compute_cbf(cbf, asl_img)
  
  return(cbf)
} 
