cbf_pasl_robust <- function(asl_img, m0img, labelfirst = 0, robust = TRUE, lambda = 0.9, 
  alpha = 0.95, TI1 = 700, TI2 = 1700, T1b = 1664) {
  
  moco_results <- motion_correction(asl_img)
  moco_results_m0 <- motion_correction(m0img, fixed = moco_results$moco_avg_img)
  moco_mask_img <- get_mask(moco_results$moco_avg_img, lowThresh = 500, highThresh = 1e+09, 
    cleanup = TRUE)
  mat <- timeseries2matrix(moco_results$moco_img, moco_mask_img)
  motionparams <- as.data.frame(moco_results$moco_params)
  predictors <- get_perfusion_predictors(as.matrix(mat), motionparams, labelfirst = labelfirst)
  deltaM <- perfusionregression(moco_mask_img, mat, predictors$xideal, predictors$nuis, 
    moco_results_m0$moco_avg_img, robust)
  
  # Return cbf in mL/100g/min
  cbf <- new("antsImage", dimension = 3, pixeltype = "float")
  ImageMath(3, cbf, "/", deltaM, moco_results_m0$moco_avg_img)
  scale <- 540000 * lambda/700 * (2 * alpha * TI1 * exp(-TI2/T1b))
  ImageMath(3, cbf, "m", cbf, scale)
  
  return(cbf)
  
} 
