ants_compcorr_group <- function(moco_img = "", bm_img = "", cortmask_img = "", mocoparams_csv = "", 
  dosvd = 0) {
  if (nchar(moco_img) == 0) {
    print(" please set a valid moco image ")
    print(" usage : ants_compcorr_group( <moco-image> , <brain-mask-image> , <cortmask-image> , <mocoparams-csv> , <dosvd> ) ")
    return
  }
  if (nchar(bm_img) == 0) {
    print(" please set a valid brain-mask image ")
    print(" usage : ants_compcorr_group( <moco-image> , <brain-mask-image> , <cortmask-image> , <mocoparams-csv> , <dosvd> ) ")
    return
  }
  if (nchar(cortmask_img) == 0) {
    print(" please set a valid cortmask image ")
    print(" usage : ants_compcorr_group( <moco-image> , <brain-mask-image> , <cortmask-image> , <mocoparams-csv> , <dosvd> ) ")
    return
  }
  if (nchar(mocoparams_csv) == 0) {
    print(" please set a valid Moco-params csv file ")
    print(" usage : ants_compcorr_group( <moco-image> , <brain-mask-image> , <cortmask-image> , <mocoparams-csv> , <dosvd> ) ")
    return
  }
  
  # split the string into filename and extension
  split_img <- strsplit(moco_img, ".", fixed = TRUE)[[1]]
  filename <- split_img[1]
  if (length(split_img) == 2) {
    extension <- paste("", split_img[2], sep = ".")
  } else if (length(split_img) == 3) {
    extension <- paste("", split_img[2], split_img[3], sep = ".")
  }
  
  compcorr_img <- paste(filename, "_compcorr", extension, sep = "")
  ImageMath(4, compcorr_img, "CompCorrAuto", moco_img, bm_img, 3)
  
  csv <- paste(filename, ".csv", sep = "")
  sccan("--timeseriesimage-to-matrix", paste("[", paste(moco_img, cortmask_img, 
    3, 0, sep = ","), "]", sep = ""), "-o", csv)
  
  compcorr_csv <- paste(filename, "_compcorr_compcorr", ".csv", sep = "")
  filt_csv <- paste(filename, "_filt", ".csv", sep = "")
  antsr_frequency_filter(csv, filt_csv, 3, 0.1, 0.25, compcorr_csv)
  
  if (dosvd == 1) {
    RSF_Networks_img <- paste(filename, "_RSF_Networks", extension, sep = "")
    sccan("--sparse-svd", paste("[", paste(filt_csv, cortmask_img, -0.05, sep = ","), 
      "]", sep = ""), "-n", 40, "-i", 40, "--PClusterThresh", 100, "-o", RSF_Networks_img)
    
    ea_rsf <- paste(filename, "_ea_rsf", sep = "")
    RSF_NetworksprojectionsView1vec_csv <- paste(filename, "_RSF_NetworksprojectionsView1vec", 
      ".csv", sep = "")
    antsr_resting_state_corr_eigenanat(ea_rsf, RSF_NetworksprojectionsView1vec_csv, 
      RSF_NetworksprojectionsView1vec_csv, compcorr_csv, mocoparams_csv)
  }
} 
