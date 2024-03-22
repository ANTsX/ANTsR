.antsMotionCorrStats <- function(
  inimg, mask, mocoparams, stupidoff=2,
  num_threads = 1,
  seed = NULL) {
  
  ants_random_seed = itk_threads = NULL
  if (!is.null(seed)) {
    ants_random_seed = Sys.getenv("ANTS_RANDOM_SEED")
    Sys.setenv(ANTS_RANDOM_SEED = seed)    
  }
  if (!is.null(num_threads)) {
    itk_threads = Sys.getenv("ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS")
    Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = num_threads)
  }  
  on.exit({
    if (!is.null(ants_random_seed)) {
      Sys.setenv(ANTS_RANDOM_SEED = ants_random_seed)
    }
    if (!is.null(itk_threads)) {
      Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = itk_threads)
    }    
  })
  
  tsimg <- antsImageClone(inimg, "float")
  mocomat <- as.matrix(mocoparams)
  ANTsRCore::antsMotionCorrStats(tsimg, mask, mocomat, stupidoff)
}

.antsMotionCorrStats0 <- function(
  inimg, mask, mocoparams, stupidoff=0,
  num_threads = 1,
  seed = NULL) {
  
  ants_random_seed = itk_threads = NULL
  if (!is.null(seed)) {
    ants_random_seed = Sys.getenv("ANTS_RANDOM_SEED")
    Sys.setenv(ANTS_RANDOM_SEED = seed)    
  }
  if (!is.null(num_threads)) {
    itk_threads = Sys.getenv("ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS")
    Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = num_threads)
  }  
  on.exit({
    if (!is.null(ants_random_seed)) {
      Sys.setenv(ANTS_RANDOM_SEED = ants_random_seed)
    }
    if (!is.null(itk_threads)) {
      Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = itk_threads)
    }    
  })  
  
  tsimg <- antsImageClone(inimg, "float")
  mocomat <- as.matrix(mocoparams)
  ANTsRCore::antsMotionCorrStats(tsimg, mask, mocomat, stupidoff)
}
