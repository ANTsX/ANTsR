Atropos <- function(d, a, x, i = "kmeans[3]", m = "[0.2,1x1]", c = "[5,0]", priorweight = 0.5, 
  ...) {
  if (typeof(d) == "list") {
    .Call("Atropos", int_antsProcessArguments(d), PACKAGE = "ANTsR")
    return(0)
  }
  if (missing(d) | missing(a) | missing(x)) {
    print("Input error - check params & usage")
    .Call("Atropos", int_antsProcessArguments(c(list("-h"))), PACKAGE = "ANTsR")
    return(NULL)
  }
  # define the output temp files
  tdir <- tempdir()
  probs <- tempfile(pattern = "antsr", tmpdir = tdir, fileext = "prob%02d.nii.gz")
  probsbase <- basename(probs)
  searchpattern <- sub("%02d", "*", probsbase)
  # categorize the initialization type - kmeans or spatial priors
  ct <- 1
  if (length(i) > 1) {
    # then spatial priors
    
    while (ct <= length(i)) {
      probchar <- paste(ct, sep = "")
      if (ct < 10) 
        probchar <- paste("0", probchar, sep = "")
      tempfn <- sub("%02d", probchar, probs)
      antsImageWrite(i[[ct]], tempfn)
      ct <- ct + 1
    }
    i <- paste("PriorProbabilityImages[", length(i), ",", probs, ",", priorweight, 
      "]", sep = "")
    print(i)
  }
  if (typeof(a) == "list") 
    outimg <- antsImageClone(a[[1]], "unsigned int") else outimg <- antsImageClone(a, "unsigned int")
  mydim <- as.numeric(outimg@dimension)
  outs <- paste("[", antsrGetPointerName(outimg), ",", probs, "]", sep = "")
  mymask <- antsImageClone(x, "unsigned int")
  if (length(a) == 1) 
    myargs <- list(d = d, a = a, m = m, o = outs, c = c, m = m, i = i, x = mymask, 
      ...)
  if (length(a) == 2) 
    myargs <- list(d = d, a = a[[1]], a = a[[2]], m = m, o = outs, c = c, m = m, 
      i = i, x = mymask, ...)
  if (length(a) == 3) 
    myargs <- list(d = d, a = a[[1]], a = a[[2]], a = a[[3]], m = m, o = outs, 
      c = c, m = m, i = i, x = mymask, ...)
  if (length(a) > 3) {
    myargs <- list(d = d, a = a[[1]], a = a[[2]], a = a[[3]], m = m, o = outs, 
      c = c, m = m, i = i, x = mymask, ...)
    print(" more than 3 input images not really supported, using first 3 ")
  }
  .Call("Atropos", int_antsProcessArguments(c(myargs)), PACKAGE = "ANTsR")
  probsout <- list.files(path = tdir, pattern = glob2rx(searchpattern), full.names = TRUE, 
    recursive = FALSE)
  pimg <- antsImageRead(probsout[1], mydim)
  probimgs <- c(pimg)
  for (x in c(2:length(probsout))) {
    probimgs <- c(probimgs, antsImageRead(probsout[x], mydim))
  }
  return(list(segmentation = outimg, probabilityimages = probimgs))
} 
