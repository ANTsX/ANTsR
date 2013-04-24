cvEigenanatomy <- function(demog, images, outcome, ratio=10, mask=NA, sparseness=0.01, nvecs=50, its=5, cthresh=250, ...){
  if (ratio < 1){
    demog.split <- splitData(demog, ratio, return.rows=TRUE)
    mydecom <- sparseDecom(images[demog.split$rows.in, ], mask, sparseness, nvecs, its, cthresh)
    result <- regressProjections(images[demog.split$rows.in, ], images[demog.split$rows.out, ], 
				 demog.split$data.in, demog.split$data.out, 
				 mydecom$eigenanatomyimages, mask, outcome, ...)
  } else {
    demog.split <- splitData(demog, ratio, return.rows=TRUE)
    result <- list()
    for (i in 1:ratio){
      mydecom <- sparseDecom(images[demog.split[[i]]$rows.in, ], mask, sparseness, 
			     nvecs, its, cthresh)
      result[[paste("fold", i, sep='')]] <- regressProjections(
					images[demog.split[[i]]$rows.in, ], 
					images[demog.split[[i]]$rows.out, ], 
					demog.split[[i]]$data.in, 
					demog.split[[i]]$data.out, 
					mydecom$eigenanatomyimages, mask, outcome, ...)
    }
  }
  return(result)
}
