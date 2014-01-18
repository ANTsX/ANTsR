eigSeg <- function(mask = NA, imgList = NA, applySegmentationToImages=TRUE ) {
  if (typeof(mask) != "S4") {
    print(args(eigSeg))
    return(1)
  }
  maskvox <- (mask > 0)
  maskseg <- antsImageClone(mask)
  maskseg[maskvox] <- 0
  if (length(imgList) > 0) {
    if (typeof(imgList) == "list") 
      mydata <- imageListToMatrix(imgList, mask)
    if (typeof(imgList) == "character") 
      mydata <- imagesToMatrix(imgList, mask)
    segids <- apply(abs(mydata), 2, which.max)
    segmax <- apply(abs(mydata), 2, max)
    maskseg[maskvox] <- (segids * (segmax > 1e-09))
    print(max(segmax))
    print(max(segids))
    if ( applySegmentationToImages  )
      {
        for ( i in 1:length(imgList) )
          {
          img<-imgList[[i]]
          img[ maskseg != as.numeric(i) ]<-0
          imgList[[i]]<-img
          }
      }
    return(maskseg)
  } else print("No images in list")
} 


matrixSeg <- function(  mydatamatrix ) {
  segids <- apply(abs(mydatamatrix), 2, which.max)
  segmax <- apply(abs(mydatamatrix), 2, max)
  binmat<-mydatamatrix*0
  for ( i in 1:ncol(binmat) ) binmat[segids[i],i]<-1
  return(mydatamatrix*binmat)
} 
