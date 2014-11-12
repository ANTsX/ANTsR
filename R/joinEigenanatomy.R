joinEigenanatomy <- function(datamatrix, mask = NA, list_of_eanat_images, graphdensity = 0.65) {
  if (nargs() == 0) {
    print("Usage: ")
    print(args(joinEigenanatomy))
    return(1)
  }
  pckg <- try(require(igraph))
  if (!pckg) {
    getPckg("igraph")
  }
  library(igraph)
  if (!is.na(mask))
    decom <- imageListToMatrix(list_of_eanat_images, mask) else decom <- t(list_of_eanat_images)
  myproj <- datamatrix %*% t(decom)
  mycor <- cor(myproj)
  costs <- rep(NA, length(graphdensity))
  ct <- 1
  for (gd in graphdensity) {
    gg <- makeGraph(mycor, gd)
    communitymembership <- gg$walktrapcomm$membership
    if (!is.na(mask)) {
      newelist <- list()
      for (cl in 1:max(communitymembership)) {
        newe <- antsImageClone(list_of_eanat_images[[1]])
        newe[mask > 0] <- 0
        templist <- list_of_eanat_images[communitymembership == cl]
        for (eimg in templist) {
          newe[mask > 0] <- newe[mask > 0] + eimg[mask > 0]/sum(eimg[mask >
          0])
          print(sum(newe > 0)/sum(mask > 0))
        }
        newe[mask > 0] <- newe[mask > 0]/sum(newe[mask > 0])
        newelist <- lappend(newelist, newe)
      }
      decom2 <- imageListToMatrix(newelist, mask)
    }
    if (is.na(mask)) {
      newdecom <- matrix(rep(0, ncol(datamatrix) * max(communitymembership)),
        nrow = max(communitymembership))
      for (cl in 1:max(communitymembership)) {
        vec <- rep(0, ncol(datamatrix))
        ntosum <- sum(communitymembership == cl)
        tempmat <- decom[communitymembership == cl, ]
        if (ntosum > 1)
          tempvec <- apply(tempmat, FUN = sum, MARGIN = 2) else tempvec <- tempmat
        tempvec <- tempvec/sum(abs(tempvec))
        newdecom[cl, ] <- tempvec
      }
      decom2 <- newdecom
      newelist <- decom2
    }
    myproj2 <- datamatrix %*% t(decom2)
    cm <- cor(myproj2)
    costs[ct] <- mean(abs(cm[upper.tri(cm)]))
    ct <- ct + 1
  }
  myfavoritecost<-1.05*min(costs)
  if (length(graphdensity) > 1) {
    return( joinEigenanatomy(datamatrix, mask, list_of_eanat_images, graphdensity[  which( costs <= myfavoritecost )[1]  ] ) )
  }
  myproj <- datamatrix %*% t(decom2)
  colnames(myproj) <- paste("V", 1:ncol(myproj), sep = "")
  return(list(fusedlist = newelist, fusedproj = myproj, memberships = communitymembership,
    graph = gg, bestdensity = graphdensity))
}
