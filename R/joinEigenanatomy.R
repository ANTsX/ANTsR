#' Simple joinEigenanatomy function.
#'
#' joinEigenanatomy joins the input matrix using a community membership
#' approach.
#'
#'
#' @param datamatrix input matrix before decomposition
#' @param mask mask used to create datamatrix
#' @param listEanatImages list containing pointers to eanat images
#' @param graphdensity target graph density or densities to search over
#' @param joinMethod see igraph's community detection
#' @param verbose bool
#' @return return(list(fusedlist = newelist, fusedproj = myproj, memberships =
#' communitymembership , graph=gg, bestdensity=graphdensity ))
#' @author Avants BB
#' @examples
#' \dontrun{
#' # if you dont have images
#' mat <- replicate(100, rnorm(20))
#' mydecom <- sparseDecom(mat)
#' kk <- joinEigenanatomy(mat, mask = NULL, mydecom$eigenanatomyimages, 0.1)
#' # or select optimal parameter from a list
#' kk <- joinEigenanatomy(mat, mask = NULL, mydecom$eigenanatomyimages, c(1:10) / 50)
#' # something similar may be done with images
#' mask <- as.antsImage(t(as.matrix(array(rep(1, ncol(mat)), ncol(mat)))))
#' mydecom <- sparseDecom(mat, inmask = mask)
#' eanatimages <- matrixToImages(mydecom$eigenanatomyimages, mask)
#' kki <- joinEigenanatomy(mat, mask = mask, eanatimages, 0.1)
#' if (usePkg("igraph")) {
#'   mydecomf <- sparseDecom(mat,
#'     inmask = mask, initializationList = kki$fusedlist,
#'     sparseness = 0, nvecs = length(kki$fusedlist)
#'   )
#' }
#' }
#' @export joinEigenanatomy
joinEigenanatomy <- function(
    datamatrix, mask = NULL, listEanatImages,
    graphdensity = 0.65,
    joinMethod = "walktrap", verbose = F) {
  if (nargs() == 0) {
    print("Usage: ")
    print(args(joinEigenanatomy))
    return(1)
  }
  if (!usePkg("igraph")) {
    print("Need igraph package")
    return(NULL)
  }
  if (!is.null(mask)) {
    mask <- check_ants(mask)
    decom <- imageListToMatrix(listEanatImages, mask)
  } else {
    decom <- (listEanatImages)
  }
  for (i in 1:nrow(decom)) {
    if (min(decom[i, ]) < 0 & max(decom[i, ]) == 0) {
      decom[i, ] <- decom[i, ] * (-1)
    }
  }
  myproj <- datamatrix %*% t(decom)
  mycor <- cor(myproj)
  costs <- rep(NA, length(graphdensity))
  ct <- 1
  for (gd in graphdensity) {
    gg <- makeGraph(mycor, gd, communityMethod = joinMethod)
    communitymembership <- gg$community$membership
    if (!is.null(mask)) {
      newelist <- list()
      for (cl in 1:max(communitymembership)) {
        newe <- antsImageClone(listEanatImages[[1]])
        newe[mask > 0] <- 0
        templist <- listEanatImages[communitymembership == cl]
        for (eimg in templist) {
          newe[mask > 0] <- newe[mask > 0] + eimg[mask > 0] / sum(eimg[mask >
            0])
          # print(sum(newe > 0)/sum(mask > 0))
        }
        newe[mask > 0] <- newe[mask > 0] / sum(newe[mask > 0])
        newelist <- lappend(newelist, newe)
      }
      decom2 <- imageListToMatrix(newelist, mask)
    }
    if (is.null(mask)) {
      newdecom <- matrix(rep(0, ncol(datamatrix) * max(communitymembership)),
        nrow = max(communitymembership)
      )
      for (cl in 1:max(communitymembership)) {
        vec <- rep(0, ncol(datamatrix))
        ntosum <- sum(communitymembership == cl)
        tempmat <- decom[communitymembership == cl, ]
        if (ntosum > 1) {
          tempvec <- apply(tempmat, FUN = sum, MARGIN = 2)
        } else {
          tempvec <- tempmat
        }
        tempvec <- tempvec / sum(abs(tempvec))
        newdecom[cl, ] <- tempvec
      }
      decom2 <- newdecom
      newelist <- decom2
    }
    myproj2 <- datamatrix %*% t(decom2)
    cm <- cor(myproj2)
    diag(cm) <- 0
    costs[ct] <- mean(abs(cm[upper.tri(cm)]))
    ct <- ct + 1
  }
  myfavoritecost <- min(costs)
  if (verbose) {
    print(paste("costs"))
    print(costs)
    print(paste("myfavoritecost", myfavoritecost))
  }
  if (length(graphdensity) > 1) {
    return(joinEigenanatomy(datamatrix, mask, listEanatImages, graphdensity[which.min(costs)]))
  }
  myproj <- datamatrix %*% t(decom2)
  colnames(myproj) <- paste("V", 1:ncol(myproj), sep = "")
  return(list(
    fusedlist = newelist, fusedproj = myproj, memberships = communitymembership,
    graph = gg, bestdensity = graphdensity
  ))
}
