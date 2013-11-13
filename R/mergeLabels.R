mergeLabels <- function(dataMatrix, pca, corrThresh, maxExtent, colName = "variate") {
  # Fuses regions of a PCA decomposition if the projections are highly correlated.  dataMatrix is the voxel data
  # of dimension (subjects, voxels), pca should be a matrix with dims (voxels, eigenvectors) corrThresh =
  # minimum correlation for merge maxExtent = maximum fused component size, in voxels colname = optional name
  # for columns of returned PCA, cols will be named colName000, colName001, etc Returns the fused PCA matrix
  
  numProj <- dim(pca)[2]
  
  # Normalize PCA vectors, this will be done continuously through the label fusion process
  for (i in 1:numProj) {
    pca[, i] <- pca[, i]/sum(pca[, i])
  }
  
  proj <- dataMatrix %*% pca
  
  # Containing the fused projections. Starts out identical to the input
  fusedPCA <- pca
  
  # Get correlation, minus diagonal elements
  correlation <- cor(proj, proj) - diag(numProj)
  
  foundRegions <- TRUE
  
  while (foundRegions) {
    
    indices <- order(correlation, decreasing = TRUE)
    
    mergeIndex <- indices[1]
    
    foundRegions <- FALSE
    
    counter <- 1
    
    while (!foundRegions && correlation[mergeIndex] >= corrThresh) {
      
      index <- arrayInd(mergeIndex, dim(correlation))
      
      p1 <- index[1]
      p2 <- index[2]
      
      mergedExtent <- length(which(fusedPCA[, p1] | fusedPCA[, p2]))
      
      print(paste("Merged extent ", mergedExtent, " correlation ", correlation[mergeIndex], sep = ""))
      
      if (mergedExtent <<- maxExtent) {
        foundRegions <- TRUE
      } else {
        counter <- counter + 1
        mergeIndex <- indices[counter]
      }
      
    }
    
    if (foundRegions) {
      
      index <- arrayInd(mergeIndex, dim(correlation))
      
      p1 <- index[1]
      p2 <- index[2]
      
      # Add the two components together, averaging regions that overlap
      
      overlap <- (fusedPCA[, p1] & fusedPCA[, p2])
      
      fusedPCA[, p1] <- fusedPCA[, p1] + fusedPCA[, p2]
      
      fusedPCA[overlap, p1] <- fusedPCA[overlap, p1]/2
      
      fusedPCA[, p1] <- fusedPCA[, p1]/sum(fusedPCA[, p1])
      
      fusedPCA <- fusedPCA[, -p2]
      
      numProj <- numProj - 1
      
      proj <- dataMatrix %*% fusedPCA
      
      correlation <- cor(proj, proj) - diag(numProj)
      
    }
    
  }
  
  
  numEigenvectors <- dim(fusedPCA)[2]
  
  print(paste("   Returning ", numEigenvectors, " regions"))
  
  pca_names <- array(colName, c(1, numEigenvectors))
  pca_suffix <- sprintf("%03d", seq(0, numEigenvectors - 1, 1))
  pca_names <- paste(pca_names, pca_suffix, sep = "")
  colnames(fusedPCA) <- pca_names
  
  
  return(fusedPCA)
  
} 
