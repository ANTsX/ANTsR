# SummarizeClusters.R - print out anatomical labels of ROIs labels - array of
# labels sets (images) labelinfo - array of label names data frames clusters -
# array of ROI images pvalues - significance values ( or other ) associated with
# ROIs
SummarizeClusters <- function(labels, labelinfo, clusters, pvalues) {
  nSets <- length(labels)
  nClusters <- length(clusters)
  
  if (length(labelinfo) != 2 * nSets) {
    stop("must have labelinfo for all labels")
  }
  
  if (length(pvalues) != nClusters) {
    stop("must have a pvalues for all clusters")
  }
  
  # For each set of anatomical references
  for (i in c(1:nSets)) {
    
    lbl <- labels[[i]]
    
    ids <- labelinfo[[i * 2 - 1]]
    names <- as.character(labelinfo[[i * 2]])
    
    # For each result ROI
    for (j in c(1:nClusters)) {
      
      clust <- clusters[[j]]
      clustVox <- which(clust > 0)
      clust[clust > 0] <- 1
      centroid <- LabelImageCentroids(clust, physical = TRUE)
      xc <- centroid$centroids[1]
      yc <- centroid$centroids[2]
      zc <- centroid$centroids[3]
      cent <- paste("(", xc, " ", yc, " ", zc, ")", sep = "")
      
      clustLabels <- as.array(lbl)[clustVox]
      
      regions <- sort(unique(clustLabels))
      
      partialvalues <- rep(0, length(regions))
      partialnames <- rep("", length(regions))
      
      for (r in c(1:length(regions))) {
        
        regionSize <- length(which(clustLabels == regions[r]))
        partialvalues[r] <- regionSize/length(clustVox)
        name <- names[[which(ids == r)]]
        partialnames[r] <- name
      }
      
      mag <- sort(partialvalues, decreasing = TRUE, index.return = TRUE)
      
      regionStr <- ""
      for (m in mag$ix) {
        if (nchar(regionStr) > 0) {
          regionStr <- paste(regionStr, ": ")
        }
        
        regionStr <- paste(regionStr, as.character(partialnames[m]), " (", 
          partialvalues[m], ")", sep = "")
      }
      print(paste("Labelset ", i, ", Cluster ", j, ", ", pvalues[j], ", ", 
        regionStr, ", ", cent, sep = ""))
      
      # print( paste( ' Cluster ', j, ',', regionString ) )
    }
  }
} 
