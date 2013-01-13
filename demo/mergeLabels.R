getMask = function(maskFile) {

  # Returns a boolean mask vector that can be used to convert images to / from matrices

  mask_image = antsImageRead(maskFile,  3)

  mask = mask_image > 0

  return(mask)

}

readVoxelData = function(imageListFile, mask) {

  # Imagelist is a text file containing images, where the relative path of the images are stored one per line
  
  # Mask is a boolean mask vector

  # Returns matrix of dimension (numImages, numVoxelsInMask) 

  inputDir = dirname(imageListFile)

  imageList = read.table(imageListFile, header = FALSE)

  imageList = as.character(imageList[[1]])

  numImages = length(imageList)

  numVoxels = length(which(mask > 0))  

  dataMatrix = matrix(nrow = numImages, ncol = numVoxels)

  for (i in 1:numImages) {
    image = antsImageRead(paste(inputDir, "/", imageList[i], sep = ""),  3)

    dataMatrix[i,] = as.numeric(image, mask)  
  }

  return(dataMatrix)

}

writeVoxelData = function(dataMatrix, mask, imageTemplateFile, outputRoot) {

  # Writes rows of a matrix to 3D images. First unmasks to full vector, then reshapes
  # to array with dims, then writes output

  # imageTemplateFile should be the path to an image with the correct header

  numImages = dim(dataMatrix)[1]

  numVoxelsInMask = dim(dataMatrix)[2]

  if (length(which(mask)) != numVoxelsInMask) {
    stop("Number of masked voxels do not match data")
  }


  # Trick to get header correctly
  img = new("antsImage", "float", 3)
  ImageMath(3, img, "m", imageTemplateFile, "0.0")


  numVoxelsInImage = prod(dim(img))
  
  if ( length(mask) != numVoxelsInImage ) {
    stop("Size of mask does not match image dimensions")
  }

  for (i in 1:numImages) {

    img[1:numVoxelsInImage > 0] = 0

    img[mask > 0] = dataMatrix[i,]

    antsImageWrite( img, paste(outputRoot, sprintf("%03d.nii.gz", i), sep = "") ) 

  }

}


mergeLabels = function(dataMatrix, pca, corrThresh, maxExtent, colName = "variate") {

  # dataMatrix is the voxel data of dimension (subjects, voxels), pca should be a matrix 
  # with dims (voxels, eigenvectors) 

  # corrThresh = minimum correlation for merge
  # maxExtent = maximum fused component size, in voxels

  # colname = optional name for columns of returned PCA, cols will be named colName000, colName001, etc
  
  numProj = dim(pca)[2]

  # Normalize PCA vectors, this will be done continuously through the label fusion process
  for (i in 1:numProj) {
    pca[,i] = pca[,i] / sum(pca[,i])
  }

  proj = dataMatrix %*% pca

  # Containing the fused projections. Starts out identical to the input
  fusedPCA = pca
 
  # Get correlation, minus diagonal elements 
  correlation = cor(proj, proj) - diag(numProj)

  foundRegions = TRUE
 
  while (foundRegions) {

    indices = order(correlation, decreasing = TRUE)

    mergeIndex = indices[1]
 
    foundRegions = FALSE

    counter = 1

    while(!foundRegions && correlation[mergeIndex] >= corrThresh) {

      index = arrayInd(mergeIndex, dim(correlation))
      
      p1 = index[1]
      p2 = index[2]

      mergedExtent = length(which(fusedPCA[,p1] | fusedPCA[,p2]))

      print( paste("Merged extent ", mergedExtent, " correlation ", correlation[mergeIndex], sep = "") )

      if (mergedExtent <= maxExtent) {
        foundRegions = TRUE         
      }
      else {
        counter = counter + 1
        mergeIndex = indices[counter]
      }

    }

    if (foundRegions) {    
  
      index = arrayInd(mergeIndex, dim(correlation))
      
      p1 = index[1]
      p2 = index[2]

      # Add the two components together, averaging regions that overlap

      overlap = (fusedPCA[,p1] & fusedPCA[,p2])

      fusedPCA[,p1] = fusedPCA[,p1] + fusedPCA[,p2]

      fusedPCA[overlap,p1] = fusedPCA[overlap,p1] / 2

      fusedPCA[,p1] = fusedPCA[,p1] / sum(fusedPCA[,p1])

      fusedPCA = fusedPCA[,-p2]

      numProj = numProj - 1
     
      proj = dataMatrix %*% fusedPCA

      correlation = cor(proj, proj) - diag(numProj)

    }

  }


  numEigenvectors = dim(fusedPCA)[2]

  print( paste("   Returning ", numEigenvectors, " regions") )
 
  pca_names = array(colName, c(1,numEigenvectors))
  pca_suffix = sprintf("%03d", seq(0,numEigenvectors - 1,1))
  pca_names = paste(pca_names, pca_suffix, sep = "")
  colnames(fusedPCA) = pca_names

 
  return(fusedPCA)

}


