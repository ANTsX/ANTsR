#' create a label image from landmark coordinates
#'
#' create a label image from landmark coordinates
#' @param pts k x 2 or k x 3 matrix containing landmark coordinates
#' @param neighbours integer: number of closest points in the image grid to label
#' @param spacing spacing of the output image
#' @param margin margin around the landmarks (determined by the extend of the bounding box) to be part of the image
#' @param labelcolors vector of length nrow(pts) with a color for each landmark label
#' @param RAS2IJK  RAS2IJK 4x4 (3D) or 3x3 (2D) matrix with transform from RAS to IJK space (e.g. when landmarks are placed with Slicer).
#' @param refimage optional: an object of class antsImage to project the point label image into the image domain defined by refimage.
#' @return returns an antsImage
#' @author Schlager S
#' @importFrom Rvcg vcgKDtree
#' @importFrom Morpho applyTransform
#' @examples
#' load(system.file("extdata/landmarks.RData",package="ANTsR"))
#' fi <- antsImageRead(getANTsRData("r16") ,2)
#' li <- points2LabelImage(fiLM,refimage=fi,RAS2IJK=diag(c(-1,-1,1)))
#' plot(fi,list(li),alpha=0.3,add=T)
#' 
#' @export 
points2LabelImage <- function(pts, neighbours=16,spacing=rep(1,ncol(pts)),margin=0.1,labelcolors=seq_len(nrow(pts)),RAS2IJK=NULL,refimage=NULL) {
    if (!is.null(RAS2IJK))
        pts <- applyTransform(pts,RAS2IJK)
    if (nrow(pts) != length(labelcolors))
        stop("number of labelcolors and number of landmarks must correspond")
    m <- ncol(pts)
    ranges <- apply(pts,2,range)
    ranges <- apply(ranges,2,extendrange,f=margin)
    grid <- lapply(1:m,function(x) seq(from=ranges[1,x],to=ranges[2,x],by=spacing[x]))
    mygrid <- as.matrix(expand.grid(grid))
    arrdims <- sapply(grid,length)
    myarr <- array(0,dim=arrdims)
    gridlist <- lapply(1:m,function(x) x <- 1L:arrdims[x])
    indices <- as.matrix(expand.grid(gridlist))
    nhs <- vcgKDtree(mygrid,pts,k=neighbours)$index
    dists <- rep(0,nrow(mygrid))
    for(i in 1:nrow(nhs))
        myarr[indices[nhs[i,],]] <- i
    origin <- mygrid[1,]
    outimage <- as.antsImage(myarr, spacing = spacing, origin = origin,pixeltype="unsigned int")
    if (!is.null(refimage)) {
        ## create identity transform to get label image into reference image domain
        file <- paste0(tempfile(pattern = "transform"),".mat")
        trafo <- transform2mat(diag(m+1),file)
        outimage <- antsApplyTransforms(refimage,outimage,trafo,whichtoinvert = FALSE)
        outarr <- as.array(outimage)
        ## ## discretize values
        storage.mode(outarr) <- "integer"
        outarr <- as.antsImage(outarr)
        antsCopyImageInfo(outarr,outimage)
        outimage <- outarr
    }
        
    return(outimage)
}
    
