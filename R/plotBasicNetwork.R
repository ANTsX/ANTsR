#' Simple plotBasicNetwork function.
#'
#' takes an object output from renderSurfaceFunction and a list of centroids
#' and plots the centroid network over the rendering object
#'
#' If \code{edgecolors} is not specified, a heat-like color palette is used. Weights
#' can be quantile transformed or clipped at a given quantile in order to improve
#' contrast.
#'
#' If weights are not specified, only the nodes are plotted.
#'
#' @param centroids input matrix of size N 3D points ( in rows ) by 3 (
#' in columns ), for N nodes.
#' @param brain input rendering object which is output of renderSurfaceFunction.
#' or a function derived from renderSurfaceFunction.
#' @param weights edge weights, a symmetric matrix of size N. Weights should be non-negative.
#' @param nodecolors a color or color vector for nodes.
#' @param edgecolors a color(map) for edges. If a color map function, weights will be transformed
#' to the range [0,1], which is compatible with functions returned by \code{colorRamp}.
#' @param backgroundColor background color.
#' @param nodetype sphere or other node type supported by RGL.
#' @param edgeContrast a vector of length 2, specifying the contrast range for edge colors.
#' Weights are normalized to the range [0,1].
#' The normalized weights can be rescaled with this parameter, eg \code{c(0.05,0.95)}
#' would stretch the contrast over the middle 90\% of normalized weight values.
#' @param quantileTransformWeights quantile transform the weights.
#' @param lwd line width for drawing edges.
#' @param minRadius minimum node radius. Ignored if the radius is specified explicitly with \code{radius}.
#' @param maxRadius maximum node radius. The node radius between \code{minRadius} and \code{maxRadius} is
#' determined from the sum of the edge weights connecting the node. Ignored if the radius is specified
#' explicitly with \code{radius}.
#' @param radius a constant radius or vector of length nrow(centroids). If not specified, node radius is
#' determined by the sum of edge weights connected to the node.
#' @param showOnlyConnectedNodes boolean, if \code{TRUE}, only nodes with non-zero edge weights are plotted.
#' 
#' @return None
#' @author Avants BB, Duda JT, Cook PA
#' @examples
#'
#' \dontrun{ 
#' # more complete example
#'   mnit<-getANTsRData("mni")
#'   mnit<-antsImageRead(mnit)
#'   mnia<-getANTsRData("mnia")
#'   mnia<-antsImageRead(mnia)
#'   mnit<-thresholdImage( mnit, 1, max(mnit) )
#'   mnit<-iMath(mnit,"FillHoles")
#'   cnt<-getCentroids( mnia, clustparam = 0 )
#'   aalcnt<-cnt[1:90,1:3]
#'   brain<-renderSurfaceFunction( surfimg =list( mnit ) , 
#'   alphasurf=0.1 ,smoothsval = 1.5 )
#'   testweights<-matrix( rep( 0, 90*90 ) ,nrow=90)
#'   testweights[31,37]<-1  # ant cingulate to hipp
#'   testweights[31,36]<-2  # ant cingulate to post cingulate
#'   testweights[11,65]<-3  # broca to angular
#'   plotBasicNetwork( centroids = aalcnt , brain , weights=testweights, edgecolors = "red" )
#'   id<-rgl::par3d('userMatrix')
#'   rid<-rotate3d( id , -pi/2, 1, 0, 0 )
#'   rid2<-rotate3d( id , pi/2, 0, 0, 1 )
#'   rid3<-rotate3d( id , -pi/2, 0, 0, 1 )
#'   rgl::par3d(userMatrix = id )
#'   dd<-make3ViewPNG(  rid, id, rid2,  paste('network1',sep='') )
#'   rgl::par3d(userMatrix = id )
#' # another example 
#' mni<-getANTsRData("mni")
#' mni<-antsImageRead(mni)
#' mnit<-thresholdImage( mni, 1, max(mni) )
#' mnit<-iMath(mnit,"FillHoles")
#' mniseg = kmeansSegmentation( mni, 3 )$segmentation
#' wmbkgd = thresholdImage( mniseg, 3, 3 ) %>% 
#' iMath( "GetLargestComponent" ) %>% 
#' iMath( "FillHoles" )
#' wmbkgd = smoothImage( iMath( wmbkgd, "MD", 1 ), 2.0 )
#' brain<-renderSurfaceFunction( surfimg =list( wmbkgd ) , 
#' alphasurf=0.8 ,smoothsval = 1.0 )
#' data( powers_areal_mni_itk )
#' coords = powers_areal_mni_itk[,1:3]
#' id<-rgl::par3d('userMatrix')
#' rid<-rotate3d( id , -pi/2, 1, 0, 0 )
#' rid2<-rotate3d( id , pi/2, 0, 0, 1 )
#' rid3<-rotate3d( id , -pi/2, 0, 0, 1 )
#' rgl::par3d(userMatrix = id )
#' handMat2 = t( matrix(  c(-0.9998656511 , 0.01626961,  0.00198165 ,   
#' 0 ,-0.0163816363, -0.99584705 ,-0.08955579   , 0, 0.0005163439, 
#' -0.08957647,  0.99597979 ,   0,  0.0000000000,  0.00000000,  
#' 0.00000000  ,  1),  ncol = 4) )
#' loccolor = as.character( powers_areal_mni_itk$Color )
#' loccolor[ loccolor == "Peach" ] = "sienna1"
#' loccolor[ loccolor == "Cyan" ] = "cyan"
#' loccolor[ loccolor == "Orange" ] = "orange"
#' loccolor[ loccolor == "Purple" ] = "darkorchid1"
#' loccolor[ loccolor == "Pink" ] = "deeppink"
#' loccolor[ loccolor == "Red" ] = "red"
#' loccolor[ loccolor == "Gray" ] = "gray74"
#' loccolor[ loccolor == "Teal" ] = "turquoise4"
#' loccolor[ loccolor == "Blue" ] = "blue"
#' loccolor[ loccolor == "Yellow" ] = "yellow"
#' loccolor[ loccolor == "Black" ] = "black"
#' loccolor[ loccolor == "Brown" ] = "brown"
#' loccolor[ loccolor == "Pale blue" ] = "steelblue1"
#' loccolor[ loccolor == "Green" ] = "green"
#' tt = plotBasicNetwork( centroids = coords, brain, 
#' nodecolors = loccolor, radius=3 )
#' dd<-make3ViewPNG(  handMat2, id, rid2, tempfile( fileext='.png' ) )
#' rgl::par3d(userMatrix = id )
#' }
#'
#' @export plotBasicNetwork
plotBasicNetwork <- function(
                             centroids,
                             brain,
                             weights = NA,
                             edgecolors = -1,
                             backgroundColor = "white",
                             nodecolors = "blue",
                             nodetype = "s",
                             edgeContrast = c(0, 1),
                             quantileTransformWeights = FALSE,
                             lwd = 2,
                             minRadius=0,
                             maxRadius=3,
                             radius = NA,
                             showOnlyConnectedNodes = TRUE ) {
    if (missing(centroids) | missing(brain)) {
        print(args(plotBasicNetwork))
        return(1)
    }
    if(!usePkg('rgl')){
        print("rgl is necessary for this function.")
        return(NULL)
    }

    edgeColorsIsFunction <- is.function(edgecolors)

    # Test if weights are all NA here. Then use this boolean in place of (is.na(weights), which
    # causes warnings in scalar contexts like if statements
    weightsNA <- all(is.na(weights))

    numNodes <- nrow(centroids)
    
    rgl::rgl.bg(color = backgroundColor)
    rgl::par3d(windowRect = c(100, 100, 600, 600))
    mesh <- .getvertices(brain[[1]])
    nSurfaceVerts <- dim(mesh$vertices)[1]
    mesh$vertices <- rbind(mesh$vertices, as.matrix(centroids))
    labelVerts <- c(1:nrow(centroids)) + nSurfaceVerts
    
    if (weightsNA) {
        if ( all(is.na( radius )) ) radius = 3
        rgl::spheres3d(mesh$vertices[labelVerts, ], color = nodecolors, type = nodetype, radius = radius)
        return(1)
    }
  
    if ( !(length(dim(weights) == 2 && dim(weights)[1] == numNodes && dim(weights)[2] == numNodes)) ) {
        stop("Weights must be a 2D symmetric matrix with one entry for each pair of nodes")
    }
    
    if ( all(is.na(radius)) ) { # node scaled by strength
        radiusw <- rep(0, nrow(centroids))
        radiusscale <- as.numeric(apply(weights, FUN = sum, MARGIN = 1, na.rm = T) +
                                  apply(weights, FUN = sum, MARGIN = 2, na.rm = T))
        radiusscale <- ( radiusscale/max(radiusscale) )
        nodeMask <- 1 * (radiusscale > 0) # unconnected nodes stay at 0 if showOnlyConnectedNodes
        radiusw <- (minRadius + (maxRadius - minRadius) * radiusscale)

        if (showOnlyConnectedNodes) {
            radiusq <- radiusw * nodeMask
        }
      
        radius <- radiusw
    }
   
    ggg <- weights
    luniqvals <- length( unique( ggg[ggg > 0] ) )
    
    binaryWeights <- FALSE
    
    if (luniqvals == 1) {
        binaryWeights <- TRUE
        # If some constant that is not binary, convert to binary so that we don't mess up
        # color mapping
        weights <- weights / max(weights)
    }
    else if (quantileTransformWeights) {
        if ( luniqvals > 250 ) ncuts = 1.0 / 250.0 else ncuts = 1.0 / ( luniqvals*0.5 )
        
        qqq = quantile(ggg[ ggg > 0 ], probs=seq(0, 1, by=ncuts), na.rm=TRUE, names=FALSE, type=7 )
        qqq = unique( qqq )
        myquartile <- cut(ggg, breaks = qqq, include.lowest=TRUE )
        myquartile <- as.numeric( myquartile )
        myquartile[ is.na( myquartile) ] = 0
        myquartile[ is.nan( myquartile) ] = 0
        weights <- matrix( myquartile, nrow=nrow(weights) )
    }
    
    rgl::spheres3d(mesh$vertices[labelVerts, ], color = nodecolors, type = nodetype, radius = radius)

    edgelocations <- c()
    edgeweights <- c()
    
    
    for (i in c(1:nrow(weights))) {
        for (j in c(1:ncol(weights))) {
            if (weights[i, j] > 0 & weights[i, j] < Inf) {
                # Draw each edge once only
                if (i < j) { 
                    edgelocations <- c(edgelocations, nSurfaceVerts + c(i, j))
                    edgeweights <- c(edgeweights, weights[i, j])
                }
            }
        }
    }

    # normalize weights to range [0-1]
    edgeweightsNorm <- edgeweights
    
    if (!binaryWeights) {
        minWeight <- min(edgeweights)
        maxWeight <- max(edgeweights)
        
        edgeweightsNorm <- (edgeweights - minWeight) / (maxWeight - minWeight)
        
        # user defined contrast stretch
        # Want to map (0.05, 0.95) to (0,1)
        # then clip values outside this range
        edgeweightsNorm <- (edgeweightsNorm - edgeContrast[1]) / (edgeContrast[2] - edgeContrast[1])
        
        edgeweightsNorm[edgeweightsNorm > 1] <- 1
        edgeweightsNorm[edgeweightsNorm < 0] <- 0
    }
    
    # Map colors unless color map or other explicit color scheme provided.
    if (!edgeColorsIsFunction && (length(edgecolors) == 1) && (edgecolors[1] == -1)) {
        # heat.colors makes high end white, which is invisible with default background
        
        colormap <- colorRampPalette(c("#650000","dark red", "red", "darkorange", "orange", "yellow", "#FAFAD0"))
        
        edgecolors <- edgeweightsNorm
        
        colorArr <- colormap(256)
        
        for (i in c(1:length(edgeweightsNorm))) {
            edgecolors[i] <- colorArr[1 + floor(edgeweightsNorm[i] * 255)]
        }
        
    }
    if (edgeColorsIsFunction) {
        # Convert function to color array
        colSeq <- seq(0,1,1/255)
        
        colorArr <- rgb(edgecolors(colSeq), maxColorValue = 255)

        edgecolors <- edgeweightsNorm
        
        for (i in c(1:length(edgeweightsNorm))) {
            edgecolors[i] <- colorArr[1 + floor(edgeweightsNorm[i] * 255)]
        }
        
    }
    
    rgl::segments3d(mesh$vertices[edgelocations, ], col = edgecolors, lwd = lwd)
    
}


.getvertices <- function(inrglmesh) {
    cter <- nrow(inrglmesh[[1]])
    vertices <- matrix(NA, nrow = 3 * cter, ncol = 3)
    inds <- c(1:cter)
    vertices[(3 * inds - 2), ] <- inrglmesh[[1]]
    vertices[(3 * inds - 1), ] <- inrglmesh[[2]]
    vertices[(3 * inds - 0), ] <- inrglmesh[[3]]
    indices <- rep(NA, nrow(vertices))
    return(list(vertices = vertices, indices = indices))
}
