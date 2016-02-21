#' 3D surface-based rendering of volume images.
#'
#' Will use rgl to render a substrate (e.g. anatomical) and overlay image (e.g.
#' functional).
#'
#' @param surfimg Input image to use as rendering substrate.
#' @param funcimg Input list of images to use as functional overlays.
#' @param surfval intensity level that defines isosurface
#' @param basefval intensity level that defines lower threshold for functional
#' image
#' @param offsetfval intensity level that defines upper threshold for
#' functional image
#' @param smoothsval smoothing for the surface image
#' @param smoothfval smoothing for the functional image
#' @param blobrender render a blob as opposed to a surface patch
#' @param alphasurf alpha for the surface contour
#' @param alphafunc alpha value for functional blobs
#' @param outdir  output directory
#' @param outfn  output file name
#' @param mycol  name of color or colors
#' @param physical boolean
#' @return 0 -- Success\cr 1 -- Failure
#' @author Avants B, Kandel B
#' @seealso \code{\link{plotBasicNetwork}}
#' @examples
#' \dontrun{
#'        mnit<-getANTsRData("mni")
#'        mnit<-antsImageRead(mnit)
#'        mnia<-getANTsRData("mnia")
#'        mnia<-antsImageRead(mnia)
#'        mnit<-thresholdImage( mnit, 1, max(mnit) )
#'        mnia<-thresholdImage( mnia, 1, 2 )
#'        brain<-renderSurfaceFunction( surfimg =list( mnit ) ,
#'           list(mnia), alphasurf=0.1 ,smoothsval = 1.5 )
#'        }
#' @export renderSurfaceFunction
renderSurfaceFunction <- function(
  surfimg,
  funcimg,
  surfval = 0.5,
  basefval,
  offsetfval,
  smoothsval = 0,
  smoothfval = 0,
  blobrender = TRUE,
  alphasurf = 1,
  alphafunc = 1,
  outdir = "./",
  outfn = NA,
  mycol,
  physical = TRUE) {
  if (missing(surfimg)) {
    stop("Check usage:  at minimum, you need to call \n renderSurfaceFunction( list(an_ants_image) ) \n ")
  }
  havemsc3d<-usePkg("misc3d")
  if ( ! havemsc3d ) {
    print("Need misc3d for this")
    return(NA)
  }
  smoothsval <- rep(smoothsval, length.out = length(surfimg))
  for (i in 1:length(surfimg)) {
    if (smoothsval[i] > 0) {
      simg <- antsImageClone(surfimg[[i]])
      simg<-smoothImage(simg, smoothsval[i])
      surfimg[[i]] <- simg
    }
  }
  surfval <- rep(surfval, length.out = length(surfimg))
  if (length(alphasurf) != length(surfimg))
    alphasurf <- rep(alphasurf, length.out = length(surfimg))
  mylist <- list()
  if (missing(funcimg)) {
    alphafunc = 0
    temp = surfimg[[1]] * 0
    dtemp = dim( temp )
    x = round(dtemp[1]/2):round(dtemp[1]/2)+1
    y = round(dtemp[2]/2):round(dtemp[2]/2)+1
    z = round(dtemp[3]/2):round(dtemp[3]/2)+1
    temp[ x, y, z ] = 1
    funcimg = list( temp )
  }
  if (smoothfval > 0) {
    for (i in 1:length(funcimg)) {
      fimg <- antsImageClone(funcimg[[i]])
      fimg<-smoothImage( fimg, smoothfval )
      funcimg[[i]] <- fimg
    }
  }
  if (missing(mycol)) {
    mycol <- rainbow(length(funcimg))
  }
  if (length(alphafunc) != length(funcimg))
    alphafunc <- rep(alphafunc, length.out = length(funcimg))
  for (i in 1:length(surfimg)) {
    surf <- as.array(surfimg[[i]])
    brain <- misc3d::contour3d(surf, level = c(surfval[i]), alpha = alphasurf[i], draw = FALSE,
      smooth = 1, material = "metal", depth = 0.6, color = "white")
    if (physical == TRUE) {
      brain$v1 <- antsTransformIndexToPhysicalPoint(surfimg[[i]], brain$v1)
      brain$v2 <- antsTransformIndexToPhysicalPoint(surfimg[[i]], brain$v2)
      brain$v3 <- antsTransformIndexToPhysicalPoint(surfimg[[i]], brain$v3)
    }
    mylist[[i]] <- brain
  }
  for (i in 1:length(funcimg)) {
    func <- as.array(funcimg[[i]])
    vals <- abs(funcimg[[i]][funcimg[[i]] > 0])
    if (missing(basefval)) {
      # just threshold at mean > 0
      usefval <- mean(vals)
    } else usefval <- basefval
    if (missing(offsetfval))
      offsetfval <- sd(vals[vals > usefval])
    # print(paste(i, usefval, alphafunc[i]))
    blob <- misc3d::contour3d(func, level = c(usefval-0.001), alpha = alphafunc[i], draw = FALSE,
      smooth = 1, material = "metal", depth = 0.6, color = mycol[[i]])
    if (physical == TRUE) {
      blob$v1 <- antsTransformIndexToPhysicalPoint(funcimg[[i]], blob$v1)
      blob$v2 <- antsTransformIndexToPhysicalPoint(funcimg[[i]], blob$v2)
      blob$v3 <- antsTransformIndexToPhysicalPoint(funcimg[[i]], blob$v3)
    }
    mylist <- lappend(mylist, list(blob))
  }
  # s<-scene3d() s$rgl::par3d$windowRect <- c(0, 0, 500, 500) # make the window large
  # 1.5*s$rgl::par3d$windowRect s$par3d$zoom = 1.1 # larger values make the image
  # smaller
  misc3d::drawScene.rgl(mylist)  # surface render
  rgl::par3d(windowRect = c(0, 0, 500, 500))  # make the window large
  rgl::par3d(zoom = 1.1)  # larger values make the image smaller
  misc3d::drawScene.rgl(mylist)  # surface render
  if (!is.na(outfn))
    rgl::movie3d(rgl::spin3d(), duration = 15, dir = outdir, movie = outfn, clean = F)
  return(mylist)
}




# Make a function that will make each facet from data returned from
# surfaceTriangles applied to a function (probably a more elegant way to do
# this?)
.makefacet <- function(data) {
  # Code for 3D function->stl files for molding and casting stl creation functions
  # similar to week 4 files Laura Perovich Oct 2012 Load package misc3d that
  # includes surfaceTriangles function Define character constants used in the stl
  # files
  tristart1 <- "facet normal 0 0 0"
  tristart2 <- " outer loop"
  triend1 <- " endloop"
  triend2 <- "endfacet"
  startline1 <- "+"
  startline2 <- " solid LAURA"
  endline <- " endsolid LAURA"

  facetvector <- c()
  progress <- txtProgressBar(min = 0, max = nrow(data[[1]]), style = 3)
  for (i in 1:nrow(data[[1]])) {
    v1 <- paste("  vertex", as.character(data[[1]][i, 1]), as.character(data[[1]][i,
      2]), as.character(data[[1]][i, 3]), sep = " ")
    v2 <- paste("  vertex", as.character(data[[2]][i, 1]), as.character(data[[2]][i,
      2]), as.character(data[[2]][i, 3]), sep = " ")
    v3 <- paste("  vertex", as.character(data[[3]][i, 1]), as.character(data[[3]][i,
      2]), as.character(data[[3]][i, 3]), sep = " ")
    facetvector <- c(facetvector, tristart1, tristart2, v1, v2, v3, triend1,
      triend2)
    if (i%%50 == 0) {
      setTxtProgressBar(progress, i)
    }
  }
  return(facetvector)
}

# Make a function that puts the facets together with the file headers and writes
# it out
.makestl <- function(facetvector, outfile) {
  # Code for 3D function->stl files for molding and casting stl creation functions
  # similar to week 4 files Laura Perovich Oct 2012 Load package misc3d that
  # includes surfaceTriangles function
  havemsc3d<-usePkg("misc3d")
  if ( ! havemsc3d ) {
    print("Need misc3d for this")
    return(NA)
  }
  # Define character constants used in the stl files
  tristart1 <- "facet normal 0 0 0"
  tristart2 <- " outer loop"
  triend1 <- " endloop"
  triend2 <- "endfacet"
  startline1 <- "+"
  startline2 <- " solid LAURA"
  endline <- " endsolid LAURA"
  fileConn <- file(outfile)
  myout <- c(startline1, startline2, facetvector, endline)
  writeLines(myout, fileConn)
  close(fileConn)
}
############################ to use this do ############################ ############################
############################ source('R/renderSurfaceFunction.R')
############################ fn<-'/Users/stnava/Downloads/resimplerenderingexample/wmss.nii.gz'
############################ img<-antsImageRead(fn,3) brain<-renderSurfaceFunction( img )
############################ fv<-.makefacet(brain[[1]]) .makestl(fv,'/tmp/temp.stl')

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
# vtri <- surfaceTriangles(vertices[,1], vertices[,2], vertices[,3] ,
# color='red') drawScene(updateTriangles(vtri, material = 'default', smooth = 3)
# )
