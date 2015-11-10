#' Create an itk transform from landmarks (rigid, similarity and affine)
#'
#' Create an itk transform from landmarks (rigid, similarity and affine) to transform an image
#' @param lmFix fix landmarks
#' @param lmMoving moving landmarks
#' @param type of transform (can be "rigid","similarity" or "affine")
#' @param RAS2IJK 4x4 (3D) or 3x3 (2D) matrix with transform from RAS to IJK space (e.g. when landmarks are placed with Slicer).
#' @param ... additional parameters (such as landmark weights) to be passed to \code{\link{rotonto}}, in case type != affine.
#' @details
#' lmMoving are landmarks placed on the image to be transformed to the position of lmFix
#' @return
#' writes a transform to a tempfile and returns the path to that transform
#' @examples
#' fi <- antsImageRead(getANTsRData("r16") ,2)
#' mi <- antsImageRead(getANTsRData("r64") ,2)
#' load(system.file("extdata/landmarks.RData",package="ANTsR"))
#' trafo <- landmarkTransform(miLM,fiLM,RAS2IJK = diag(c(-1,-1,1)))
#' fiTransformed <- antsApplyTransforms(mi,fi,trafo)
#' plot(fiTransformed)
#' @author Schlager S
#' @importFrom RcppOctave .CallOctave o_load
#' @importFrom Morpho rotonto applyTransform computeTransform getTrafo4x4
#' @export
landmarkTransform <- function(lmFix,lmMoving,type=c("rigid","similarity","affine"),RAS2IJK=NULL,...) {
    m <- ncol(lmFix)
    file <- (tempfile(pattern = "transform"))
    file <- paste0(file,".mat")
    if (!is.null(RAS2IJK)) {
        lmFix <- applyTransform(lmFix,RAS2IJK)
        lmMoving <- applyTransform(lmMoving,RAS2IJK)
    }
    scale <- FALSE
    type = match.arg(type[1],c("rigid","affine","similarity"))
    #afftrans <- computeTransform(lmFix,lmMoving,type=type)
    if (type == "affine") {
        afftrans0 <- computeTransform(lmMoving,lmFix,type=type)
    } else {
        scale <- ifelse(type == "rigid",FALSE,TRUE)
        if (!exists("reflection"))
            reflection <- FALSE
        rot <- rotonto(lmMoving,lmFix,scale=scale,reflection=reflection,...)
        afftrans0 <- getTrafo4x4(rot)
    }
    
    transform2mat(afftrans0,file)
    return (file)
}

#' write an affine transform matrix to ITK mat format
#'
#' write an affine transform matrix to ITK mat format
#' @param affinemat a 4x4 (3D case) or 3x3 (2D case) homogenous transform matrix
#' @param file filename
#' @param RAS2IJK transform from RAS to IJK space (e.g. when landmarks are placed with Slicer).
#' @return
#' returns the character of the filename
#' @author Schlager S
#' @examples
#' require(Morpho)
#' fi <- antsImageRead(getANTsRData("r16") ,2)
#' load(system.file("extdata/landmarks.RData",package="ANTsR"))
#' ## project landmarks into IJK space 
#' fiLM2IJK <- applyTransform(fiLM,diag(c(-1,-1,1)))
#' ## now compute a rigid transform to permuted landmarks
#' rotmat <- computeTransform(fiLM2IJK,fiLM2IJK[4:1,])
#' trafo <- transform2mat(rotmat,"rotmat2D.mat")
#' ## write transfrom to disk
#' fiRotated <- antsApplyTransforms(fi,fi,trafo)
#' plot(fiRotated)
#' @export
transform2mat <- function(affinemat,file,RAS2IJK=NULL) {
    if (!is.null(RAS2IJK))
        affinemat <- RAS2IJK%*%affinemat%*%RAS2IJK
    m <- nrow(affinemat)-1
    affinemat <- affinemat[1:m,]
    AffineTransform_double_3_3 <- c(t(affinemat[1:m,1:m]))
    AffineTransform_double_3_3 <- c(AffineTransform_double_3_3,affinemat[,m+1])
    fixed <- rep(0,m)
    if (m == 2) {
       o_load(list(AffineTransform_double_2_2=matrix(AffineTransform_double_3_3,6,1),fixed=as.matrix(fixed)))
       .CallOctave("save", "-v4", file, "AffineTransform_double_2_2", "fixed") 
    } else {
        o_load(list(AffineTransform_double_3_3=matrix(AffineTransform_double_3_3,12,1),fixed=as.matrix(fixed)))
        .CallOctave("save", "-v4", file, "AffineTransform_double_3_3", "fixed") 
    }
    return(file)

}
