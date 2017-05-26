## ---- echo = FALSE, message = FALSE, include = FALSE---------------------
library( knitr )
knitr::opts_chunk$set(collapse = T, comment = "#>")
library(ANTsR)
library(ggplot2)
library(grid)

if("parallel" %in% rownames(installed.packages()) == TRUE) {
  library(parallel)
}  


## ----plotColor,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5, echo=FALSE----
# How to plot a 2D color image
plotColor <- function(imgList, scale=TRUE, vectors=NULL, points=NULL, paths=NULL) {

  if (class(imgList) == "antsImage") {
    imgList = list(imgList, imgList, imgList)
  }

  direction = antsGetDirection( imgList[[1]] )

  # max in all images
  maxi = 1.0
  if ( scale )
    {
    maxi = max( unlist( lapply( imgList, function(x) { max(x) } ) ) )
    }

  rgbList = lapply( imgList, function(x) { apply(t(as.matrix(x)),2,rev) / maxi })
  rgbList = lapply( imgList, function(x) { t(as.matrix(x)) / maxi })

  col <- rgb(rgbList[[1]], rgbList[[2]], rgbList[[3]])

  d = dim(rgbList[[1]])

  x = rep(1:d[2],each=d[1])
  y = rep(1:d[1], d[2])
  pts = antsTransformIndexToPhysicalPoint( imgList[[1]], cbind(x,y) )

  dat = data.frame(x=pts[,1], y=pts[,2], col=col)
  x1 = min(pts[,1])
  x2 = max(pts[,1])
  y1 = min(pts[,2])
  y2 = max(pts[,2])

  g = ggplot(dat) + geom_raster(aes(x=x, y=y, fill=col), hjust=0, vjust=0, alpha=1) + theme(legend.position="none", aspect.ratio=1,text=element_blank(),axis.ticks=element_blank(), panel.grid=element_blank() ) + scale_fill_manual(values=as.character(levels(factor(col))) )

  g = g + coord_cartesian( xlim=c(x1,x2), ylim=c(y1,y2) )
  if ( direction[1,1] > 0 ) {
    g = g + scale_x_continuous( lim=c(x1,x2) )
    }
  else {
    g = g + scale_x_reverse( lim=c(x2,x1) )  
   }
  if ( direction[2,2] > 0 ) {
    g = g + scale_y_continuous( lim=c(y1,y2) )
    }
  else {
    g = g + scale_y_reverse( lim=c(y2,y1) )  
   }

  if ( !is.null(points) ) {
    pdat = data.frame( x=points[,1], y=points[,2], id=factor(1:dim(points)[1]) )
    g = g + geom_point( data=pdat, aes(x=x, y=y, colour=id ))
  }

  if ( !is.null(paths) ) {
    g = g + geom_path(data=paths, aes(x=x,y=y,group=id,colour=id))
    }

  if ( !is.null(vectors) ) {
    xvec = as.vector( t(as.matrix(vectors[[1]])) )
    yvec = as.vector( -t(as.matrix(vectors[[2]])) )
    vpts = antsTransformIndexToPhysicalPoint( imgList[[1]], cbind(x+0.5,y+0.5) )

    mag = sqrt(xvec*xvec + yvec*yvec)
    elim = which(mag < 0.01)
    if (length(elim) > 0 ) {
      xvec = xvec[-elim]
      yvec = yvec[-elim]
      vpts = vpts[-elim,]
      }
    vdat = data.frame(x=vpts[,1]-xvec, y=vpts[,2]-yvec, xend=vpts[,1]+xvec, yend=vpts[,2]+yvec)
    g = g + geom_segment(data=vdat, aes(x=x,y=y,xend=xend,yend=yend), colour="red", alpha=0.5)
  }

  suppressWarnings(print(g))
}

## ----basics1,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5, echo=TRUE----
tx <- createAntsrTransform( precision="float", type="AffineTransform", dimension=2)
setAntsrTransformParameters(tx, c(0,-1,1,0,0,0))
setAntsrTransformFixedParameters(tx, c(128,128))
print(tx)

point = c(80,40)
outpoint = applyAntsrTransform(tx, point)
print(outpoint)

## ----basics2,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5, echo=TRUE----
ptsMat = rbind(c(80,40), c(20,30))
t(apply( ptsMat, 1, function(x) {applyAntsrTransform(tx, x)}))

ptsList = list(c(80,40), c(20,30))
lapply( ptsList, function(x) {applyAntsrTransform(tx, x)})

## ----basics3,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5, echo=TRUE----
itx = invertAntsrTransform(tx)
applyAntsrTransform( itx, outpoint )

## ----basics4,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5, echo=TRUE----
vector = c(80,40)
applyAntsrTransform(tx, vector, dataType="vector")

## ----images1,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5, echo=TRUE----
img <- antsImageRead(getANTsRData("r16"))
invisible(plotColor(img))

moveX1 = createAntsrTransform(dimension=2, type="Euler2DTransform", translation=c(20,0) )
shift1 = applyAntsrTransform(transform=moveX1, data=img, reference=img)
plotColor(shift1)

## ----images2,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5, echo=TRUE----
moveX2 = createAntsrTransform(dimension=2, type="Euler2DTransform", translation=c(128,0) )
shift2 = applyAntsrTransform(transform=moveX2, data=img, reference=img)
plotColor(shift2)

## ----images3,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5, echo=TRUE----
refImg = antsImageClone(img)
antsSetOrigin(refImg, c(-128,0))
shift3 = applyAntsrTransform(transform=moveX2, data=img, reference=refImg)
plotColor(shift3)
print(img)
print(refImg)

## ----imagesInterp,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5, echo=TRUE----
img2 = applyAntsrTransform(tx, data=img, reference=img)
invisible(plotColor(img2))
img3 = applyAntsrTransform(tx, data=img, reference=img, interpolation="Gaussian")
invisible(plotColor(img3))
img4 = applyAntsrTransform(tx, data=img, reference=img, interpolation="NearestNeighbor")
invisible(plotColor(img4))
img5 = applyAntsrTransform(tx, data=img, reference=img, interpolation="HammingWindowedSinc")
invisible(plotColor(img5))

## ----compose,message=FALSE,warnings=FALSE, fig.width=7, fig.height=5, echo=TRUE----
txStretch = createAntsrTransform( "AffineTransform", dim=2 )
params = getAntsrTransformParameters( txStretch )
params[1] = 0.8
setAntsrTransformParameters(txStretch, params)

cos45 = cos(pi*45/180)
sin45 = sin(pi*45/180)
txRotate <- createAntsrTransform( precision="float", type="AffineTransform", dim=2 )
setAntsrTransformParameters(txRotate, c(cos45,-sin45,sin45,cos45,0,0) )
setAntsrTransformFixedParameters(txRotate, c(128,128))

rotateFirst = composeAntsrTransforms(list(txStretch, txRotate))
order1 = applyAntsrTransform(rotateFirst, img, img)
plotColor(order1)

stretchFirst = composeAntsrTransforms(list(txRotate, txStretch))
order2 = applyAntsrTransform(stretchFirst, img, img)
plotColor(order2)

