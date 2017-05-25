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

