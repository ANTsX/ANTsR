

fn<-"KKI2009-01-fMRI.nii.gz" # 4D image 
glb <- glob2rx(paste(fn, sep = ""))
fn <- list.files(path = "../../", pattern = glb, full.names = T, recursive = T)[1]
img<-antsImageRead(fn,'double',4)
#' get the average to look at it in 3D
avg <- new( "antsImage" , "double" , 3 )
antsMotionCorr( list( d = 3 , a = img , o = avg ) )
#' look at the header to determine slices to display 
ImageMath('4','na','PH',img)
plotANTsImage(myantsimage=avg,slices="12x33x3",axis=3)



myres<-filterfMRIforNetworkAnalysis(img,tr=4,0.03,0.09,cbfnetwork="BOLD",moreaccurate=FALSE,maskThresh=100000)
# check if the mask is ok
plotANTsImage(myantsimage=avg,functional=myres$mask,slices="12x33x3",axis=3,threshold="0.5x1.5")
# The mask looks fine ( does not have to be perfect ) so we proceed.



# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



library(ggplot2)
samplevox1<-round( ncol(myres$filteredTimeSeries)/2 )
samplevox2<-round( ncol(myres$filteredTimeSeries)/3 )
data<-data.frame( time = c(1:nrow( myres$filteredTimeSeries ) )*4 , v1 = myres$filteredTimeSeries[ ,samplevox1] , v2 = myres$filteredTimeSeries[ ,samplevox2]  )
p1<-( ggplot( data , aes( x = time , y = v1 ) ) + geom_line()  + ggtitle("Time series @ voxel 1") )
p2<-( ggplot( data , aes( x = time , y = v2 ) ) + geom_line()  + ggtitle("Time series @ voxel 2") )
multiplot( p1, p2, cols=1) # function stolen from the internet


