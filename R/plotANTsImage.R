getPckg <- function(pckg) install.packages(pckg, repos = "http://cran.r-project.org")

plotANTsImage<-function( myantsimage , functional=NA, color="red", axis=1, slices="1x1x1" , threshold="1x0", quality=NA )
{
  spec = c( 
    'verbose', 'v', 2, "integer" ," verbose output ",
    'help'   , 'h', 0, "logical" ," print the help ", 
    'myantsimage'   , 'b', 2, "character"," the reference image on which to overlay ",
    'color'   , 'c', 1, "character"," the color for the overlay ",
    'functional'   , 'f', 1, "character"," the image to use as overlay ",
    'axis'   , 'a', 1, "character"," the axis to slice (1 , 2 or  3)  ",
    'slices'   , 's', 1, "character"," the slices to overlay written as 10x20x3 where 10x20 is the range and 3 is the increment etc. ",
    'threshold'   , 't', 1, "character"," we overlay values above/below this threshold : of form LOxHI  ",
    'quality'         , 'q',1,"integer"," integer quality magnification factor 1 => large (e.g. 10) ",
    'output' , 'o', 1, "character"," the output prefix ")
# ............................................. #
  spec=matrix(spec,ncol=5,byrow=TRUE)
# get the options

  if ( missing(myantsimage) ) {
  #print a friendly message and exit with a non-zero error code
  cat("\n")
  self<-"plotANTsImage"
  cat(paste(self,"\n"))
  for ( x in 1:nrow(spec) ) {
    cat("\n")
    longopt<-paste("--",spec[x,1],sep='')
    shortopt<-paste("-",spec[x,2],sep='')
    hlist<-paste(shortopt,"|",longopt,spec[x,5],"\n \n")
                                        # print(hlist,quote=F)
    cat(format(hlist, width=40, justify = c("left")))
  }
  cat(format("Example: in 2D \n", width=40, justify = c("left")))
  ex<-paste(" plotANTsImage(myantsimage=mask,functional=mask,threshold=\"50x150\",color=\"red\",axis=1)\n \n ")
  cat(format("Example: in 3D \n", width=40, justify = c("left")))
  ex<-paste(" plotANTsImage(myantsimage=img,functional=img,threshold=\"50x150\",slices=\"10x20x3\",color=\"red\",axis=0)\n \n ")
  ex<-format(ex, width=length(ex), justify = c("left"))
  cat("\n")
  cat(ex)
  return(NULL);
  }


  imagedim<-length(dim(myantsimage))
  pckg = try(require(pixmap))
  if(!pckg) {
    cat("Installing 'pixmap' from CRAN\n")
    getPckg("pixmap")
    require("pixmap")
  }
  pckg = try(require(misc3d))
  if(!pckg) {
    cat("Installing 'misc3d' from CRAN\n")
    getPckg("misc3d")
    require("misc3d")
  }
  pckg = try(require(rgl))
  if(!pckg) {
    cat("Installing 'rgl' from CRAN\n")
    getPckg("rgl")
    require("rgl")
  }
  library(utils)
  library('misc3d') ; library('rgl') ; library('pixmap')
  read.img <- function(x,dim=2) {
    img<-antsImageRead(x,'double',dim)
    img<-as.array(img);
  }
#
# define a bunch of functions for rotating matrices
# Flip matrix (upside-down)
flip.matrix <- function(x) {
  mirror.matrix(rotate180.matrix(x))
}

# Mirror matrix (left-right)
mirror.matrix <- function(x) {
  xx <- as.data.frame(x);
  xx <- rev(xx);
  xx <- as.matrix(xx);
  xx;
}

# Rotate matrix 90 clockworks
rotate90.matrix <- function(x) {
  t(mirror.matrix(x))
}

# Rotate matrix 180 clockworks
rotate180.matrix <- function(x) { 
  xx <- rev(x);
  dim(xx) <- dim(x);
  xx;
}

# Rotate matrix 270 clockworks
rotate270.matrix <- function(x) {
  mirror.matrix(t(x))
}

############################################################################
# Color methods
############################################################################
# The inverse function to col2rgb()
rgb2col <- function(rgb) {
  hexDigit <- c(0:9, "A", "B", "C", "D", "E", "F")
  rgb <- rgb %% 256
  hi <- rgb %/% 16
#  lo <- rgb %% 16
  lo <- rgb - 16*hi  # Faster?
  x <- t(matrix(c(hi,lo), ncol=2)) + 1
  s <- matrix(hexDigit[x], nrow=6)
  s <- apply(s, MARGIN=2, FUN=paste, collapse="")
  paste("#", s, sep="")
}

############################################################################
# Draw methods
############################################################################
image180 <- function(z, ...) {
  image(rotate180.matrix(z), ...)
}

image270 <- function(z, ...) {
  image(rotate270.matrix(z), ...)
}

if ( is.na(quality)) {
  quality<-4
}
if ( is.na(slices) & imagedim > 2 ) 
{
  print(paste(" No slices set."))
  return(NULL)
}
if ( is.na(myantsimage)  ) 
{
  print(paste("image",myantsimage,"does not exist. Exiting."))
  return(NULL)
}
if ( is.na(functional) ) 
{
  print(paste("functional image file",functional,"does not exist. no overlay will be produced."))
  functional<-myantsimage
  thresh<-"1.e9x1.e9"
}
# .................................................
img<-as.array(myantsimage) # the background/template image 
labimg<-as.array(functional) # the label image 
perms<-c(1,2,3)
axis<-as.numeric(axis)
if ( axis == 1 ) { print("axis-1") ; perms<-c(2,3,1) }
if ( axis == 2 ) { print("axis-2") ; perms<-c(3,1,2) }
if ( axis == 3 ) { print("axis-3") ; perms<-c(1,2,3) }
# now label the results
if ( imagedim == 3 )   img<-aperm(img   ,c(perms),resize=T)
if ( imagedim == 3 )labimg<-aperm(labimg,c(perms),resize=T)
# labimg[]<-rank(c(labimg))
#
# check sizes
dimcheck<-F
for ( x in c(1:imagedim)) 
  if ( dim(img)[x] != dim(labimg)[x] )
    {
    print("mask and label image do not match---exiting")
    return(NULL)
    }
slicesin<-c(as.numeric(unlist(strsplit(slices,"x"))))
threshold<-c(as.numeric(unlist(strsplit(threshold,"x"))))
print(paste('threshold at ',threshold[1],' and ',threshold[2],'you chose these slices :'))
print(slicesin)
if ( slicesin[1] > dim(img)[imagedim] |  slicesin[2] > dim(img)[imagedim] )
{
  print("slices do not fit in image dimensions ---exiting")
  print(paste('slices1 ',slicesin[1],'slices2',slicesin[2],'dim-to-slice',dim(img)[imagedim]))
  return(NULL)
}
nslices<-round((slicesin[2]-slicesin[1])/slicesin[imagedim])+1
slices<-rep(NA,nslices)
ct<-1 ; curslice<-slicesin[1]
while ( ct < nslices) { slices[ct]<-curslice ; curslice<-curslice+slicesin[imagedim] ; ct<-ct+1  }
if ( is.na(slices[nslices]) ) slices[nslices]<-slicesin[2]
winrows=round(length(slices)/10+0.5)
if (winrows<1) winrows=1
wincols=10
if (length(slices)<10) wincols=length(slices)
if ( axis != 2 & imagedim > 2 ) slice<-rotate90.matrix(img[,,slices[1]])
if ( axis == 2  & imagedim > 2 ) slice<-flip.matrix(img[,,slices[1]])
if ( imagedim > 2 ) slice<-mirror.matrix(slice) else slice<-img
slicerow<-nrow(slice)
slicecol<-ncol(slice)
bigslice<-matrix(0,nrow=slicerow*winrows,ncol=(slicecol*wincols))
biglab<-matrix(0,nrow=slicerow*winrows,ncol=(slicecol*wincols))
rowsl<-0
# convert to 0 255 
nlevels<-2^8
mncl<-min(labimg)
mxcl<-max(labimg)
temp<-labimg
temp<-(temp-mncl)/(mxcl-mncl)*(nlevels-1)
labimg<-temp
for ( sl in c(0:(length(slices)-1)) ) {
  if ( sl < dim(img)[imagedim] ) {
  if ( axis != 2 & imagedim > 2)slice<-rotate90.matrix(img[,,slices[sl+1]])
  if ( axis == 2 & imagedim > 2)slice<-flip.matrix(img[,,slices[sl+1]])
  if ( axis != 2 & imagedim > 2)labslice<-rotate90.matrix(labimg[,,slices[sl+1]])
  if ( axis == 2 & imagedim > 2)labslice<-flip.matrix(labimg[,,slices[sl+1]])
  if ( imagedim > 2 ) {
    slice<-mirror.matrix(slice)
    labslice<-mirror.matrix(labslice)
  } else { 
    slice<-img
    labslice<-labimg
  }
  maskslice<-(labslice > 0)
  locsl<-(sl %% (wincols))+1
  if (locsl==1) rowsl<-rowsl+1
  xl<-((locsl-1)*slicecol+1)
  xs<-c(xl:(xl+slicecol-1))
  yl<-(rowsl-1)*slicerow+1
  ys<-c(yl:(yl+slicerow-1))
  bigslice[ys,xs]<-slice 
  biglab[ys,xs]<-labslice 
  }
}
# pdf(paste(output,'.pdf',sep=''))
# onm<-paste(output,'.jpg',sep='')
mag<-quality
# jpeg(onm,width = ncol(bigslice)*mag, height = nrow(bigslice)*mag, units = "px",quality=75,bg="black")
x <- pixmapGrey(bigslice, nrow=nrow(bigslice))
# dd<-pixmapRGB(c(bigslice,bigslice,bigslice),nrow=nrow(bigslice),ncol=ncol(bigslice),bbox=c(0,0,wincols,winrows))
# plot(dd)
plot(x)
if ( threshold[1] > threshold[2] )
  {
  return(0);
  }
overlaycolors<-sort( c((unique(c(labimg)))))
threshold[1:2]<-round( ( threshold[1:2]-mncl)/(mxcl-mncl)*(nlevels-1))
print(threshold)
overlaycolors<-c(0:nlevels)
minind<-0
mindiff<-1.e9
maxind<-0
maxdiff<-1.e9
for ( i in c(1:length(overlaycolors)) ) 
{
  diff<-abs(overlaycolors[i] - threshold[1] )
  if ( diff < mindiff )
  {   
    minind<-i
    mindiff<-diff 
  }
  diff<-abs(overlaycolors[i] - threshold[2] )
  if ( diff < maxdiff )
  {   
    maxind<-i
    maxdiff<-diff 
  }
}
if ( minind > 1) minind<-minind-1 
heatvals<-heat.colors(nlevels,alpha=0.5)  
heatvals<-rainbow(nlevels,alpha=0.5) 
colorfun<-colorRampPalette(c('gray60',color),interpolate = c("spline"), space = "Lab")
colorfun<-colorRampPalette(c('black',color),interpolate = c("spline"), space = "Lab")
heatvals<-colorfun(nlevels)
if ( threshold[1] > 1 ) heatvals[1: ( threshold[1]-1) ]<-NA
if ( threshold[2] < (nlevels-1) ) {
  upper<-c( (threshold[2]+1):nlevels )
  heatvals[upper]<-NA
}
plot(pixmapIndexed(biglab,col=heatvals),add=TRUE)
# g<-biglab ; g[]<-0 ; b<-biglab ; b[]<-0
# print('try rgb')
# dd<-pixmapRGB(c(biglab,g,b),nrow=nrow(bigslice),ncol=ncol(bigslice),bbox=c(0,0,wincols,winrows))
# plot(dd,add=TRUE)
# plot(dd)
# dev.off()
# print(paste('wrote',onm))
# warnings()
}
