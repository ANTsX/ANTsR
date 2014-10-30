\name{abpBrainExtraction}
\alias{abpBrainExtraction}

\title{
An ants-based brain extraction script.
}

\description{
Brain extraction based on mapping a template image and its mask to the input image.  Should be preceded by abpN4.
}

\usage{
abpBrainExtraction( img=imgToBExtract, tem = template,
   temmask = mask, tempriors=c(img1,img2,...,imgN) )
}

\arguments{
\item{img}{
 image to which we map a brain mask
}

\item{tem}{
Template image which has an associated label mask.
}

\item{tmask}{
Template's antsImage brain mask.
}

\item{tempriors=c(img1,img2,...,imgN)}{
Vector of spatial priors - NOT IMPLEMENTED YET
}

}

\value{
outputs a brain image and brain mask.
1 -- Failure
}

\author{
Tustison N, Avants BB
}

\examples{
\dontrun{
# short example first
fn<-getANTsRData('r16')
img<-antsImageRead(fn,2)
tf<-getANTsRData('r64')
tem<-antsImageRead(tf,2)
temmask<-antsImageClone( tem )
temmask[ tem  > 20 ]<-1
temmask[ tem  <= 20 ]<-0
bm<-abpBrainExtraction(img=img,tem=tem,temmask=temmask)

# more complete example
idim<-2
fn<-getANTsRData('r16')
img<-antsImageRead(fn,idim)
tf<-getANTsRData('r64')
atem<-antsImageRead(tf,idim)
temmask<-antsImageClone( atem )
temmask[ atem > 20 ]<-1
temmask[ atem <= 20 ]<-0
tsegs <- Atropos(d = idim, a = atem, m = "[0.2,1x1]",
  c = "[5,0]", i ="kmeans[3]", x = temmask)
acsf<-tsegs$probabilityimages[[1]]
agm<-tsegs$probabilityimages[[2]]
awm<-tsegs$probabilityimages[[3]]
abm<-temmask
kk<-abpN4(img)
kkbm<-abpBrainExtraction(img=kk,tem=atem,temmask=abm)
kcsf<-antsApplyTransforms(fixed=kk,moving=acsf,transformlist=kkbm$invtransforms)
kgm<-antsApplyTransforms(fixed=kk,moving=agm,transformlist=kkbm$invtransforms)
kwm<-antsApplyTransforms(fixed=kk,moving=awm,transformlist=kkbm$invtransforms)
plotANTsImage(kk,slices='40x120x20',functional=kwm,thresh='0.5x1')
for ( its in 1:3 ) {
  kk<-abpN4( img , weight = kwm , mask = kkbm$bmask )
  ksegs <- Atropos(d = kk@dimension, a = kk, m = "[0.1,1x1]",
    c = "[5,0]", i = list( kcsf, kgm, kwm) , x = kkbm$bmask )
  kcsf<-ksegs$probabilityimages[[1]]
  kgm<-ksegs$probabilityimages[[2]]
  kwm<-ksegs$probabilityimages[[3]]
  plotANTsImage(kk,slices='40x120x20',functional=kwm,thresh='0.5x1')
}

}
}
