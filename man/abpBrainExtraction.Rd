\name{abpBrainExtraction}
\alias{abpBrainExtraction}

\title{
An ants-based brain extraction script.
}

\description{
Brain extraction based on mapping a template image and its mask to the input image.  Should be preceded by abpN4.
}

\usage{
abpBrainExtraction( img=imgToBExtract, tem = template, temmask = mask, tempriors=c(img1,img2,...,imgN) )
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
fn<-getANTsRData('r16')
img<-antsImageRead(fn,2)
tf<-getANTsRData('r64')
tem<-antsImageRead(tf,2)
temmask<-antsImageClone( tem )
temmask[ tem  > 20 ]<-1 
temmask[ tem  <= 20 ]<-0 
bm<-abpBrainExtraction(img=img,tem=tem,temmask=temmask)
}
}
