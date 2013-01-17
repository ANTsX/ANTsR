\name{antsRegistration}
\alias{antsRegistration}

\title{
A simplified (or full) interface to antsRegistration.
}

\description{
Register a pair of images either through the full or simplified interface.  Uses file I/O to manage images / transformations.
}

\usage{
antsRegistration( fixed , moving , typeofTransform = c("Rigid","Affine","SyN"),  outprefix="./antsRegOut")
}

\arguments{
\item{fixed}{
fixed image to which we register the moving image.
}

\item{movingImage}{
moving image to be mapped to fixed space.
}

\item{typeofTransform}{
Either a one stage rigid/affine mapping or a 2-stage affine+syn mapping.  Mutual information metric by default.
}

\item{outprefix}{
output will be named with this prefix.
}

}

\value{
outputs a list containing warped images and transforms.
1 -- Failure
}

\author{
Shrinidhi KL, Avants BB
}

\examples{
\dontrun{
# will give the full form of help
antsRegistration( "-h")
# example 1 - simplified, uses antsImages for I/O
antsRegOut<-antsRegistration(fixed=img1,moving=img2,
  typeofTransform="Affine",outprefix="./test")
# example 2
  fn1<-list.files(path="../../", pattern = glob2rx("r16slice.nii.gz", trim.head=TRUE),full.names = T,recursive=T)
  fn2<-list.files(path="../../", pattern = glob2rx("r64slice.nii.gz", trim.head=TRUE),full.names = T,recursive=T)
  fi<-antsImageRead(fn1[1],2)
  mi<-antsImageRead(fn2[1],2)
  mytx<-antsRegistration(fixed=fi , moving=mi , typeofTransform = c("SyN"),  outprefix="/tmp/Z")
  mywarpedimage<-antsApplyTransforms(fixed=fi,moving=mi,transformlist=mytx$fwdtransforms)
  par(mfrow=c(1,2))
  plotANTsImage(fi) 
  plotANTsImage(mywarpedimage) 
# example 3 - full access, only uses file-based I/O 
antsRegistration( "-d", "2", "-m", 
  "mi[r16slice.nii.gz,r64slice.nii.gz,1,20,Regular,0.05]",
  "-t", "affine[1.0]", "-c", "2100x1200x1200x0", "-s", 
  "3x2x1x0", "-f", "4x3x2x1","-u", "1", "-o", 
  "[xtest,xtest.nii.gz,xtest_inv.nii.gz]" )
}
}
