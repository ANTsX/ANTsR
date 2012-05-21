\name{antsMotionCorr}
\alias{antsMotionCorr}

\title{
Motion Correction
}

\description{
This program is a user-level registration application meant to utilize ITKv4-only classes. 
The user can specify any number of \emph{stages} where a \emph{stage} consists of -- a transform, an image metric, number of iterations, shrink factors, and smoothing sigmas for each level. Specialized for 4D time series data: fixed image is 3D, moving image should be the 4D time series. Fixed image is a reference space or time slice.
}

\usage{
antsMotionCorr( <list of named arguments> )
<list of named arguments> -- an R list of name-value pairs as described in the section 'Arguments'
see section: Arguments, Examples
}

\arguments{
\item{d -or- dimensionality = <value>}{
This option forces the image to be treated as a specified-dimensional image. If not specified, N4 tries to infer the dimensionality from the input image. Allowed values: 2, 3.
}

\item{n -or- "n-images" = <value>}{
This option sets the number of images to use to construct the template image. Default: 10.
}

\item{
m -or metric =
\itemize{
\item list( name = CC, <fixedImage>, <movingImage>, <metricWeight>, <radius>, <samplingStrategy={Regular,Random}>, <samplingPercentage=[0,1]>)
\item list( name = MI, <fixedImage>, <movingImage>, <metricWeight>, <numberOfBins>, <samplingStrategy={Regular,Random}>, <samplingPercentage=[0,1]>)
\item list( name = Demons, <fixedImage>, <movingImage>, <metricWeight>, <radius>, <samplingStrategy={Regular,Random}>, <samplingPercentage=[0,1]>)
\item list( name = GC, <fixedImage>, <movingImage>, <metricWeight>, <radius>, <samplingStrategy={Regular,Random}>, <samplingPercentage=[0,1]>)
}
}{
Four image metrics are available--- GC : global correlation, CC: ANTS neighborhood cross correlation, MI: Mutual information, and Demons: Thirion's Demons (modified mean-squares). Note that the metricWeight is currently not used. Rather, it is a temporary place holder until multivariate metrics are available for a single stage.
}

\item{u -or- useFixedReferenceImage = <value>}{
use a fixed reference image instead of the neighor in the time series.
}

\item{e -or- useScalesEstimator = <value>}{
use the scale estimator to control optimization.
}

\item{
t -or- transform =
\itemize{ 
\item list( name = Affine, <gradientStep>)
\item list( name = Rigid, <gradientStep>)
\item list( name = GaussianDisplacementField, <gradientStep>, <updateFieldSigmaInPhysicalSpace>, <totalFieldSigmaInPhysicalSpace>)
}
}{
Several transform options are available. The gradientStep orlearningRate characterizes the gradient descent optimization and is scaled appropriately for each transform using the shift scales estimator. Subsequent parameters are transform-specific and can be determined from the usage.
}

\item{i -or- iterations = <value>}{
Specify the number of iterations at each level in the form MxNx0...
}

\item{s -or- smoothingSigmas = <value>}{
Specify the amount of smoothing at each level in the form MxNx0...
}

\item{f -or- shrinkFactors = <value>}{
Specify the shrink factor for the virtual domain (typically the fixed image) at each level in the form MxNx0...
}

\item{o -or- output = list( <outputTransformPrefix>, <outputWarpedImage>, <outputAverageImage>)}{
Specify the output transform prefix (output format is .nii.gz ).Optionally, one can choose to warp the moving image to the fixed space and, if the inverse transform exists, one can also output the warped fixed image.
}

\item{a -or- "average-image" = <image>}{
Average the input time series image.
}
}

\value{
0 -- Success\cr
1 -- Failure
}

\author{
Shrinidhi KL
}

\examples{
\dontrun{
antsMotionCorr( list( d = 3 , a = img , o = avg_img ) )
antsMotionCorr( list( d = 3 , o = list( moco_params , moco_img , avg_img ) , m = list( name = "MI" , avg_img , img , 1 , 32 , 50 ) , t = "Rigid[0.01]" , i = 25 , u = 1 , e = 1 , s = 0 , f = 1 , n = 25 ) )
}
}
