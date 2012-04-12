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
antsMotionCorr( <comma-separated-list-of-quoted-arguments> )
see section: Arguments, Examples
}

\arguments{
\item{-d, --dimensionality 2/3}{
This option forces the image to be treated as a specified-dimensional image. If not specified, N4 tries to infer the dimensionality from the input image.
}

\item{-n, --n-images 10}{
This option sets the number of images to use to construct the template image.
}

\item{
-m, --metric 
\itemize{ 
\item CC[fixedImage, movingImage, metricWeight, radius, <samplingStrategy={Regular,Random}>, <samplingPercentage=[0,1]>]
\item MI[fixedImage, movingImage, metricWeight, numberOfBins, <samplingStrategy={Regular,Random}>, <samplingPercentage=[0,1]>]
\item Demons[fixedImage, movingImage, metricWeight, radius, <samplingStrategy={Regular,Random}>, <samplingPercentage=[0,1]>]
\item GC[fixedImage, movingImage, metricWeight, radius, <samplingStrategy={Regular,Random}>, <samplingPercentage=[0,1]>]
}
}{
Four image metrics are available--- GC : global correlation, CC: ANTS neighborhood cross correlation, MI: Mutual information, and Demons: Thirion's Demons (modified mean-squares). Note that the metricWeight is currently not used. Rather, it is a temporary place holder until multivariate metrics are available for a single stage.
}

\item{-u, --useFixedReferenceImage}{
use a fixed reference image instead of the neighor in the time series.
}

\item{-e, --useScalesEstimator}{
use the scale estimator to control optimization.
}

\item{
-t, --transform 
\itemize{ \item Affine[gradientStep]
\item Rigid[gradientStep]
\item GaussianDisplacementField[gradientStep, updateFieldSigmaInPhysicalSpace, totalFieldSigmaInPhysicalSpace]
}
}{
Several transform options are available. The gradientStep orlearningRate characterizes the gradient descent optimization and is scaled appropriately for each transform using the shift scales estimator. Subsequent parameters are transform-specific and can be determined from the usage.
}

\item{-i, --iterations MxNx0...}{
Specify the number of iterations at each level.
}

\item{-s, --smoothingSigmas MxNx0...}{
Specify the amount of smoothing at each level.
}

\item{-f, --shrinkFactors MxNx0...}{
Specify the shrink factor for the virtual domain (typically the fixed image) at each level.
}

\item{-o, --output [outputTransformPrefix, <outputWarpedImage>, <outputAverageImage>]}{
Specify the output transform prefix (output format is .nii.gz ).Optionally, one can choose to warp the moving image to the fixed space and, if the inverse transform exists, one can also output the warped fixed image.
}

\item{-a, --average-image}{
Average the input time series image.
}

\item{-h}{
Print the help menu (short version). 
<VALUES>: 0
}

\item{--help}{
Print the help menu. 
<VALUES>: 1, 0
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
antsMotionCorr( "-d" , 3 , "-a" , "<input_image.nii>" , "-o" , "<ouput_average_image.nii>" )
antsMotionCorr( "-d" , 3 , "-o" , "[input_image,input_image_moco.nii,input_image_avg.nii]" , "-m" , "MI[input_image_avg.nii,input_image.nii,1,32,50]" , "-t" , "Rigid[0.01]" , "-i" , 25 , "-u" , 1 , "-e" , 1 , "-s" , 0 , "-f" , 1 , "-n" , 25 )
}
}
