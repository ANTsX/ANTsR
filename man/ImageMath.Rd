\name{ImageMath}
\alias{ImageMath}

\title{
Image Math
}

\description{
Perform math-operations on the given image
}

\usage{
ImageMath( "<ImageDimension>" , "<OutputImage>" , "<operator>" , "<Image1.ext>" [, "<Image2.ext>"] )
See section 'Arguments' for details. Number and type of arguments vary depending on 'operator' used.
}

\arguments{
\item{imageDimension 2|3}{
Number of dimensions of the input image
}

\item{outputImage}{
Result image
}

\item{operator}{
Must be one of the operators listed below.\cr
Mathematical Operations:
\itemize{
\item{m : Multiply}
\item{+ : Add}
\item{- : Subtract}
\item{/ : Divide}
\item{^ : Power}
\item{exp : Take exponent exp(imagevalue*value)}
\item{addtozero : add image-b to image-a only over points where image-a has zero values}
\item{overadd : replace image-a pixel with image-b pixel if image-b pixel is non-zero}
\item{abs : absolute value }
\item{total : Sums up values in an image or in image1*image2 (img2 is the probability mask)}
\item{Decision : Computes result=1./(1.+exp(-1.0*( pix1-0.25)/pix2))}
\item{Neg : Produce image negative}
}
Spatial Filtering:
\itemize{
\item{G : Smooth with Gaussian of sigma = s \describe{\item{Usage: }{G Image1.ext s}}}
\item{MD : Morphological Dilation with radius s \describe{\item{Usage: }{MD Image1.ext s}}}
\item{ME : Morphological Erosion with radius s \describe{\item{Usage: }{ME Image1.ext s}}}
\item{MO : Morphological Opening with radius s \describe{\item{Usage: }{MO Image1.ext s}}}
\item{MC : Morphological Closing with radius s \describe{\item{Usage: }{MC Image1.ext s}}}
\item{GD : Grayscale Dilation with radius s \describe{\item{Usage: }{GD Image1.ext s}}}
\item{GE : Grayscale Erosion with radius s \describe{\item{Usage: }{GE Image1.ext s}}}
\item{GO : Grayscale Opening with radius s \describe{\item{Usage: }{GO Image1.ext s}}}
\item{GC : Grayscale Closing with radius s \describe{\item{Usage: }{GC Image1.ext s}}}
}
Time Series Operations:
\itemize{
\item{CompCorrAuto : Outputs a csv file containing global signal vector and N comp-corr eigenvectors determined from PCA of the high-variance voxels.  Also outputs a comp-corr + global signal corrected 4D image as well as a 3D image measuring the time series variance.  Requires a label image with label 1 identifying voxels in the brain. \describe{\item{Usage: }{ CompCorrAuto 4D_TimeSeries.nii.gz LabeLimage.nii.gz  N-comp-corr-eigenvectors }}}
\item{CompCorr : Outputs a comp-corr corrected 4D image as well as a 3D image measuring the correlation of a time series voxel/region with a reference voxel/region factored out.  Requires a label image with 1=overall region of interest,  2=reference voxel, 3=region to factor out.  If there is no 3rd label, then only the global signal is factored out. \describe{\item{Usage: }{ CompCorr 4D_TimeSeries.nii.gz LabeLimage.nii.gz  Sigma-for-temporal-smoothing }}}
\item{TimeSeriesSubset : Outputs n 3D image sub-volumes extracted uniformly from the input time-series 4D image. \describe{\item{Usage: }{ TimeSeriesSubset 4D_TimeSeries.nii.gz n }}}
\item{TimeSeriesToMatrix : Converts a 4D image + mask to matrix (stored as csv file) where rows are time and columns are space. \describe{\item{Usage: }{ TimeSeriesToMatrix 4D_TimeSeries.nii.gz mask }}}
\item{ComputeTimeSeriesLeverage : Outputs a csv file that identifies the raw leverage and normalized leverage for each time point in the 4D image.  leverage, here, is the difference of the time-point image from the average of the n images.  the normalized leverage is =  average( sum_k abs(Leverage(t)-Leverage(k)) )/Leverage(t). \describe{\item{Usage: }{ ComputeTimeSeriesLeverage 4D_TimeSeries.nii.gz k_neighbors }}}
}
Tensor Operations:
\itemize{
\item{4DTensorTo3DTensor : Outputs a 3D_DT_Image with the same information. \describe{\item{Usage: }{4DTensorTo3DTensor 4D_DTImage.ext}}}
\item{ComponentTo3DTensor : Outputs a 3D_DT_Image with the same information as component images. \describe{\item{Usage: }{ComponentTo3DTensor component_image_prefix[xx,xy,xz,yy,yz,zz] extension}}}
\item{ExtractComponentFrom3DTensor : Outputs a component images. \describe{\item{Usage: }{ExtractComponentFrom3DTensor dtImage.ext which={xx,xy,xz,yy,yz,zz}}}}
\item{ExtractVectorComponent : Produces the WhichVec component of the vector \describe{\item{Usage: }{ExtractVectorComponent VecImage WhichVec}}}
\item{TensorColor : Produces RGB values identifying principal directions \describe{\item{Usage: }{TensorColor DTImage.ext}}}
\item{TensorFA : Tensor FA \describe{\item{Usage: }{TensorFA DTImage.ext}}}
\item{TensorFADenominator : Tensor FA denominator \describe{\item{Usage: }{TensorFADenominator DTImage.ext}}}
\item{TensorFANumerator : Tensor FA numerator \describe{\item{Usage: }{TensorFANumerator DTImage.ext}}}
\item{TensorIOTest : Will write the DT image back out ... tests I/O processes for consistency. \describe{\item{Usage: }{TensorIOTest DTImage.ext}}}
\item{TensorMeanDiffusion : Tensor mean diffusion \describe{\item{Usage: }{TensorMeanDiffusion DTImage.ext}}}
\item{TensorToVector : Produces vector field identifying one of the principal directions, 2 = largest eigenvalue \describe{\item{Usage: }{TensorToVector DTImage.ext WhichVec}}}
\item{TensorToVectorComponent : 0 => 2 produces component of the principal vector field (largest eigenvalue). 3 = 8 => gets values from the tensor \describe{\item{Usage: }{TensorToVectorComponent DTImage.ext WhichVec}}}
}
Unclassified Operators:
\itemize{
\item{ Byte : Convert to Byte image in [0,255]}
\item{ CompareHeadersAndImages: Tries to find and fix header errors. Outputs a repaired image with new header. Never use this if you trust your header information. \describe{\item{ Usage: }{CompareHeadersAndImages Image1 Image2}}}
\item{ ConvertImageSetToMatrix: Each row/column contains image content extracted from mask applied to images in *img.nii. ConvertImageSetToMatrix output can be an image type or csv file type. \describe{\item{Usage: }{ConvertImageSetToMatrix rowcoloption Mask.nii *images.nii}}}
\item{ RandomlySampleImageSetToCSV: N random samples are selected from each image in a list. RandomlySampleImageSetToCSV outputs a csv file type. \describe{\item{Usage: }{RandomlySampleImageSetToCSV N_samples *images.nii}}}
\item{ ConvertImageSetToEigenvectors: Each row/column contains image content extracted from mask applied to images in *img.nii. ConvertImageSetToEigenvectors output will be a csv file for each label value > 0 in the mask. \describe{\item{Usage: }{ConvertImageSetToEigenvectors N_Evecs Mask.nii *images.nii}}}
\item{ ConvertImageToFile : Writes voxel values to a file \describe{\item{Usage: }{ConvertImageToFile imagevalues.nii {Optional-ImageMask.nii}}}}
\item{ ConvertLandmarkFile : Converts landmark file between formats. See ANTS.pdf for description of formats. \describe{\item{Usage: }{ConvertLandmarkFile InFile.txt}\item{Example 1: }{ImageMath 3  outfile.vtk  ConvertLandmarkFile  infile.txt}}}
\item{ ConvertToGaussian : Convert to Gaussian \describe{\item{Usage: }{ConvertToGaussian  TValueImage  sigma-float}}}
\item{ ConvertVectorToImage : The vector contains image content extracted from a mask. Here the vector is returned to its spatial origins as image content \describe{\item{Usage: }{ConvertVectorToImage Mask.nii vector.nii}}}
\item{ CorrelationUpdate : In voxels, compute update that makes Image2 more like Image1. \describe{\item{Usage: }{CorrelationUpdate Image1.ext Image2.ext RegionRadius}}}
\item{ CountVoxelDifference : The where function from IDL \describe{\item{Usage: }{CountVoxelDifference Image1 Image2 Mask}}}
\item{ CorruptImage : Currupt image \describe{\item{Usage: }{CorruptImage Image NoiseLevel Smoothing}}}
\item{ D : DistanceTransform}
\item{  DiceAndMinDistSum : Outputs DiceAndMinDistSum and Dice Overlap to text log file + optional distance image \describe{\item{ Usage: }{DiceAndMinDistSum LabelImage1.ext LabelImage2.ext OptionalDistImage}}}
\item{  EnumerateLabelInterfaces: Enumerate label interfaces \describe{\item{Usage: }{EnumerateLabelInterfaces ImageIn ColoredImageOutname NeighborFractionToIgnore}}}
\item{ ExtractSlice : Extracts slice number from last dimension of volume (2,3,4) dimensions \describe{\item{Usage: }{ExtractSlice volume.nii.gz slicetoextract}}}
\item{ FastMarchingSegmentation: final output is the propagated label image. Optional stopping value: higher values allow more distant propagation \describe{\item{Usage: }{FastMarchingSegmentation speed/binaryimagemask.ext initiallabelimage.ext Optional-Stopping-Value}}}
\item{ FillHoles : Parameter = ratio of edge at object to edge at background; Parameter = 1 is a definite hole bounded by object only, 0.99 is close; Default of parameter > 1 will fill all holes \describe{\item{Usage: }{FillHoles Image.ext parameter}}}
\item{ FitSphere : Fit sphere \describe{\item{Usage: }{FitSphere GM-ImageIn {WM-Image} {MaxRad-Default=5}}}}
\item{ FlattenImage : Replaces values greater than percentageofMax*Max to the value percentageofMax*Max \describe{\item{Usage: }{FlattenImage Image percentageofMax}}}
\item{ GetLargestComponent : Get the largest object in an image \describe{\item{Usage: }{GetLargestComponent InputImage {MinObjectSize}}}}
\item{ Grad : Gradient magnitude with sigma s (if normalize, then output in range [0, 1]) \describe{\item{Usage: }{Grad Image.ext s normalize?}}}
\item{ HistogramMatch : Histogram matching \describe{\item{Usage: }{HistogramMatch SourceImage ReferenceImage {NumberBins-Default=255} {NumberPoints-Default=64}}}}
\item{ InvId : computes the inverse-consistency of two deformations and write the inverse consistency error image \describe{\item{Usage: }{InvId VectorFieldName VectorFieldName}}}
\item{ LabelStats : Compute volumes / masses of objects in a label image. Writes to text file \describe{\item{Usage: }{LabelStats labelimage.ext valueimage.nii}}}
\item{ Laplacian : Laplacian computed with sigma s (if normalize, then output in range [0, 1]) \describe{\item{Usage: }{Laplacian Image.ext s normalize?}}}
\item{ Lipschitz : Computes the Lipschitz norm of a vector field \describe{\item{Usage: }{Lipschitz VectorFieldName}}}
\item{ MakeImage : Make Image \describe{\item{Usage: }{MakeImage SizeX  SizeY {SizeZ};}}}
\item{ Normalize : Normalize to [0,1]. Option instead divides by average value \describe{\item{Usage: }{Normalize Image.ext opt}}}
\item{ PadImage : If Pad-Number is negative, de-Padding occurs \describe{\item{Usage: }{PadImage ImageIn Pad-Number}}}
\item{ CenterImage2inImage1 : Center image2 to image1 \describe{\item{Usage: }{ReferenceImageSpace ImageToCenter }}}
\item{ PH : Print Header}
\item{  PoissonDiffusion : Solves Poisson's equation in a designated region using non-zero sources \describe{\item{ Usage: }{PoissonDiffusion inputImage labelImage [sigma=1.0] [regionLabel=1] [numberOfIterations=500] [convergenceThreshold=1e-10]}}}
\item{ PropagateLabelsThroughMask: Final output is the propagated label image. Optional stopping value: higher values allow more distant propagation \describe{\item{Usage: }{PropagateLabelsThroughMask speed/binaryimagemask.nii.gz initiallabelimage.nii.gz Optional-Stopping-Value}}}
\item{ PValueImage : P value image \describe{\item{Usage: }{PValueImage TValueImage dof}}}
\item{ RemoveLabelInterfaces: Remove label interfaces \describe{\item{Usage: }{RemoveLabelInterfaces ImageIn}}}
\item{ ROIStatistics : computes anatomical locations, cluster size and mass of a stat image which should be in the same physical space (but not nec same resolution) as the label image. \describe{\item{Usage: }{ROIStatistics LabelNames.txt labelimage.ext valueimage.nii}}}
\item{ SetOrGetPixel : Set or get pixel; You can also pass a boolean at the end to force the physical space to be used \describe{\item{Usage: }{SetOrGetPixel ImageIn Get/Set-Value IndexX IndexY {IndexZ}}\item{Example 1: }{ImageMath 2 outimage.nii SetOrGetPixel Image Get 24 34; Gets the value at 24, 34}\item{Example 2: }{ImageMath 2 outimage.nii SetOrGetPixel Image 1.e9 24 34; This sets 1.e9 as the value at 23 34}}}
\item{ Segment : Segment an Image  with option of Priors, weight 1 => maximally local/prior-based \describe{\item{Usage: }{Segment Image1.ext N-Classes LocalityVsGlobalityWeight-In-ZeroToOneRange OptionalPriorImages}}}
\item{ stack : Will put 2 images in the same volume \describe{\item{Usage: }{Stack Image1.ext Image2.ext}}}
\item{ ThresholdAtMean : See the code \describe{\item{Usage: }{ThresholdAtMean Image percentageofMean}}}
\item{ TileImages : Tile Images \describe{\item{Usage: }{TileImages NumColumns ImageList}}}
\item{ TriPlanarView : Tri planar view \describe{\item{Usage: }{TriPlanarView  ImageIn.nii.gz PercentageToClampLowIntensity PercentageToClampHiIntensity x-slice y-slice z-slice}}}
\item{ TruncateImageIntensity: Truncate image intensity \describe{\item{Usage: }{TruncateImageIntensity InputImage.ext [lowerQuantile=0.05] [upperQuantile=0.95] [numberOfBins=65] [binary-maskImage]}}}
\item{ Where : The where function from IDL \describe{\item{Usage: }{Where Image ValueToLookFor maskImage-option tolerance}}}
}
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
ImageMath( 3 , "input_img.nii" , "output_img.nii" , 2 , 3 )
ImageMath( 3 , "input_img.nii" , "output_img.nii" , "Otsu" , 3 )
}
}
