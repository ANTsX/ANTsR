#' iMath
#'
#' Perform various (often mathematical) operations on the input image.
#' Additional parameters should be specific for each operation.  See the
#' the full ImageMath in ANTs, on which this function is based.
#'
#' @param img input object, usually antsImage
#' @param operation a character string e.g. "GetLargestComponent" ... the
#' special case of "GetOperations" or "GetOperationsFull" will return
#' a list of operations and brief description.
#' Some operations may not be valid (WIP), but most are.
#' @param ... additional parameters
#' @author BB Avants
#' @examples
#' fi<-antsImageRead( getANTsRData("r16") , 2 )
#' mask<-getMask( fi )
#' op1<-iMath( fi , "GD" , 1 )  # gray matter dilation by 1 voxel
#' op2<-iMath( mask , "Neg" )  # negate
#' op3<-iMath( mask , "D" )  # distance transform
#' ops<-iMath( mask , "GetOperations" )  # list all ops
#'
#' @export iMath
iMath <- function( img, operation , ... ) {
  if ( class( operation ) != 'character')
  {
  print(class(operation))
  print("2nd param should be a character string defining the operation")
  return(NA)
  }
  if ( operation == "GetOperations" | operation == "GetOperationsFull" )
  {
    ops<-c(
      "m            : Multiply ---  use vm for vector multiply",
      "+             : Add ---  use v+ for vector add",
      "-             : Subtract ---  use v- for vector subtract",
      "/             : Divide",
      "^            : Power ",
      "  max            : voxelwise max","  exp            : Take exponent exp(imagevalue*value)","  addtozero        : add image-b to image-a only over points where image-a has zero values","  overadd        : replace image-a pixel with image-b pixel if image-b pixel is non-zero","  abs            : absolute value","  total            : Sums up values in an image or in image1*image2 (img2 is the probability mask)","  mean            :  Average of values in an image or in image1*image2 (img2 is the probability mask)","  vtotal            : Sums up volumetrically weighted values in an image or in image1*image2 (img2 is the probability mask)","  Decision        : Computes result=1./(1.+exp(-1.0*( pix1-0.25)/pix2))","  Neg            : Produce image negative","  Project Image1.ext axis-a which-projection   : Project an image along axis a, which-projection=0(sum, 1=max, 2=min)","  G Image1.ext s    : Smooth with Gaussian of sigma = s","  MD Image1.ext s    : Morphological Dilation with radius s","  ME Image1.ext s    : Morphological Erosion with radius s","  MO Image1.ext s    : Morphological Opening with radius s","  MC Image1.ext s    : Morphological Closing with radius s","  GD Image1.ext s    : Grayscale Dilation with radius s","  GE Image1.ext s    : Grayscale Erosion with radius s","  GO Image1.ext s    : Grayscale Opening with radius s","  GC Image1.ext s    : Grayscale Closing with radius s","  BlobDetector Image1.ext NumberOfBlobsToExtract  Optional-Input-Image2 Blob-2-out.nii.gz N-Blobs-To-Match  :  blob detection by searching for local extrema of the Laplacian of the Gassian (LoG)"," CompCorrAuto : Outputs a csv file containing global signal vector and N comp-corr eigenvectors determined from PCA of the high-variance voxels.  Also outputs a comp-corr + global signal corrected 4D image as well as a 3D image measuring the time series variance.  Requires a label image with label 1 identifying voxels in the brain.","    Usage        : ThreeTissueConfounds 4D_TimeSeries.nii.gz LabeLimage.nii.gz  csf-label wm-label"," TimeSeriesSubset : Outputs n 3D image sub-volumes extracted uniformly from the input time-series 4D image."," TimeSeriesDisassemble : Outputs n 3D image volumes for each time-point in time-series 4D image.")

    ops2<-c(" TimeSeriesAssemble : Outputs a 4D time-series image from a list of 3D volumes."," TimeSeriesToMatrix : Converts a 4D image + mask to matrix (stored as csv file) where rows are time and columns are space ."," TimeSeriesSimpleSubtraction : Outputs a 3D mean pair-wise difference list of 3D volumes."," TimeSeriesSurroundSubtraction : Outputs a 3D mean pair-wise difference list of 3D volumes."," TimeSeriesSincSubtraction : Outputs a 3D mean pair-wise difference list of 3D volumes."," SplitAlternatingTimeSeries : Outputs 2 3D time series"," ComputeTimeSeriesLeverage : Outputs a csv file that identifies the raw leverage and normalized leverage for each time point in the 4D image.  leverage, here, is the difference of the time-point image from the average of the n images.  the normalized leverage is =  average( sum_k abs(Leverage(t)-Leverage(k)) )/Leverage(t)."," SliceTimingCorrection : Outputs a slice-timing corrected 4D time series","bspline] [sincRadius=4 / bsplineOrder=3]"," PASL : computes the PASL model of CBF"," pCASL : computes the pCASL model of CBF"," PASLQuantifyCBF : Outputs a 3D CBF image in ml/100g/min from a magnetization ratio image","  FuseNImagesIntoNDVectorField     : Create ND field from N input scalar images","  MajorityVoting : Select label with most votes from candidates","  CorrelationVoting : Select label with local correlation weights","  STAPLE : Select label using STAPLE method","  MostLikely : Select label from from maximum probabilistic segmentations","  AverageLabels : Select label using STAPLE method","  PearsonCorrelation: r-value from intesities of two images","  NeighborhoodCorrelation: local correlations","  NormalizedCorrelation: r-value from intesities of two images","  Demons:","  Mattes: mutual information","  ReflectionMatrix : Create a reflection matrix about an axis","  MakeAffineTransform : Create an itk affine transform matrix","  ClosestSimplifiedHeaderMatrix : does what it says ... image-in, image-out","  Byte            : Convert to Byte image in [0,255]","  CompareHeadersAndImages: Tries to find and fix header errors. Outputs a repaired image with new header.","  ConvertImageSetToMatrix: Each row/column contains image content extracted from mask applied to images in *img.nii","  RandomlySampleImageSetToCSV: N random samples are selected from each image in a list","  FrobeniusNormOfMatrixDifference: take the difference between two itk-transform matrices and then compute the frobenius norm","  ConvertImageSetToEigenvectors: Each row/column contains image content extracted from mask applied to images in *img.nii","  ConvertImageToFile    : Writes voxel values to a file","  CorrelationUpdate    : In voxels, compute update that makes Image2 more like Image1.","  CountVoxelDifference    : The where function from IDL","  CorruptImage        :","  D             : Danielson Distance Transform","  MaurerDistance : Maurer distance transform (much faster than Danielson)","  DiceAndMinDistSum    : Outputs DiceAndMinDistSum and Dice Overlap to text log file + optional distance image","OptionalDistImage","  EnumerateLabelInterfaces:","  ClusterThresholdVariate        :  for sparse estimation","  ExtractSlice        : Extracts slice number from last dimension of volume (2,3,4) dimensions","  FastMarchingSegmentation: final output is the propagated label image. Optional stopping value: higher values allow more distant propagation","  FillHoles        : Parameter = ratio of edge at object to edge at background;  --","  PeronaMalik       : anisotropic diffusion w/varying conductance param (0.25 in example below)","  Finite            : replace non-finite values with finite-value (default = 0)","  FitSphere        :","  FlattenImage        : Replaces values greater than %ofMax*Max to the value %ofMax*Max","  GetLargestComponent    : Get the largest object in an image","  Grad            : Gradient magnitude with sigma s (if normalize, then output in range [0, 1])","  HistogramMatch    :","  RescaleImage    :","  WindowImage    :","  NeighborhoodStats    :","  InvId            : computes the inverse-consistency of two deformations and write the inverse consistency error image","  ReplicateDisplacement            : replicate a ND displacement to a ND+1 image","  ReplicateImage            : replicate a ND image to a ND+1 image","  ShiftImageSlicesInTime            : shift image slices by one","  LabelStats        : Compute volumes / masses of objects in a label image. Writes to text file","  Laplacian        : Laplacian computed with sigma s (if normalize, then output in range [0, 1])","  Canny        : Canny edge detector","  CropBinaryImage        : returns cropped image","  Lipschitz        : Computes the Lipschitz norm of a vector field","  MakeImage        :","  MTR        : Computes the magnetization transfer ratio ( (M0-M1)/M0 ) and truncates values to [0,1]","  Normalize        : Normalize to [0,1]. Option instead divides by average value","  PadImage       : If Pad-Number is negative, de-Padding occurs","  SigmoidImage   :","  Sharpen   :","  CenterImage2inImage1        :","  PH            : Print Header","  PoissonDiffusion","  PropagateLabelsThroughMask: Final output is the propagated label image. Optional stopping value: higher values allow more distant propagation","  RemoveLabelInterfaces:","  ReplaceVoxelValue: replace voxels in the range [a,b] in the input image with c","  ROIStatistics        : computes anatomical locations, cluster size and mass of a stat image which should be in the same physical space (but not nec same resolution) as the label image.","  SetOrGetPixel    :","LocalityVsGlobalityWeight-In-ZeroToOneRange OptionalPriorImages","  stack            : Will put 2 images in the same volume","  ThresholdAtMean    : See the code","  TileImages    :","  TriPlanarView    :","  TruncateImageIntensity:","  Where            : The where function from IDL")

  ops<-c(ops,ops2)
  if ( operation == "GetOperationsFull" ) return(ops)
  trimops<-ops
  for ( i in 1:length(ops) )
    {
    trimops[i]<-unlist(strsplit(ops[i],":"))[1]
    }
  return(trimops)
  }
  if ( class(img)[[1]] == 'antsImage' )
  {
  dim<-img@dimension
  outimg<-antsImageClone(img)
  args<-list(dim,outimg,operation,img,...)
  catchout<-.Call("ImageMath",
    .int_antsProcessArguments(args), PACKAGE = "ANTsR")
  return(outimg)
  }
  if ( class(img)[[1]] != 'antsImage' )
  {
  print("1st param should be an antsImage")
  return(NA)
  }
}
