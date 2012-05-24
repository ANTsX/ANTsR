\name{Atropos}
\alias{Atropos}

\title{
FMM Segmentation
}

\description{
A finite mixture modeling (FMM) segmentation approach with possibilities for specifying prior constraints. These prior constraints include the specification of a prior label image, prior probability images (one for each class), and/or an MRF prior to enforce spatial smoothing of the labels. Similar algorithms include FAST and SPM. 
}

\usage{
Atropos( <list of named arguments> )
<list of named arguments> -- an R list of name-value pairs as described in the section 'Arguments'
See sections: Arguments, Examples for details. Number and type of arguments depend on options.
}

\arguments{
\item{d -or- "image-dimensionality" = <value>}{
This option forces the image to be treated as a specified-dimensional image. If not specified, Atropos tries to infer the dimensionality from the first input image. Allowed values: 2, 3, 4.
}
\item{a -or- "intensity-image" = list( <intensityImage>, <adaptiveSmoothingWeight>)}{
One or more scalar images is specified for segmentation using the -a/--intensity-image option. For segmentation scenarios with no prior information, the first scalar image encountered on the command line is used to order labelings such that the class with the smallest intensity signature is class '1' through class 'N' which represents the voxels with the largest intensity values. The optional adaptive smoothing weight parameter is applicable only when using prior label or probability images. This scalar parameter is to be specified between [0,1] which smooths each labeled region separately and modulates the intensity measurement at each voxel in each intensity image between the original intensity and its smoothed counterpart. The smoothness parameters are governed by the -b/--bspline option. 
}
\item{b -or- bspline = list( <numberOfLevels=6>, <initialMeshResolution=1x1x...>, <splineOrder=3>)}{
If the adaptive smoothing weights are > 0, the intensity images are smoothed in calculating the likelihood values. This is to account for subtle intensity differences across the same tissue regions. 
}
\item{i -or- initialization =
\itemize{
\item{list( name = "Random", <numberOfClasses>)}
\item{list( name = "Otsu", <numberOfTissueClasses>)}
\item{list( name = "KMeans", <numberOfTissueClasses>, <clusterCenters(in ascending order and for first intensity image only)>)}
\item{list( name = "PriorProbabilityImages", <numberOfTissueClasses>, <probimagelist>, <priorWeighting>, <priorProbabilityThreshold>)}
\item{list( name = "PriorLabelImage", <numberOfTissueClasses>, <labelImage>, <priorWeighting>)}
}
}{
To initialize the FMM parameters, one of the following options must be specified. If one does not have prior label or probability images we recommend using kmeans as it is typically faster than otsu and can be used with multivariate initialization. However, since a Euclidean distance on the inter cluster distances is used, one might have to appropriately scale the additional input images. Random initialization is meant purely for intellectual curiosity. The prior weighting (specified in the range [0,1]) is used to modulate the calculation of the posterior probabilities between the likelihood*mrfprior and the likelihood*mrfprior*prior. For specifying many prior probability images for a multi-label segmentation, we offer a minimize usage option (see -m). With that option one can specify a prior probability threshold in which only those pixels exceeding that threshold are stored in memory. <probimagelist> -- object of type 'antsImageList' .
}
\item{s -or- "partial-volume-label-set" = <value>}{
<value> -- label1xlabel2xlabel3. The partial volume estimation option allows one to modelmixtures of classes within single voxels. Atropos currently allows the user to model two class mixtures per partial volume class. The user specifies a set of class labels per partial volume class requested. For example, suppose the user was performing a classic 3-tissue segmentation (csf, gm, wm) using kmeans initialization. Suppose the user also wanted to model the partial voluming effects between csf/gm and gm/wm. The user would specify it using -i kmeans[3] and -t 1x2 -t 2x3. So, for this example, there would be 3 tissue classes and 2 partial volume classes. Optionally,the user can limit partial volume handling to mrf considerations only whereby the output would only be the three tissues.
}
\item{"use-partial-volume-likelihoods" = <value>}{
<value> -- 1(0) | true/(false)
The user can specify whether or not to use the partial volume likelihoods, in which case the partial volume class is considered separate from the tissue classes. Alternatively, one can use the MRF only to handle partial volume in which case, partial volume voxels are not considered as separate classes. 
}
\item{p -or- "posterior-formulation" =
\itemize{
\item{list( name = "Socrates", <useMixtureModelProportions=1>, <initialAnnealingTemperature=1>, <annealingRate=1>, <minimumTemperature=0.1>)}
\item{list( name = "Plato", <useMixtureModelProportions=1>, <initialAnnealingTemperature=1>, <annealingRate=1>, <minimumTemperature=0.1>)}
\item{list( name = "Aristotle", <useMixtureModelProportions=1>, <initialAnnealingTemperature=1>, <annealingRate=1>, <minimumTemperature=0.1>)}
}
}{
Different posterior probability formulations are possible as are different update options. To guarantee theoretical convergence properties, a proper formulation of the well-known iterated conditional modes (ICM) uses an asynchronous update step modulated by a specified annealing temperature. If one sets the AnnealingTemperature > 1 in the posterior formulation a traditional code set for a proper ICM update will be created. Otherwise, a synchronous update step will take place at each iteration. The annealing temperature, T, converts the posteriorProbability to posteriorProbability_raisedto_(1/T) over the course of optimization. 
}
\item{x -or- "mask-image" = <maskimagefilename>}{
The image mask (which is required) defines the region which is to be labeled by the Atropos algorithm. 
}
\item{c -or- convergence = list(<numberOfIterations=5>, <convergenceThreshold=0.001>)}{
Convergence is determined by calculating the mean maximum posterior probability over the region of interest at each iteration. When this value decreases or increases less than the specified threshold from the previous iteration or the maximum number of iterations is exceeded the program terminates. 
}
\item{k -or- "likelihood-model" =
\itemize{
\item{"Gaussian"}
\item{list( name = "HistogramParzenWindows", <sigma=1.0>, <numberOfBins=32>)}
\item{list( name = "ManifoldParzenWindows", <pointSetSigma=1.0>, <evaluationKNeighborhood=50>, <CovarianceKNeighborhood=0>, <kernelSigma=0>)}
\item{list( name = "JointShapeAndOrientationProbability", <shapeSigma=1.0>, <numberOfShapeBins=64>, <orientationSigma=1.0>, <numberOfOrientationBins=32>)}
\item{"LogEuclideanGaussian"}
}
}{
Both parametric and non-parametric options exist in Atropos. The Gaussian parametric option is commonly used (e.g. SPM & FAST) where the mean and standard deviation for the Gaussian of each class is calculated at each iteration. Other groups use non-parametric approaches exemplified by option 2. We recommend using options 1 or 2 as they are fairly standard and the default parameters work adequately. 
}
\item{m -or- mrf =
\itemize{
\item{list(<smoothingFactor=0.3>, <radius=1x1x...>)}
\item{list(<mrfCoefficientImage>, <radius=1x1x...>)}
}
}{
Markov random field (MRF) theory provides a general framework for enforcing spatially contextual constraints on the segmentation solution. The default smoothing factor of 0.3 provides a moderate amount of smoothing. Increasing this number causes more smoothing whereas decreasing the number lessens the smoothing. The radius parameter specifies the mrf neighborhood. Different update schemes are possible but only the asynchronous updating has theoretical convergence properties. 
}
\item{g -or- icm = list(<useAsynchronousUpdate=1>, <maximumNumberOfICMIterations=1>, <icmCodeImage=''>)}{
Asynchronous updating requires the construction of an ICM code image which is a label image (with labels in the range [1,..,MaximumICMCode]) constructed such that no MRF neighborhood has duplicate ICM code labels. Thus, to update the voxel class labels we iterate through the code labels and, for each code label, we iterate through the image and update the voxel class label that has the corresponding ICM code label. One can print out the ICM code image by specifying an ITK-compatible image filename. 
}
\item{o -or- output = list(<classifiedImage>, <posteriorProbabilityImages>)}{
The output consists of a labeled image where each voxel in the masked region is assigned a label from 1, 2, ..., N. Optionally, one can also output the posterior probability images specified in the same format as the prior probability images. <posteriorProbabilityImages> -- object of type 'antsImageList'.
}
\item{u -or- "minimize-memory-usage" = <value>}{
<value> = (0)/1
By default, memory usage is not minimized, however, if this is needed, the various probability and distance images are calculated on the fly instead of being stored in memory at each iteration. Also, if prior probability images are used, only the non -negligible pixel values are stored in memory.
<VALUES>: 0
}
\item{w -or- "winsorize-outliers" =
\itemize{
\item{list( name = BoxPlot, <lowerPercentile=0.25>, <upperPercentile=0.75>, <whiskerLength=1.5>)}
\item{list( name = GrubbsRosner, <significanceLevel=0.05>, <winsorizingLevel=0.10>)}
}
}{
To remove the effects of outliers in calculating the weighted mean and weighted covariance, the user can opt to remove the outliers through the options specified below. 
}
\item{e -or- "use-euclidean-distance" = <value>}{
<value> = (0)/1
Given prior label or probability images, the labels are propagated throughout the masked region so that every voxel in the mask is labeled. Propagation is done by using a signed distance transform of the label. Alternatively, propagation of the labels with the fast marching filter respects the distance along the shape of the mask (e.g. the sinuous sulci and gyri of the cortex). 
<VALUES>: 0
}
\item{l -or- "label-propagation" = list( name = whichLabel, <lambda=0.0>, <boundaryProbability=1.0>)}{
The propagation of each prior label can be controlled by the lambda and boundary probability parameters. The latter parameter is the probability (in the range [0,1]) of the label on the boundary which increases linearly to a maximum value of 1.0 in the interior of the labeled region. The former parameter dictates the exponential decay of probability propagation outside the labeled region from the boundary probability, i.e. boundaryProbability*exp( -lambda * distance ). 
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
Atropos( list( d = 3 , a = avg_img_float , a = compcorr_var_img_float , m = "[0.3,1x1x1]" , o = seg_img_uint , c = "[5,0]" , i = "kmeans[3]" , x = bm_img_uint ) )
Atropos( list( d = 3 , a = avg_img_float , a = compcorr_var_img_float , m = "[0.3,1x1x1]" , o = list( seg_img_uint , prob_img_list ) , c = "[5,0]" , i = "kmeans[3]" , x = bm_img_uint ) )
Atropos( list( d = 3 , a = avg_img_float , a = compcorr_var_img_float , m = "[0.3,1x1x1]" , o = list( seg_img_uint , prob_img_list ) , c = "[5,0]" , i = list( name = "priorprobabilityimages" , 3 , prob_img_list , 0 ) , x = bm_img_uint ) )
}
}
