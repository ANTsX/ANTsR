\name{sccan}
\alias{sccan}

\title{
Sparse Statistical Analysis
}

\description{
A tool for sparse statistical analysis on images : scca, pscca (with options), mscca. Can also convert an imagelist/mask pair to a binary matrix image.
}

\usage{
sccan( <list of named arguments> )
<list of named arguments> -- an R list of name-value pairs
as described in the section 'Arguments'
See sections: Arguments, Examples for details.
Number and type of arguments depend on options.
}

\arguments{
\item{o -or- output = <outputImage>}{
Output dependent on which option is called.
}
\item{p -or- "n_permutations" = <value>}{
Number of permutations to use in scca. Default: 500.
}
\item{i -or- iterations = <value>}{
Max iterations for scca optimization (min 20). Default: 20.
}
\item{n -or- "n_eigenvectors" = <value>}{
Number of permutations to use in scca. Default: 2
}
\item{r -or- robustify = <value>}{
rank-based scca. Default: 0
}
\item{PClusterThresh = <value>}{
cluster threshold on view P. Default: 1.
}
\item{QClusterThresh = <value>}{
cluster threshold on view Q. Default: 1.
}
\item{e -or- "eigen_cca" = <value>}{
Number of permutations to use in scca. Default: 0.
}
\item{"partial-scca-option" = <value>}{
Choices for pscca: "PQ", "PminusRQ", "PQminusR", "PminusRQminusR"
}
\item{"imageset-to-matrix" = list(<list.txt>, <mask.nii.gz>)}{
takes a list of image files names (one per line) and converts it to a 2D matrix image in binary or csv format depending on the filetype used to define the output.
}
\item{"timeseriesimage-to-matrix" = list(<four_d_image.nii.gz>, <three_d_mask.nii.gz>, <optional-spatial-smoothing-param-in-spacing-units-default-zero>, <optional-temporal-smoothing-param-in-time-series-units-default-zero>)}{
takes a timeseries (4D) image and converts it to a 2D matrix csv format as output.
}
\item{"vector-to-image" = list(<vector.csv>, <three_d_mask.nii.gz>, <which-row-or-col>)}{
converts the 1st column vector in a csv file back to an image --- currently needs the csv file to have > 1 columns. if the number of entries in the column does not equal the number of entries in the mask but the number of rows does equal the number of entries in the mask, then it will convert the row vector to an image.
}
\item{"imageset-to-projections" = list(<list_projections.txt>, <list_images.txt>, <bool do-average-not-real-projection>)}{
takes a list of image and projection files names (one per line) and writes them to a csv file --- basically computing X*Y (matrices).
}
\item{scca =
\itemize{
\item{list( name = "two-view", <matrix-view1.mhd>, <matrix-view2.mhd>, <mask1>, <mask2>, <FracNonZero1>, <FracNonZero2>)}
\item{list( name = "three-view", <matrix-view1.mhd>, <matrix-view2.mhd>, <matrix-view3.mhd>, <mask1>, <mask2>, <mask3>, <FracNonZero1>, <FracNonZero2>, <FracNonZero3>)}
\item{list( name = partial, <matrix-view1.mhd>, <matrix-view2.mhd>, <matrix-view3.mhd>, <mask1>, <mask2>, <mask3>, <FracNonZero1>, <FracNonZero2>, <FracNonZero3>)}
}
}{
Matrix-based scca operations for 2 and 3 views.For all these options, the FracNonZero terms set the fraction of variables to use in the estimate. E.g. if one sets 0.5 then half of the variables will have non-zero values. If the FracNonZero is (+) then the weight vectors must be positive. If they are negative, weights can be (+) or (-). partial does partial scca for 2 views while partialing out the 3rd view.
}
\item{svd =
\itemize{
\item{list( name = sparse, <matrix-view1.mhd>, <mask1>, <FracNonZero1>, <nuisance-matrix>, i = <num-of-approx>, n = <num-of-eigenvec>)}
\item{list( name = classic, <matrix-view1.mhd>, <mask1>, <FracNonZero1>, <nuisance-matrix>, i = <num-of-approx>, n = <num-of-eigenvec>)}
\item{list( name = cgspca, <matrix-view1.mhd>, <mask1>, <FracNonZero1>, <nuisance-matrix>, i = <num-of-approx>, n = <num-of-eigenvec>)}
\item{list( name = prior, ....)}
\item{list( name = network, <matrix-view1.mhd>, <mask1>, <FracNonZero1>, <guidance-matrix>)}
}
}{
a sparse svd implementation --- will report correlation of eigenvector with original data columns averaged over columns with non-zero weights. will only use view1 ... unless nuisance matrix is specified. -i controls the number of sparse approximations per eigenvector, -n controls the number of eigenvectors. total output will then be  i*n sparse eigenvectors.
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
sccan( list( "timeseriesimage-to-matrix" = list( moco_img_double , cortmask_img_double , 0.0 , 0.0  ) , o = mat ) )
sccan( list( svd = list( name = "sparse" , filt_mat , cortmask_img_double , -0.15 ) , n = 40 , i = 40 , PClusterThresh = 50 , o = list( mat1 , imglist , mat2 ) ) )
}
}
