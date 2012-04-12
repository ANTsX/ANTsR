\name{sccan}
\alias{sccan}

\title{
Sparse Statistical Analysis
}

\description{
A tool for sparse statistical analysis on images : scca, pscca (with options), mscca. Can also convert an imagelist/mask pair to a binary matrix image.
}

\usage{
sccan( "<comma-separted-list-of-arguments>" )
See sections: Arguments, Examples for details. Number and type of arguments depend on options.
}

\arguments{
\item{-h }{
Print the help menu (short version). 
<VALUES>: 0
}
\item{--help }{
Print the help menu (long version). 
<VALUES>: 0
}
\item{-o, --output outputImage}{
Output dependent on which option is called. 
}
\item{-p, --n_permutations 500}{
Number of permutations to use in scca. 
}
\item{-i, --iterations 20}{
Max iterations for scca optimization (min 20). 
}
\item{-n, --n_eigenvectors 2}{
Number of permutations to use in scca. 
}
\item{-r, --robustify 0}{
rank-based scca 
}
\item{--PClusterThresh 1}{
cluster threshold on view P 
}
\item{--QClusterThresh 1}{
cluster threshold on view Q 
}
\item{-e, --eigen_cca 0}{
Number of permutations to use in scca. 
}
\item{--partial-scca-option PminusRQ}{
Choices for pscca: PQ, PminusRQ, PQminusR, PminusRQminusR 
}
\item{--imageset-to-matrix [list.txt,mask.nii.gz]}{
takes a list of image files names (one per line) and converts it to a 2D matrix image in binary or csv format depending on the filetype used to define the output. 
}
\item{--timeseriesimage-to-matrix [four_d_image.nii.gz, three_d_mask.nii.gz, optional-spatial-smoothing-param-in-spacing-units-default-zero, optional-temporal-smoothing-param-in-time-series-units-default-zero]}{
takes a timeseries (4D) image and converts it to a 2D matrix csv format as output. 
}
\item{--vector-to-image [vector.csv, three_d_mask.nii.gz, which-row-or-col]}{
converts the 1st column vector in a csv file back to an image --- currently needs the csv file to have > 1 columns. if the number of entries in the column does not equal the number of entries in the mask but the number of rows does equal the number of entries in the mask, then it will convert the row vector to an image. 
}
\item{--imageset-to-projections [list_projections.txt, list_images.txt, bool do-average-not-real-projection]}{
takes a list of image and projection files names (one per line) and writes them to a csv file --- basically computing X*Y (matrices). 
}
\item{--scca \itemize{\item{
two-view[matrix-view1.mhd, matrix-view2.mhd, mask1, mask2, FracNonZero1, FracNonZero2]
}
\item{
three-view[matrix-view1.mhd, matrix-view2.mhd, matrix-view3.mhd, mask1, mask2, mask3, FracNonZero1, FracNonZero2, FracNonZero3]
}
\item{
partial[matrix-view1.mhd, matrix-view2.mhd, matrix-view3.mhd, mask1, mask2, mask3, FracNonZero1, FracNonZero2, FracNonZero3]
}
}
}{
Matrix-based scca operations for 2 and 3 views.For all these options, the FracNonZero terms set the fraction of variables to use in the estimate. E.g. if one sets 0.5 then half of the variables will have non-zero values. If the FracNonZero is (+) then the weight vectors must be positive. If they are negative, weights can be (+) or (-). partial does partial scca for 2 views while partialing out the 3rd view. 
}
\item{--svd \itemize{\item{
sparse[matrix-view1.mhd,mask1,FracNonZero1,nuisance-matrix] -i <num-of-approx> -n <num-of-eigenvec>
}
\item{
classic[matrix-view1.mhd,mask1,FracNonZero1,nuisance-matrix] -i <num-of-approx> -n <num-of-eigenvec>
}
\item{
cgspca[matrix-view1.mhd,mask1,FracNonZero1,nuisance-matrix] -i <num-of-approx> -n <num-of-eigenvec>
}
\item{
prior[....]
}
\item{
network[matrix-view1.mhd,mask1,FracNonZero1,guidance-matrix]
}
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
sccan( "--timeseriesimage-to-matrix", "[moco_img.nii, cortmask_img.nii, 0.0, 0.0]", "-o" , "output.csv" )
sccan( "--svd" , "sparse[filt.csv, cortmask_img.nii, -0.15]", "-n" , 40 , "-i" , 40 , "--PClusterThresh" , 50 , "-o" , "RSF_Networks_img.nii" ) ;
}
}
